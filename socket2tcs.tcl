# socket2tcs.tcl - TCS socket communications.
#
# $Id$
#
# May be sourced and used directly, or sourced within a thread so that
# socket communications can be done there.  TCSWriteRead is the main
# procedure to call, all others are support. - arg


proc TCSSetStatus {i} {

  # puts "TCSSetStatus: $i"
  global gsTCSSockStatus
  set gsTCSSockStatus $i
}


proc TCSWritable {hSocket} {

  fileevent $hSocket writable ""
  TCSSetStatus 2
}


# Open socket to TCS asynchronously and wait for it to be writable (connected).
# Returns 0 if fails, else the channel handle for the socket.
proc TCSOpenAsync {iTimeout} {

  global gTCSinterface gsTCSSockStatus gTCSDebug

  # Default return value.
  set hRet 0
  
  # If can't open socket asynchronously.
  if {[catch {socket -async $gTCSinterface 5750} hSocket]} {

    if {$gTCSDebug} { puts "[clock seconds] TCSOpenAsync Error: Could not open socket." }
  
  } else {
  
    # Make communications non-blocking and buffer by lines.
    fconfigure $hSocket -blocking 0 -buffering line
        
    # Wait (without blocking) for socket to become writable or timeout.
    TCSSetStatus 0
    set iWait [after $iTimeout TCSSetStatus 1]
    fileevent $hSocket writable [list TCSWritable $hSocket]
    vwait gsTCSSockStatus
    
    # Cancel events.
    after cancel $iWait
    fileevent $hSocket writable ""
    
    # If timed out.
    if {$gsTCSSockStatus != 2} {

      if {$gTCSDebug} { puts "[clock seconds] TCSOpenAsync Error: Timeout opening socket." }
      close $hSocket

    } else {
    
      # If error opening socket.
      set sStatus [fconfigure $hSocket -error]
      if {$sStatus != ""} {

        if {$gTCSDebug} { puts "[clock seconds] TCSOpenAsync Error: Error opening socket: $sStatus" }
        close $hSocket
      
      } else {
      
        # Make communications non-blocking and buffer by lines.
        fconfigure $hSocket -blocking 0 -buffering line
        
        # Return success.
        set hRet $hSocket
      }
    }
  }
  
  return $hRet
}


# This is called whenever either at least one byte of input
# data is available, or the channel was closed by the client.
proc TCSReadAsync {hSocket} {

  global gsTCSReply
  
  # puts "TCSReadAsync: Starting"

  # If can read data then append it to what we have so far.
  if {[set i [gets $hSocket s]] > 0} { append gsTCSReply $s }
  
  # If eof then socket was closed and we have all we will get so signal done.
  if {[eof $hSocket]} {

    fileevent $hSocket readable ""
    TCSSetStatus 2
  
  # Else if we've received over 100kB of data so far then signal abort.
  } elseif {[string length $gsTCSReply] > 102400} { TCSSetStatus 3
  
  # Else if we read data then append a new line.  (Didn't want this when eof.)
  } elseif {$i > 0} { append gsTCSReply "\n" }

  # puts "TCSReadAsync: Ending"
}


# Send a command to TCS NG and get a response, or timeout trying.
# Uses non-blocking socket communications.  Timeout in seconds.
# Returns list of status and returned string.
proc TCSWriteRead {sCommand {iTimeout 10}} {

  global gsTCSSockStatus gsTCSReply gTCSDebug

  set gTCSDebug 0
  set lRet 0
  
  if {$gTCSDebug} { puts "[clock seconds] TCSComm: $sCommand" }
  
  # Keep trying until succeed or timeout.
  set iStop [expr {[clock seconds] + $iTimeout}]
  set bDone 0
  while {!$bDone} {
  
    # If open TCS socket.  Use odd timeouts so we aren't in-sync with anything else.
    if {[set hSocket [TCSOpenAsync 1100]] != 0} {

      # Send the command.
      if {[catch {puts $hSocket $sCommand}]} {

        if {$gTCSDebug} { puts "[clock seconds] TCSComm Error: Could not write to socket." }
      
      } else {
      
        # Read data on channel (without blocking) until socket closes or timeout.
        set gsTCSReply ""
        TCSSetStatus 0
        set iWait [after 2000 TCSSetStatus 1]
        fileevent $hSocket readable [list TCSReadAsync $hSocket]
        vwait gsTCSSockStatus
        
        # Cancel events.
        after cancel $iWait
        fileevent $hSocket readable ""
        
        # What happened with the read?
        switch $gsTCSSockStatus {
        
          1 {
            # Timed out.
            if {$gTCSDebug} { puts "[clock seconds] TCSComm Error: Read timed out." }
          }
          
          2 {
            # Got a reply until socket closed.
            set lRet [list OK [string trim $gsTCSReply "\n"]]
            set bDone 1
          }
          
          3 {
            # Getting spammed.
            if {$gTCSDebug} { puts "[clock seconds] TCSComm Error: Aborting read since reply is over 100kB so far." }
          }
          
          default {
            if {$gTCSDebug} { puts "[clock seconds] TCSComm Error: Unknown read error $gsTCSSockStatus." }
          }
        }
        
        close $hSocket
      }
    }
    
    # If not done.
    if {!$bDone} {
    
      # If time enough to try again then wait before trying again else timeout.
      if {$iStop > [expr {[clock seconds] + 0.28}]} {
      
        TCSSetStatus 0
        after 280 { TCSSetStatus 1 }
        vwait gsTCSSockStatus
        
      } else {
      
        if {$gTCSDebug} { puts "[clock seconds] TCSComm Error: Retries timed out." }
        set bDone 1
      }
    }
  }

  if {$lRet != 0} {
    if {$gTCSDebug} { puts "[clock seconds] TCSComm: $sCommand - $lRet" }
    return $lRet
  } else {
    if {$gTCSDebug} { puts "[clock seconds] TCSComm Error: Error sending $sCommand" }
    return [list ERROR ""]
  }
}

