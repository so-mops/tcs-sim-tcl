# Module aqtelemetry.tcl. Contains procedures that directly interface with TCS
# the CSS Acquisition Module.

# Revision History for Module
# see ./doc/release.notes for details

########################################################################################
#
# Name: ToggleTelemetry.tcl
#
# Function: Turn on and off the monitoring of TCS and camera status
#
# Calling protocol: ToggleTelemetry
#
# Returns: No returns.
#          Sets global variables gtelemetrystarted, gcamstatuschannel, gtelstatuschannel
#          gtelreaderrunning, gTCSbusy, and gCamstatudisable
#
# Description: 
#
#     If telemetry is being monitored, a call turns it off. If telmetry is not being
#     monitored, a call turns it on. The button that fires this routine is
#     also updated appropriately.
#
#     The current status of the global variable gtelemetrystarted indicates the
#     status of telemetry. If telemetry is not running, we start an event loop
#     that monitors the serial port connected to TCS. This event loop is managed
#     by the TelReader proc. TelReader just watches the serial port, updating 
#     a global variable that contains the status string from TCS. If TelReader
#     fires up OK, then the button that starts telemetry is changed to one that 
#     stops it. If the camera is initialized (as it normally should be) then
#     buttons that control the creation of observing programs are enabled.
#     Finally, the routines which periodically interrogate the camera and the
#     TCS status string and stuff appropriate global variables with those values
#     are started up. 
#
#     Terminating telemetry reverses this process.
#
#     Modifications for TCS-ng are not too complicated. Obviously, to stop
#     and start the telreader is specific to the old system, and we negotiate
#     around those if we are using NG. Otherwise, most of the work for telemetry
#     in the new system is done by GetTelemetry
#
# Modification History:
#
#     01/09/03: Initial release. Ed Beshore
#     10/26/10: Changes to use TCSng. Ed Beshore
#
#
########################################################################################

proc ToggleTelemetry {} {

# NB - It may be useful to do a vwait on the slew limit variable that
# can raise an error management routine if the telescope tracks into 
# a slew limit -  TBD
    
    source globals0
    source aqglob.tcl
    
    global gcamstatuschannel gtelstatuschannel
    
    if {$gtelemetrystarted} {

	# stop the entry in the event loop to turn off refresh

	set gtelemetrystarted 0
	after cancel $gtelstatuschannel
	after cancel $gcamstatuschannel
	
        if {$guseTCSng == 0} {
            # disable the event loop monitoring old PC-TCS data stream
            fileevent $gTCSport readable ""
        }

	# change the button

	.a.telemetry config -text "Telemetry Start"

	# disable programs and pointing

	DisablePointing
	DisablePrograms
	HideButton .a.autofocus
	HideButton .a.autopoint

	# clear the status display

	set currentRA ""
	set currentDEC ""
	set currentAirmass ""
	set currentAZ ""
	set currentEL ""
	set currentLST ""
	set currentLHA ""
	set currentUTtime ""
	
    } else {
	
	# Need to start up the event loop.
	# Enable the TCS handler, start the loop,
	# and enable appropriate buttons
	
        if {!$guseTCSng} {

            set gtelreaderrunning 0
            fileevent $gTCSport readable { TelReader $gTCSport }

            # if we wait a second or two, gtelreaderrunning should be true if
            # the telemetry stream has started. If not, just return

            set dum 0
            after 2000 {set dum 1}
            vwait dum

            # the telemetry reader should be running now. Check the global it
            # sets, and if it running, set other flags, enable other actions
            # as appropriate and return

            if {!$gtelreaderrunning} {
                return
            }
        }

	set gtelemetrystarted 1
	# This was being done in legacy. - arg
	if {!$guseTCSng} { set gTCSbusy 0 }
	set gCamstatdisable 0
	
	.a.telemetry config -text "Telemetry Stop"
	
	EnablePointing
	
	# if the camera is also initialized, we can enable programs
	# and autofocus too
	
	if {$gcamerainitialized} {
	    EnablePrograms
	    ShowButton .a.autofocus AutoFocInit
	    ShowButton .a.autopoint StartAutoPoint
	}
	
	# begin the routines which periodically update the display variables
	
	GetTelemetry
	GetCamStatusEvent

    }
}


########################################################################################
#
# Name: GetTelemetry
#
# Function: Get the latest version of the TCS telemetry string, parse it, stuff 
#           appropraite globals, and reschedule its own execution.
#
# Calling protocol: GetCamstatus
#
#                   CAUTION! Only call this once as the routine reschedules its
#                   own rexecution. Multiple calls will result in mutiple instances
#                   of scheduled reexcution and ultimately, disaster and mayhem.
#                   See description for information on how to terminate rescheduling.
#
# Returns: None
#          Sets numerous global variables used to described status of the telescope 
#          and mount.
#
# Description: This routine tears apart the TCS telemetry string that is held
#              in the global grawtelmetry. (The value of grawtelemetry itself is
#              refreshed by proc TelReader.) The routine is pretty self-explanatory.
#              Since most of these values are used in the status display window, it
#              also updates the current value of the UT time from the computers clock,
#              rather than from PC-TCS. TCS status flags are also refreshed here.
#
#              TCS supports two forms of telemtry strings, a 100-character version and
#              a 150-character version. The only value present in the longer version
#              not present in the 100 character version is the value of the focus position
#              If the 150 character string is used, the focus value is captured.
#
#              Finally, TCS supplies a flag in the telemetry string that is used
#              to interpret how well it swallowed the last command sent to it. When 
#              this value changes, it is placed in the global variable gTCScmdstatus.
#              This allows routines that command TCS to clear the variable, issue
#              the command, and wait for the variable to be set.
#
#              For interpretation on the flags, refer to the PC-TCS manual. 
#
#              To terminate rescheduling, an "after cancel $gtelstatuschannel" will
#              do the trick. (This is done by ToggleTelmetry).
#
#
# Modification History:
#
#     01/09/03: Initial release. Ed Beshore
#
########################################################################################

proc GetTelemetry {} {

    source globals0
    source aqglob.tcl
    
    global gtelstatuschannel gTCStelemetrylen 

    if {$gpausetelemetry == 0} {

	if {$guseTCSng} {
	    
	    # using tcs-ng
	    # unlike the old TCS system, we actually get the data right now
	    
	    set result [CommandTCS "ALL" 10]
	    
	    set gTCSstatus [lindex $result 0]
	    
	    if {$gTCSstatus == "OK"} {
		
		set telemetry [lindex $result 1]
		
                # Convert hex string flags to integer.
		set gTCSmovestatus [scan [string range $telemetry 14 15] "%x"]
		set currentRA [string range $telemetry 17 27]
		set currentDEC [string range $telemetry 29 39]
		set currentLHA [string range $telemetry 41 49]
		set currentLST [string range $telemetry 51 58]
		set currentEL [string range $telemetry 60 63]
		set currentAZ [string range $telemetry 66 70]
		set currentAirmass [string range $telemetry 74 77]
		set currentEpoch [string range $telemetry 79 85 ]

# EJC: should be asking the TCS what is the UT time, not querying the local machine.  strip off fractional seconds
		set tcstime [CommandTCS "TIME" 10]
		set currentUTtime [ string range [ lindex [ lindex $tcstime 1 ] 3 ] 0 end-3 ]
	    }
	    
	    set result [CommandTCS "FOCUS" 10]
	    set gTCSstatus [lindex $result 0]
	    
	    if {$gTCSstatus == "OK"} {
		set gTCSFocus [lindex [lindex $result 1] 3]		
	    }

	    set result [CommandTCS "LIMIT" 10]
	    set gTCSstatus [lindex $result 0]
	    
	    # IT will have the following interpretation
	    # bit 0 = RA limit, 1 = DEC Limit, 2 = Derotator, 3 = H/W Horizon, 4 = S/W Horizon
	    #     5 = Focus up, 6 = Focus Down
	    
	    if {$gTCSstatus == "OK"} {
		
		# check bit fields on return
		
		set telemetry [lindex [lindex $result 1] 3]		
		set gTCSlimitstatus [ scan $telemetry "%x" ]

		if {$gTCSlimitstatus == 0} {
		    
		    set gTCSDisabled 0
		    set gTCSRAlimit 0
		    set gTCSDEClimit 0
		    set gTCSHorzlimit 0
		    set gTCSSoftlimit 0
		    set gTCSDRlimit 0
		    set gTCSFocUplimit 0
		    set gTCSFocDnlimit 0

		} else {
		    
            # EJC: need to initialize limits that are not active!		    
		    set gTCSRAlimit 0
		    set gTCSDEClimit 0
		    set gTCSHorzlimit 0
		    set gTCSSoftlimit 0
		    set gTCSDRlimit 0
		    set gTCSFocUplimit 0
		    set gTCSFocDnlimit 0

		    # tease out bit fields to see which one(s) is (are) set
		 
		    if {$gTCSlimitstatus >= 64} {
			set gTCSFocUplimit 1
			incr gTCSlimitstatus -64
		    }
		    
		    if {$gTCSlimitstatus >= 32} {
			set gTCSFocDnlimit 1
			incr gTCSlimitstatus -32
		    }
		    
		    if {$gTCSlimitstatus >= 16} {
			set gTCSSoftlimit 1
			incr gTCSlimitstatus -16
		    }
		    
		    if {$gTCSlimitstatus >= 8} {
			set gTCSHorzlimit 1
			incr gTCSlimitstatus -8
		    }
		    
		    if {$gTCSlimitstatus >= 4} {
			set gTCSDRlimit 1
			incr gTCSlimitstatus -4
		    }
		    
		    if {$gTCSlimitstatus >= 2} {
			set gTCSDEClimit 1
			incr gTCSlimitstatus -2
		    }
		    
		    if {$gTCSlimitstatus != 0} {
			set gTCSRAlimit 1
		    }
		    
		    # one or more limits engaged, disable use of TCS until cleared
		    
		    set gTCSDisabled 1
# EJC: also, just because we're in some limit doesn't mean the telescope is necessarily disabled -
# i.e. focus limit shouldn't disable telescope, and limit override can allow enabling of telescope
# even if limits are active.  The TCS should be telling us if it's disabled/enabled...we should
# not be assuming it...

# this is important because the GUI relies on gTCSDisabled to allow acces to certain functions,
# i.e. pretty much everything
		    
		}
		
	    }
	    
	} else {
	    
	    # first get and parse the telemetry from TCS. This is found
	    # in the global variable grawtelemetry
	    
	    set RA_hour_string [string range $grawtelemetry 3 4]
	    set RA_min_string  [string range $grawtelemetry 5 6]
	    set RA_sec_string  [string range $grawtelemetry 7 11]
	    
	    set currentRA [concat $RA_hour_string:$RA_min_string:$RA_sec_string]
	    
	    set DEC_sign       [string range $grawtelemetry 13 13]
	    set DEC_deg_string [string range $grawtelemetry 14 15]
	    set DEC_min_string [string range $grawtelemetry 16 17]
	    set DEC_sec_string [string range $grawtelemetry 18 21]
	    
	    set currentDEC [concat $DEC_sign$DEC_deg_string:$DEC_min_string:$DEC_sec_string]
	    
	    set currentAirmass [string range $grawtelemetry 56 60]
	    set currentAZ [string range $grawtelemetry 49 54]
	    set currentEL [string range $grawtelemetry 43 47]
	    set currentLST [string range $grawtelemetry 34 41]
	    set currentLHA [string range $grawtelemetry 24 32]
	    
	    set currentUTtime [exec date -u +%T]
	    
	    # get mount status information
	    
	    set gTCSmovestatus [string index $grawtelemetry 0]
	    set gTCSRAlimit    [expr {[string index $grawtelemetry 71] eq " " ? 0 : 1}]
	    set gTCSDEClimit   [expr {[string index $grawtelemetry 72] eq " " ? 0 : 1}]
	    set gTCSHorzlimit  [expr {[string index $grawtelemetry 73] eq " " ? 0 : 1}]
	    set gTCSDisabled   [expr {[string index $grawtelemetry 74] eq " " ? 0 : 1}]
	    
	    # save the last command code from TCS before getting the next. 
	    # recall that TCS toggles between "e" and "E" so you can tell if
	    # the most recent command was accepted OK
	    
	    set gTCSlastcmdcode $gTCScmdcode
	    set gTCScmdcode [string index $grawtelemetry $gTCSStat]
	    
	    # content for 150 character output from TCS. Focus position is
	    # included in the 150 character version of TCS. If we are using
	    # it, capture focus info for FITS header
	    
	    if {$gTCStelemetrylen == 148} {
		set gTCSFocus [string range $grawtelemetry 96 101]
	    } else {
		set gTCSFocus 0
	    }
	    
	    if { $gTCScmdcode != $gTCSlastcmdcode } {
		set gTCScmdstatus [string toupper $gTCScmdcode]
	    }
	}
    } else {
	puts "Telemetry paused"
    }
	
    # and reschedule this routine to run again in 1 second
    
    if {$gtelemetrystarted == 1} {
	set gtelstatuschannel [after 1000 GetTelemetry]
    }
}

########################################################################################
#
# Name: ConvertRAtoTelemRA, ConvertDECtoTelemDEC
#
# Function: Convert readable RA and DEC coordinates to formats acceptable to oldTCS
#
# Calling protocol: ConvertRAtoTelemRA RAstring
#                       where RAstring is of the form: HH:MM:SS.S
#                   ConvertDECtoTelemDEC Decstring
#                       where DECstring is of the form: sDD:MM:SS.S
#                             (s is + or - and is required)
#
# Returns: ConvertRAtoTelemRA returns a string of the form HHMMSS.S
#          ConvertDECtoTelemDEC returns a string of the form sDDMMSS.S
#
# Description:
#          TCS accepts coordinates in the form of the above mentioned return
#          values. These routines simply take the more common colon-seperated verions
#          suitable for formatted output and generated the form needed by TCS.
#          (Actually, the seperators in the input string can be anything at all.)
#          These routines just pick out the needed substrings, ignoring the characters
#          in the seperator position. 
#
# Modification History:
#
#     01/09/03: Initial release. Ed Beshore
#
########################################################################################

proc ConvertRAtoTelemRA { RA } {
    
    set RA_hour_string [string range $RA 0 1]
    set RA_min_string  [string range $RA 3 4]
    set RA_sec_string  [string range $RA 6 9]

    if {[string range $RA_sec_string 0 1] == "60"} {
        set RA_sec_string "59.9"
    }

    set telemRA [concat $RA_hour_string$RA_min_string$RA_sec_string]

    return $telemRA
}

proc ConvertDECtoTelemDEC { Dec } {

    set DEC_sign        [string range $Dec 0 0]
    set DEC_deg_string  [string range $Dec 1 2]
    set DEC_min_string  [string range $Dec 4 5]
    set DEC_sec_string  [string range $Dec 7 10]

    if {[string range $DEC_sec_string 0 1] == "60"} {
        set DEC_sec_string "59.9"
    }

    set telemDEC [concat $DEC_sign$DEC_deg_string$DEC_min_string$DEC_sec_string]

    return $telemDEC
}


########################################################################################
#
# Name: TelReader
#
# Function: An event routine driven by the serial port. When a full line of
#           telemetry data is available off of the port, it is placed in 
#           the global variable grawtelemetry.
#
# Calling protocol: TelReader channel
#
#                   where channel is the return value from a call to open a
#                         serial port as in
#                            set gTCSport [open /dev/ttyS$gSerialportNum w+]
#
#                   This routine is not to be called directly. It should be
#                   referenced in a call to the fileevent command. This is done
#                   in the ToggleTelemetry procedure.
#
# Returns: None. Sets global variables grawtelemetry and gtelreaderrunning.
#
# Description:
#
#      This routine is called when the serial port receives a character in its
#      buffer. If the total length of characters received is equal to the expected
#      length of the telemetry string, it is transferred to the global variable
#      grawtelemtry.
#
# Modification History:
#
#     01/09/03: Initial release. Ed Beshore
#
########################################################################################

proc TelReader { channel } {
    
    source globals0
    source aqglob.tcl
    
    global gTCStelemetrylen
    
    set result [gets $channel tempstr]
    
    if {$result == $gTCStelemetrylen} {
	set grawtelemetry $tempstr
	set gtelreaderrunning 1
    }
}


########################################################################################
#
# Name: WaitTCS
#
# Function: Waits for the commandstatus to change, reports on result. Allows
#           caller to specifiy a timeout duration before causing an error.
#
# Calling protocol: WaitTCS timeoutval
#
#                   where timeoutval is the time to wait (in seconds) before 
#                      declaring a timeout error.
#
# Returns: "OK" if command was accepted by TCS
#          "R" for a bad request
#          "D" for bad data
#          "U" for unrecognized command
#          "T" for timeout
#
# Description: This routine assumes that a command has been issued prior to 
#              calling this routine using a proc like CommandTCS.
#
# Modification History:
#
#     01/09/03: Initial release. Ed Beshore
#
########################################################################################

proc WaitTCS { timeout } {
    
    global gTCScmdstatus verbose
    
    # set an event to trigger in timeout seconds which sets the command
    # status to timeout
    
    set cancelid [after [expr {1000 * $timeout}] { set gTCScmdstatus "T" }]
    
    # wait for command status to change
    
    vwait gTCScmdstatus
    
    # kill the previously set event if it hasn't triggerd
    
    after cancel $cancelid
    
    # examing the command status and set the appropriate return value
    
    if { $gTCScmdstatus == "E" } {
	return "OK"
    } elseif {$gTCScmdstatus == "1"} {
	return "R"
    } elseif {$gTCScmdstatus == "2"} {
	return "D"
    } elseif {$gTCScmdstatus == "3"} {
	return "B"
    } else {
	return "T"
    }
}

########################################################################################
#
# Name: CommandTCS
#
# Function: Send a command to TCS and wait for confirmation back
#
# Calling protocol: CommandTCS command timeoutval
#
#                   where: command is the appropriate TCS command and argument
#                          timeoutval is a time in seconds to automatically
#                            assume a timeout if TCS hasn't responded.
#
#                   
# Returns: See returns from WaitTCS. Returns from this routine are returned
#          by CommandTCS unadulerated.
#
#          CommandTCS sets the global variable gTCSbusy to prevent other calls
#          from overlapping. If gTCSbusy is set upon entry, CommandTCS will wait
#          forever for it to clear. (Hey, it might happen...)
#
# Description: Refer to the TCS manual discussion on "Remote Mnemonics" for
#              proper syntax of commands. Command must include properly formatted
#              argument. E.g., to change the input epoch for the NEXTRA and NEXTDEC
#              commands to 2000, with 3 seconds for a timeout one would call 
#              CommandTCS as follows:
#
#                   CommandTCS "EPOCH 2000.00" 3
#
#              CommandTCS calls WaitTCS.
#
#              Modifications made to support new TCSng are extensive. The command
#              syntax is slightly changed. We now expect a command followed by a
#              variable number of arguments. The last argument in the variable list
#              must be a timeout values (in seconds). We begin by setting TCSbusy
#              and then query to see which TCS we are using. In the new TCS, it is
#              always: 1) open the socket, 2) send the command, 3) wait for a result
#              and 4) close the socket. We log commands sent and results received
#              into the global arrays. gTCSnglog(COMMAND,CommandID) has the command
#              string, gTCSnglog(OPEN,CommandID) has the socket from the open call
#              gTCSnglog(SEND,CommandID) is the reply back from the send command and
#              gTCSnglog(RECEIVE,CommandID) is the reply back.
#
#              The return back from the TCSng-savvy call is more than just an OK.
#              On a normal return, it is an OK followed by the return from the
#              call. This allows commands like REQUEST ALL to get the results of the
#              call directly. Other possible returns are WAITING, TIMEOUT, ERROR, and OK
#
# Modification History:
#
#     01/09/03: Initial release. Ed Beshore
#     10/26/10: TCS-ng changes Ed Beshore
#
########################################################################################

proc CommandTCS { command args } {

  source globals0
  source aqglob.tcl

  global verbose

  # If using legacy TCS use old code.  May implement lock for legacy later.  - arg
  if {!$guseTCSng} {

    # wait until TCSbusy clears and hope to God it does 
    
    if {$gTCSbusy} {
	vwait gTCSbusy
    }
    
    # set busy and clear command status flag
    
    set gTCSbusy 1 
    
    # Convert new argument format to old: CommandTCS command timeout
    set timeout [lindex $args end]
    foreach arg [lrange $args 0 end-1] {
      append command " " $arg
    }
    
    # send the command and immediately clear status

    puts  $gTCSport $command
    flush $gTCSport
    set gTCScmdstatus ""
    
    # wait for completion or a timeout
    
    set result [WaitTCS $timeout]
    
    # if an error encountered while waiting for TCS , attempt a repeat
    
    if {$result != "OK"} {
	set reply [MessBox "Error" "Couldn't send TCS command: $command.\nResult code was $result. Retry?" yesno]
	if {$reply == "YES"} {
	    set gTCSbusy 0
	    set result [CommandTCS $command $timeout]
	} else {
	    set result "FAIL"
	}
    }
    
    # one way or another, we're done - clear the flag
    
    set gTCSbusy 0
    return $result

  # Else using TCS NG. - arg
  } else {

    # set busy and clear command status flag
    
    set gTCSbusy 1
    # set gpausetelemetry 1
    
    if {$guseTCSng} {
	
        # if we are using the new TCS, we get results back depending on the
        # command, and we have to deal with these and the command formats on
        # a case by case basis
	
        # Assemble the TCS command packet string

        # get a unique command id (number from 1 to 255)
        set TCSngCommandID [GetnextTCSngCommandID]

        # form the command preamble and command body
        set TCSngCommand [concat $gTCSngtelid "TCS" $TCSngCommandID [MakeTCSngCommand $command $args]]
        # add a trailing blank, which NG likes
        set TCSngCommand "$TCSngCommand "

        # and log it
        set gTCSnglog(COMMAND,$TCSngCommandID) $TCSngCommand
        #puts "Command formed. $gTCSnglog(COMMAND,$TCSngCommandID)"

        # Send command to TCS and get response.
        set ngresult [TCSComm $TCSngCommand]
        set rcvstatus [lindex $ngresult 0]

        if {$rcvstatus == "OK"} {
        
          # check to see if TCS thought there was a problem by looking in 
          # the returned line. If the response is ERROR, then TCSng had heartburn
          set response [lindex $ngresult 1]
          if {[lindex $response 4] == "ERROR"} {
            puts "CommandTCS: Error from TCS - $response"
            set rcvstatus "ERROR"
            set ngresult [list $rcvstatus $response]
          }
        }
    }

    if {$rcvstatus != "OK"} {
      set reply [MessBox "Error" "Couldn't send TCS command: $command.  Result code was:\n\n$ngresult\n\nRetry?" yesno]
      if {$reply == "YES"} {
        set gTCSbusy 0
        set ngresult [CommandTCS $command $args]
      }
    }

    # one way or another, we're done - clear the flag
    
    set gTCSbusy 0
    set gpausetelemetry 0

    return $ngresult
  # End else using TCS NG. - arg
  }
}


########################################################################################
# routine to assemble a TCS-ng command. Input is a command, followed
# by a variable length list with at least one argument, a timeout in seconds
# which is ignored. All other arguments in the list prior to the timeout
# are arguments to the command. Return string begins with a COMMAND or REQUEST
# keyword, followed by the uppercase command followed by the appropriate 
# arguments. N.B. No syntax or semantics are checked by this routine other
# than a cursory check for the correct number of arguments in the variable
# parameter list, including the TIMEOUT value  

# Initial release   07/25/08  ECB
########################################################################################

proc MakeTCSngCommand {command arglist} {

    global verbose

    set command [string toupper $command] 

    # EJC: apparently need to turn arglist into a string w/o curly braces, since otherwise it looks like a single-element list:
    set arglist [ string map { "{" "" "}" "" } $arglist ]

    # define commands according to their formats

    set requestcommands [list ALL AZ BEAM CON CORRECTIONS DATE DEC DISEPOCH DISABSTAT DOME EL EQ FOCUS FLEXFILE FOCSPEED HA INDEX JD LIMIT LIMITPROF LIMITINHIBIT MOTION PADGUIDE PADDRIFT PAD PECSTAT PECPROG PP RA SAMDATA SAMDONE SECZ SERVO SIM SHOWP SRVFRQ ST TEST1 TIME XALL XDEC XRA ]

    # some commands, i.e. sysreset, can have a variable number of arguments (i.e. zero or three)

    set zeroargs [list CANCEL DISABLE ENABLE FOCZERO MOVNEXT MOVSTOW FOCSTOP FOCUP FOCDN SYSRESET SYSSAVE SYSKILL SUN MOON MERCURY MARS JUPITER SATURN URANUS NEPTUNE PLUTO CLEARDIFF ]
    set oneargs [list BIASDEC BIASRA DECLARE DISEPOCH DOME DOMEGOTO FOCUS FOCSPEED LIMIT PEC PADDLE PADGUIDE PADDRIFT RELFOCUS STEPDEC STEPRA TRACK BIAS ABERRATE PRECES PROPMO REFRAC FLEX NUTAT PARALLAX ABELL FK5 IC NGC OKESTONE PPM SAO YBSC ZWICKY ]
    set twoargs [list ELAZ DOME PAD]
    set threeargs [list ELAZ DOME SYSRESET VERIFY ]
    set fourargs [list ]
    set fiveargs [list NEXTPOS MOVRADEC]

    # now search each list for the command, if found, append the arguments (without the timeout)

    if {[lsearch $requestcommands $command] != -1} {
	set returnstring "REQUEST"

	# a couple of ugly exceptions:

	# DISABSTAT is not a valid TCS command, but we include it here to differentiate between
	# the DISABLE request and identical command - both have zero args.  We should maybe make
	# separate procs to deal with commands and requests

	if {$command == "DISABSTAT"} {
	    set command "DISABLE"
	}

	# DOME FOCUS & DISEPOCH are tricky because they are both request and a command: request 
	# has no arguments but command has one or more.  If it's a command, then fall through to 
	# be identified as such.  There are also a few other TCS commands we don't currently use
	# that accept a variable number of args, i.e. DOME vs. DOME PARAM request

	# (remember there is always an extra arg appended, so 0 args looks like 1)

	if {($command == "DOME" || $command == "FOCUS" || $command == "DISEPOCH" || $command == "FOCSPEED" || $command == "PADGUIDE" || $command == "PADDRIFT" || $command == "PAD" || $command == "LIMIT") && [ llength $arglist ] != 1} {
	    # do nothing
	} else {
	    set returnstring [concat $returnstring $command]
	    return $returnstring
	}
    }

    if {[lsearch $zeroargs $command] != -1} {

	if {$command == "SYSRESET" && [ llength $arglist ] != 1} {
	    # do nothing...sysreset command with more args will show up later
	} else {
	    set returnstring "COMMAND"
	    set returnstring [concat $returnstring $command]
	    return $returnstring
	}
    }

    if {[lsearch $oneargs $command] != -1} {

	if {$verbose} {
	    puts "AQTELEM: Saw 1 arg command $command.  arglist = '$arglist'"
	}

	# DOME command can have either one, two or three args: check which one:

	if {$command == "DOME" && [ llength $arglist ] != 2} {
	    # do nothing, and fall through to next test
	} else {

	    if {[llength $arglist] != 2} {
		if {$verbose} {
		    puts "   arglist = '$arglist', length = [ llength $arglist ]"
		}
		set returnstring "ERROR1"
	    } else {
		# DOME PARAM is a request; all other 1 arg dome commands are commands

		if {$command == "DOME" && [ string first "PARAM" $arglist ] != -1} {
		    set returnstring "REQUEST"
		} else {
		    set returnstring "COMMAND"
		}
		set returnstring [concat $returnstring $command]
		set returnstring [concat $returnstring [lrange $arglist 0 0]]
	    }
	    return $returnstring
	}
    }

    if {[lsearch $twoargs $command] != -1} {
	if {$verbose} {
	    puts "Saw 2 arg command $command.  arglist = '$arglist'"
	}
	if {($command == "ELAZ" && [ llength $arglist ] != 3) || ($command == "DOME" && [ llength $arglist ] != 3)} {
	    # do nothing and fall through to next test
	} else {

	    if {[llength $arglist] != 3} {
		set returnstring "ERROR2"
	    } else {
		set returnstring "COMMAND"
		set returnstring [concat $returnstring $command]
		set returnstring [concat $returnstring [lrange $arglist 0 1]]
	    }
	    return $returnstring
	}
    }

    if {[lsearch $threeargs $command] != -1} {
	if {$verbose} {
	    puts "Saw 3 arg command $command"
	}
	if {[llength $arglist] != 4} {
	    set returnstring "ERROR3"
	} else {
	    if {$command == "VERIFY"} {
		set returnstring "REQUEST"
	    } else {
		set returnstring "COMMAND"
	    }
	    set returnstring [concat $returnstring $command]
	    set returnstring [concat $returnstring [lrange $arglist 0 2]]
	}
	
	return $returnstring
    }

    # no four argument commands

    if {[lsearch $fiveargs $command] != -1} {
	if {[llength $arglist] != 6} {
	    set returnstring "ERROR5"
	} else {
	    set returnstring "COMMAND"
	    set returnstring [concat $returnstring $command]
	    set returnstring [concat $returnstring [lrange $arglist 0 4]]
	}
	return $returnstring
    }
    
    set returnstring "ERRORDEFAULT"
}

    

########################################################################################
# return a continuously recycling variable for use in commanding
# TCSng. Creates and sets it to 1 if the global does not exist (
# true first time called) and then increments it by one on successive
# calls. Sets back to 1 if it reaches 256. Useful when logging TCS
# commands.
########################################################################################

proc GetnextTCSngCommandID {} {

    global aqglob.tcl

    if {![info exists gCurrentTCSngCommandID]} {
	set gCurrentTCSngCommandID 1
    } else {
	incr gCurrentTCSngCommandID
    }

    if {$gCurrentTCSngCommandID > 255} {
	set gCurrentTCSngCommandID 1
    }

    return $gCurrentTCSngCommandID

}

# Initialize TCS socket communications.
# Create a thread pool consisting of only one thread because we
# want to use the tpool's FIFO queue and non-blocking result waits
# but we only want one open socket to TCS at a time.
proc TCSCommStart {} {

    global verbose

  package require Thread
  
  # Create the thread pool, telling new threads to load socket2tcs.tcl and what the TCS address is.
  set ::g_hTCSSocketThreadPool [tpool::create -minworkers 1 -maxworkers 1 -idletime 86400 -initcmd [concat "source socket2tcs.tcl; set gTCSinterface " $::gTCSinterface]]
  set ::g_nTCSSocketQueued 0

  if {$verbose} {
      puts "AQTELEM: TCSCommStart: gTCSinterface = $::gTCSinterface"
  }
}

# Stop TCS socket communications.
proc TCSCommStop {} {

  # If thread pool exists then release it.
  if {[info exists ::g_hTCSSocketThreadPool] && $::g_hTCSSocketThreadPool != 0 && [lsearch -exact [tpool::names] $::g_hTCSSocketThreadPool] >= 0} {
    catch { tpool::release $::g_hTCSSocketThreadPool }
  }
  set ::g_nTCSSocketQueued 0
  set ::g_hTCSSocketThreadPool 0
}

# Send a command to TCS via a thread.
proc TCSComm {sCommand {iTimeout 10}} {

  set lRet [list ERROR ""]

  # If socket communications started.
  if {[info exists ::g_hTCSSocketThreadPool] && $::g_hTCSSocketThreadPool != 0} {
  
    # If too many commands already queued.
    if {$::g_nTCSSocketQueued > 20} {
      puts "TCSComm Error: $g_lnTCSSocketJobs COMMANDS ALREADY QUEUED FOR TCS.  IGNORING: $sCommand"
    } else {
    
      incr $::g_nTCSSocketQueued
      if {[catch {
      
        # Queue the command in the thread pool.
        set hJob [tpool::post -nowait $::g_hTCSSocketThreadPool [list TCSWriteRead $sCommand]]
        # Save the resulting job handle.
        lappend ::g_lhTCSSocketJobs $hJob
        # Wait for the job to finish.
        tpool::wait $::g_hTCSSocketThreadPool $hJob
        # Get the result of the job, which is the socket communication result.
        set lRet [tpool::get $::g_hTCSSocketThreadPool $hJob]
        
      } sResult]} {
      
        puts "TCSComm Error: While posting to thread pool and getting result: $sResult"
      }
      incr $::g_nTCSSocketQueued -1
    }
  }
  update idletasks
  
  return $lRet
}

