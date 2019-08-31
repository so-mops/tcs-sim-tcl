#!/usr/bin/wish
################################################################################
proc HelpExit {} {

    puts "\n  Usage: css-ng.tcl \[LOCATION\] \[-sim\] \[-eng\] \[Hz\]\n"
    puts "    Optional args: \[LOCATION\] can be 703, G96, I52, V06, E12 or 21-IN"
    puts "                   \[-sim\] turns simulation mode ON"
    puts "                   \[-eng\] turns engineering mode ON"
    puts "                   \[-ver\] turns verbose output ON"
    puts "                   \[Hz\] is the refresh rate, cycles per second (max. 5)\n"
    puts "    If a LOCATION is not defined, 703 is assumed.\n"

    exit
}
################################################################################
proc NullRoutine {} {

    global verbose

    if {$verbose} {
	puts "GUI: NullRoutine"
    }
}
################################################################################
proc Time {} {

    return [ clock clicks -milliseconds ]
}
################################################################################
proc ReportTime { t1 t2 str } {

    puts "$str : [ expr {$t2 - $t1} ]"
}
################################################################################
proc ExitTCS {} {

    global SIMULATION simpid gTCSConnected

    set result [ MessBox "Exiting" "Note that closing the TCS GUI will NOT shut down the TCS.\n\nMake sure the telescope is properly stowed and shut down before leaving it unattended.\n\nContinue exiting the GUI?" yesno ]

    if {$result == "NO"} {
	return
    }

    # if we launched a simulator, then kill it:
    if {$SIMULATION} {
	
	set result [ MessBox "Simulation mode" "Also stop the TCS simulator?" yesno ]
	if {$result == "YES"} {
	    catch { exec kill -9 $simpid }
	    catch { exec ./killsim.tcl }
	}
    }

    # disconnect nicely from TCS before exiting:
    if {$gTCSConnected} {
	ToggleTelemetryGUI
	after 100
    }

    exit
}
################################################################################
proc Deg2Rad { deg } {

    global pi

    set rad [ expr {($deg * $pi) / 180.0} ]

    return $rad
}
################################################################################
proc Rad2Deg { rad } {
    
    global pi

    set deg [ expr {($rad/$pi) * 180.0} ]

    return $deg
}
################################################################################
proc ConvertToTelem { sex } {

    set telem [ string map { : "" } $sex ]
    return $telem
}
################################################################################
proc sex2dec { sexvalue } {

    scan $sexvalue {%3d:%2d:%4f} place1 place2 place3

    set decval [expr {abs($place1) + $place2 / 60.0 + $place3 / 3600.0}]

    if {[string index $sexvalue 0] == "-"} {
        set decval [ expr {$decval * -1.0} ]
    }

    return $decval
}
################################################################################
proc ValidateCoords { inputRA inputDEC inputEpoch } {
# masterfully-crafted proc by E. Beshore, from aqutility.tcl
# would be even more masterful to allow integer seconds, instead of requiring sub-seconds...

    # check out the RA syntax by matching it with a masterfully-crafted regular expression ;-)
    set regexppat {[ ]*([0-1][0-9]|2[0-3])[ :;.\-*/+hH][0-5][0-9][ :;.\-*/+mM][0-5][0-9].[0-9][ sS]?[ ]*}

    if {$inputRA == "" || ![regexp $regexppat $inputRA]} {
        set message "Right Ascension must be specified and be of the form XX:XX:XX.X and must include leading zeros. Separators may include a single space, a character from the following: \":;.-*/+\", or H,M, or S as appropriate."
        MessBox "Error" $message ok
        return "ERR"
    }

    # now the DEC
    set regexppat {[ ]*(\+|-)(90[ :;.\-*/+dD]00[ :;.\-*/+mM]00.0|[0-8][0-9][ :;.\-*/+dD][0-5][0-9][ :;.\-*/+mM][0-5][0-9].[0-9])[ sS]?[ ]*}

    if {$inputDEC == "" || ![regexp $regexppat $inputDEC]} {
        set message "Declination must be specified and be of the form \[+ or -\]XX:XX:XX.X and must include leading zeros. Separators may include a single space, a character from the following: \":;.-*/+\", or D,M, or S as appropriate."
        MessBox "Error" $message ok
        return "ERR"
    }

    # clean up epoch

    set inputEpoch [string trim $inputEpoch " "]

    # Match xxxx., xxxx.x or xxxx.xx

    set regexppat {[0-9]{4}.[0-9]{0,2}}

    if {$inputEpoch == "" || ![regexp $regexppat $inputEpoch]} {
        set message "Epoch must be specified and be of the form XXXX.XX (e.g. 2000.00)."
        MessBox "Error" $message ok
        return "ERR"
    }

    # if user did not supply one or two trailing zeros, add them

    if {[string length $inputEpoch] == 5} {
        append inputEpoch "00"
    } elseif {[string length $inputEpoch] == 6} {
        append inputEpoch "0"
    }

    return "OK"
}
################################################################################
proc dec2sex { decvalue { decimalplaces 1 } } {

    set isneg 0

    if {$decvalue < 0.0 } {
        set isneg 1
        set decvalue [expr {$decvalue * -1}]
    }

    set place1 [expr {int($decvalue)}]
    set decvalue [expr {$decvalue - $place1}]
    set decvalue [expr {$decvalue * 60.0}]
    set place2 [expr {int($decvalue)}]
    set decvalue [expr {$decvalue - $place2}]
    set place3 [expr {$decvalue * 60.0}]
    set place1 [expr {$place1}]

    # now build up the value                        

    if {$isneg} {
        set result [format {-%2.2d:%2.2d:%07.4f} $place1 $place2 $place3]
    } else {
        set result [format {%2.2d:%2.2d:%07.4f} $place1 $place2 $place3]
    }

    # we formatted to avoid round off, now trim to one decimal place

    set len [string length $result]
    set result [string range $result 0 [expr {$len - 5 + $decimalplaces}]]

    return $result
}
################################################################################
proc DrawCircle { x y size color type width { tag "" } } {

    global top

    set r [ expr {$size / 2} ]
    
    set x1 [ expr {$x - $r} ]
    set y1 [ expr {$y - $r} ]
    set x2 [ expr {$x + $r} ]
    set y2 [ expr {$y + $r} ]
    
    if {$type == "fill"} {
	$top.radar create oval "$x1 $y1 $x2 $y2" -outline $color -fill $color -width $width -tags "$tag"
    } elseif {$type == "dash"} {
	$top.radar create oval "$x1 $y1 $x2 $y2" -outline $color -dash {.} -width $width -tags "$tag"
    } else {
	$top.radar create oval "$x1 $y1 $x2 $y2" -outline $color -width $width -tags "$tag"
    }
}
################################################################################
proc GetRadarXY { az el { dome 0 } } {
# dome and telescope are on two different coordinate systems - telescope az, el
# is aligned with the radar, but the dome is offset to properly display relative
# offset between tel + dome due to rho/nu/phi values

    global rcentx rcenty rwidth rheight rscale rborder dcentx dcenty

    set r [ expr {(90 - $el) / $rscale} ]

    set radaz [ Deg2Rad $az ]

    set xoff [ expr {$r * sin($radaz)} ]
    set yoff [ expr {$r * cos($radaz)} ]
    
    if {!$dome} {
	# to display E on left, use $rcentx - $xoff.  E on right, $rcentx + $xoff
	set x [ expr {$rcentx - $xoff} ]
	set y [ expr {$rcenty - $yoff} ]
    } else {
	# to display E on left, use $dcentx - $xoff.  E on right, $dcentx + $xoff
	set x [ expr {$dcentx - $xoff} ]
	set y [ expr {$dcenty - $yoff} ]
    }
    return "$x $y"
}
################################################################################
proc ShowRadar {} {

    global rscale relevcolor rcentx rcenty top gTCSConnected rbackcolor rtextcolor
    
    set radar $top.radar

    $top.radar delete "elevation"
    $top.radar delete "azimuth"
    $top.radar delete "tick"

    set color $relevcolor
    set textcolor $rtextcolor

    if {!$gTCSConnected} {
	set color grey25
	set textcolor $color
    }

    # draw elevation lines:
    set elevations "90 60 50 40 30 20 0"
    foreach r $elevations {

	set size [ expr {int(2 * (90 - $r) / $rscale)} ]
	set type "normal"
	DrawCircle $rcentx $rcenty $size $color $type 1 "elevation"
    }

    # draw azimuth tick marks
    for {set i 0} {$i < 360} {incr i 5} {

	set tick 3

	if {[ expr {$i % 30} ] == 0} {
	    set tick 10
	}

	set p1 [ GetRadarXY $i 0.5 ]
	set p2 [ GetRadarXY $i $tick ]
	
	$radar create line "$p1 $p2" -width 1 -fill $color -tags "tick"
    }

    # draw cardinal direction lines
    set p1 [ GetRadarXY 0 0 ]
    set p2 [ GetRadarXY 180 0 ]
    $radar create line "$p1 $p2" -width 1 -fill $color -dash 5 -tags "azimuth"

    set p1 [ GetRadarXY 90 0 ]
    set p2 [ GetRadarXY 270 0 ]
    $radar create line "$p1 $p2" -width 1 -fill $color -dash 5 -tags "azimuth"

    # draw directional boxes w/ text:
    set directions "{0 N} {90 E} {180 S} {270 W}"
    set h [ expr {int(5 * (1/$rscale))} ]
    set w [ expr {int(5 * (1/$rscale))} ]

    foreach pair $directions {

	set d [ lindex $pair 0 ]
	set char [ lindex $pair 1 ]
	set p1 [ GetRadarXY $d 0 ]
	set p2 [ GetRadarXY $d 10 ]

	set p2x [ lindex $p2 0 ]
	set p2y [ lindex $p2 1 ]

        set p2x1 [ expr {$p2x + $w} ]
        set p2x2 [ expr {$p2x - $w} ]
        set p2y1 [ expr {$p2y + $h} ]
        set p2y2 [ expr {$p2y - $h} ]

        $radar create rectangle "$p2x1 $p2y1 $p2x2 $p2y2" -width 1 -outline $color -fill $rbackcolor -tags "azimuth"
        $radar create text $p2 -text $char -fill $textcolor -tags "azimuth"
    }
}
################################################################################
proc FixRadar {} {
    
    set radar [ BuildRadar ]
    pack $radar

    ShowDome current
    ShowTel current

    update idletasks
}
################################################################################
proc BuildRadar {} {

    global top
    global rwidth rheight rcentx rcenty rbackcolor rtextcolor relevcolor rscale
    global rlimitcolor radarDisplayed

    set radarDisplayed 1

    set showlimits 0
    set limitfile "ObsHorizon.tcl"
    if {![ file isfile $limitfile ]} {
	puts "GUI: Cannot find limits file '$limitfile'"
	set showlimits 0
    }

    set showlimits 0

    set radar $top.radar
    
    if {[ winfo exists $radar ]} {

	destroy $top.radar
    }

    canvas $radar -width $rwidth -height $rheight -background $rbackcolor

    ShowRadar

    # draw limits:

    if {$showlimits} {

	# we assume that LOCATION has already been set
	source $limitfile

	set entry [ lindex $HorizonList 0 ]

	# "step" degree intervals to mark limits - must be between 1 and 15:
	set step 3

	set az1 [ lindex $entry 0 ]
	set el1 [ lindex $entry 1 ]

	foreach entry $HorizonList {

	    set az2 [ lindex $entry 0 ]
	    set el2 [ lindex $entry 1 ]

	    for {set i $az1} {$i <= $az2} {set i [ expr {$i + $step} ]} {

		# first special case:
		if {$az1 == $az2} {
		    continue
		}

		# i is az
		set thisaz $i

		# find interpolated elevation:
		set thisel [ expr {($thisaz - $az1) * (($el2 - $el1)/($az2 - $az1)) + $el1} ]

		set limxy [ GetRadarXY $thisaz $thisel ]
		set limx [ lindex $limxy 0 ]
		set limy [ lindex $limxy 1 ]

		DrawCircle $limx $limy 2 $rlimitcolor "fill" 1 "limit"
	    }
	    set az1 $az2
	    set el1 $el2
 	}
    }

    # give the user a way to force a re-draw of the radar if things get messed up:
    bind $radar <Button-1> { FixRadar }

    return $radar
}
################################################################################
proc Refresh {} {

    global refreshrate tcswait gTCSConnected verbose radarDisplayed gLatency refreshHertz

    update idletasks

    if {$radarDisplayed} {
	UpdateRadar
    }

    if {!$gTCSConnected} {
	if {$verbose} {
	    puts "GUI: called Refresh but leaving - no TCS connection"
	}
	return
    }

    if {[ info exists tcswait ]} {
	after cancel $tcswait
    }

    UpdateStatus

    if {$gTCSConnected} {
	after $refreshrate [ set tcswait Refresh ]
    } else {
	after $refreshrate [ set tcswait NullRoutine ]
    }

    # periodically re-calibrate the latency:                                    
    if {$gLatency(count) == $gLatency(repeat)} {                               
	set gLatency(calibrated) 0                                              
	set gLatency(count) 0
    }                                                                          

    if {!$gLatency(calibrated)} {
	
	set skipcount 5
	set maxcount 10
	
	# skip the first couple of calls:
	if {$gLatency(count) == $skipcount} {
	    set gLatency(baseline) [ Time ]
	}
	
	if {$gLatency(count) == $maxcount} {
	    
	    # ready to calibrate latency:
	    set gLatency(baseline2) [ Time ]
	    
	    set targetrate [ expr {1000 / $refreshHertz * 1.0} ]
	    set requested [ expr {$targetrate * ($maxcount - $skipcount)} ]
	    set actual [ expr {$gLatency(baseline2) - $gLatency(baseline)} ]
	    
	    set totalwait [ expr {$refreshrate * ($maxcount - $skipcount)} ]
	    set totalwork [ expr {$actual - $totalwait} ]

#	    set cyclework [ expr {$totalwork / ($maxcount - $skipcount)*1.0} ]
#	    puts $cyclework
	    
	    set refreshrate [ expr {int(($requested - $totalwork)/(($maxcount - $skipcount)*1.0))} ]
	    # don't let refreshrate drop to zero
	    if {$refreshrate < 10} {
		set refreshrate 10
	    }

	    set gLatency(count) 0
	    set gLatency(calibrated) 1
	}
    }
    incr gLatency(count)                                                       
}
################################################################################
proc UpdateRadar {} {

    ShowRadar
    ShowDome "current"
    ShowTel "current"

    update idletasks
}
################################################################################
proc DumpTCS {} {

    global gTCSFocus currentFocus

    GetTelemetry

    # limits?

    set currentFocus $gTCSFocus 

    update idletasks
}
################################################################################
proc GetDome {} {
# DOME call returns 2 element list: autodome status and dome position

    set pos [ SendCommand "DOME" ]

    return $pos
}
################################################################################
proc GetDomePos {} {
# DOME call returns 2 element list: autodome status and dome position

    set pos [ lindex [ SendCommand "DOME" ] 1 ]

    return $pos
}
################################################################################
proc GetDomeParam {} {
# populate global variables that tracks telescope position within the dome
    
    global domeConst dcentx dcenty rcentx rcenty dscale

    set domestring [ SendCommand "DOME" "PARAM" ] 

    set domeConst(nu) [ lindex $domestring 4 ]
    set domeConst(rho) [ lindex $domestring 5 ]
    set domeConst(phi) [ lindex $domestring 6 ]

    # calculate the center of the dome relative to telescope:

    set elx [ expr {90 - abs(90 * $domeConst(rho))} ]
    if {$domeConst(rho) < 0} {
	set azx 270
    } else {
	set azx 90
    }
    set dcentx [ lindex [ GetRadarXY $azx $elx ] 0 ]

    set ely [ expr {90 - abs(90 * $domeConst(phi))} ]
    if {$domeConst(phi) < 0} {
	set azy 0
    } else {
	set azy 180
    }
    set dcenty [ lindex [ GetRadarXY $azy $ely ] 1 ]
}
################################################################################
proc GetAutoDome {} {
# DOME call returns 2 element list: autodome status and dome position

# dome status: 0=autodome off, 1= autodome on, 2=user domegoto move

    return [ lindex [ SendCommand "DOME" ] 0 ]
}
################################################################################
proc GetCorrection { corr { corrstring "" } } {
# returns 1 if requested correction is active, 0 if inactive.  requesting "all"
# returns the full correction string from the TCS

# if corrstring is supplied, check the string, otherwise, ask the TCS

    switch $corr {

	"M" { set i 0 }
	"P" { set i 1 }
	"N" { set i 2 }
	"A" { set i 3 }
	"R" { set i 4 }
	"F" { set i 5 }
	"p" { set i 6 }
	"+" { set i 7 }
	"t" { set i 8 }
	"o" { set i 9 }
	"b" { set i 10 }
	"all" {}

	default { return "ERROR" }
    }

    if {$corrstring == ""} {
	set corrstring [ SendCommand "CORRECTIONS" ]
    }

    if {$corr == "all"} {
	return $corrstring
    }

    set trackbit [ string range $corrstring $i $i ]
    
    if {$trackbit == $corr} {
	return 1
    }

    return 0
}
################################################################################
proc DisEnableGUI { state } {

    global top ebackcolor engineering gPaddleState

    set lbg $ebackcolor

    if {$state != "normal" && $state != "disabled"} {
	puts "GUI: DisEnableGUI: invalid state '$state'"
	return
    }

    set tel $top.telescope
    set eng $top.engineering
    set lbox $tel.list
    set pad $top.pad

    set buttons "bslew bstop bdome bfoc bdis btrack bpec bauto bdini bstow bload btelem binit bdstop bcat"
    set entries "era edec eaz eel edome efoc"
    set radios "rradec razel"
    set labels "ldate lut lcut lloc llst lcdate lclst lha lcha lra lcra ldec lcdec lepo lcepo laz lcaz lel lcel lam lcam ldome lcdome lfoc lcfoc ldis ltrack lpec lauto ltelem lcat"
    set listbox "list.head list.text"

    if {$engineering} {
	set engstring "lcom ecom lfeed"
	foreach thing $engstring {
	    $eng.$thing config -state $state
	}
    }

    set everything "$buttons $entries $radios $labels $listbox"

    foreach thing $everything {
	$tel.$thing config -state $state
    }

    if {$state == "disabled"} {
	$tel.list.scroll config -command { NullRoutine }
    } else {
	$tel.list.scroll config -command "$tel.list.text yview"
    }

    update idletasks

    # re-define the keyboard bindings:
    KeyboardTraversal
}
################################################################################
proc ReturnMoveStatus {} {
# motion status is carried in gTCSmovestatus from aqtelemetry.  It is an 8-bit int
# 64 = ??
# 32 = ??
# 16 = Derotator?
# 8  = Dome
# 4  = Focus
# 2  = Dec
# 1  = RA 

    global gTCSmovestatus

    set ra 0
    set dec 0
    set focus 0
    set dome 0

    set int $gTCSmovestatus

    if {$int >= 64} {
	incr int -64
    }
    if {$int >= 32} {
	incr int -32
    }
    if {$int >= 16} {
	incr int -16
    }
    if {$int >= 8} {
	set dome 1
	incr int -8
    }
    if {$int >= 4} {
	set focus 1
	incr int -4
    }
    if {$int >= 2} {
	set dec 1
	incr int -2
    }
    if {$int >= 1} {
	set ra 1
	incr int -1
    }

    if {$int != 0} {
	puts "GUI: problem parsing motion bits ('$int' != 0)"
    }

    return "$ra $dec $focus $dome"
}
################################################################################
proc UpdateStatus {} {

    # global variables may automatically update status in labels - check acquisition code for examples

    global gAutoDome top offcolor oncolor gMovemode
    global currentFocus currentDome currentDate currentAirmass currentCorrections
    global gTCSmovestatus gTCSConnected 
    global gTCSRAlimit gTCSDEClimit gTCSHorzlimit gTCSFocUplimit gTCSFocDnlimit gTCSSoftlimit gTCSDRlimit
    global gTCSfocusstatus gTCSdomestatus gTCSRAstatus gTCSDECstatus gPaddleState gTCSDisabled
    global motionRA motionDEC motionDome motionFoc currentGuide currentDrift currentBiasRA currentBiasDEC

    set tel $top.telescope
    set pad $top.pad

    # parse "ALL" return from TCS: this handles RA, Dec, LST, LHA, UT time, move status, limits,
    # disabled status and possibly focus?

    DumpTCS

    set currentDate [ SendCommand "DATE" ]
    set corrstring [ GetCorrection "all" ]
    set currentCorrections $corrstring

    set biascheck [ GetCorrection "b" $corrstring ]
    if {$biascheck} {
	$pad.bbias config -state normal -text "STOP BIAS"
	$pad.lcbira config -state active
	$pad.lcbidec config -state active
    } else {
	$pad.bbias config -text "Apply Bias"
	ColorButton $pad.bbias
	if {$gPaddleState} {
	    $pad.lcbira config -state normal
	    $pad.lcbidec config -state normal
	}
    }

    set currentDrift [ expr {int([ GetRate "drift" ])} ]
    set currentGuide [ expr {int([ GetRate "guide" ])} ]
    set currentBiasRA [ GetRate "biasra" ]
    set currentBiasDEC [ GetRate "biasdec" ]

    # colors of toggle buttons/labels:

    # Enable/Disable:
    # DISABSTAT is a made-up call to the TCS: we intercept it in aqtelemetry and turn it into a
    # DISABLE request (not to be confused with a DISABLE command)
    set gTCSDisabled [ SendCommand "DISABSTAT" ]
    if {$gTCSDisabled == 1} { 
	set state active
	set text "ENABLE"
	set text2 "DISABLED"
    } else {
	set state normal
	set text "DISABLE"
	set text2 "ENABLED"
    }
    $tel.ldis   config -state $state -text $text2
    $tel.bdis   config -text $text

    # tracking:
    set trackcheck [ GetCorrection "t" $corrstring ]
    if {$trackcheck == 0} {
	set text "OFF"
	set state active
    } else {
	set state normal
	set text "ON"
    }
    $tel.ltrack config -state $state -text $text

    # PEC:
    set peccheck [ SendCommand "PECSTAT" ]
    if {$peccheck == 0} {
	set state active
	set text "OFF"
    } elseif {$peccheck == 1} {
	set state normal
	set text "ON"
    } elseif {$peccheck == 2} {
	set state active
	set text "TRAINING"
    } elseif {$peccheck == 3} {
	set state active
	set text "INDEXING"
    }
    $tel.lpec   config -state $state -text $text

    # autodome:
    set domestatus [ GetDome ]
    set autocheck [ lindex $domestatus 0 ]
    set currentDome [ format "%0.1f" [ lindex $domestatus 1 ] ]
    if {$autocheck != 1} {
	set state active
	set text "OFF"
    } else {
	set state normal
	set text "ON"
    }
    $tel.lauto  config -state $state -text $text

    # Autodome - disable dome rotation control if autodome is active (only if drives are enabled):
    if {!$gTCSDisabled} {
	if {$autocheck == 1} {
	    set state disabled
	} else {
	    set state normal
	}
    } else {
	set state disabled
    }
    $tel.bdome config -state $state
    $tel.edome config -state $state

    # in-position flags:
    set motionstatus [ ReturnMoveStatus ]
    set motionRA [ lindex $motionstatus 0 ]
    set motionDEC [ lindex $motionstatus 1 ]
    set motionFoc [ lindex $motionstatus 2 ]
    set motionDome [ lindex $motionstatus 3 ]

    # telescope:
    if {$motionRA} {
	set state active
    } else {
	set state normal
    }
    $tel.lcra   config -state $state

    if {$motionDEC} {
	set state active
    } else {
	set state normal
    }
    $tel.lcdec  config -state $state

    # kind of redundant, but if ra or dec are moving, then by definition az and el are moving:
    if {$motionDEC || $motionRA} {
	$tel.lcaz   config -state active
	$tel.lcel   config -state active
    } else {
	$tel.lcaz   config -state normal
	$tel.lcel   config -state normal
    }

    # dome in-position
    if {$motionDome} {
	set state active
    } else {
	set state normal
    }
    $tel.lcdome config -state $state

    # focus in-position
    if {$motionFoc} {
	set state active
    } else {
	set state normal
    }
    $tel.lcfoc  config -state $state

    # airmass warning:
    if {$currentAirmass >= 2} {
	set state active
    } else {
	set state normal
    }
    $tel.lcam config -state $state

    # aqtelemetry updated limit bits, but we still need to check the limit override bit
    set gTCSlimoverride [ SendCommand "LIMITINHIBIT" ]

    # limit indicators:
    if {$gTCSRAlimit || $gTCSDEClimit || $gTCSHorzlimit || $gTCSFocDnlimit || $gTCSFocUplimit || $gTCSSoftlimit || $gTCSDRlimit || $gTCSlimoverride} {
	set text ""
	set state active
	append text "***  "
	if {$gTCSRAlimit} {
	    append text "RA "
	}
	if {$gTCSDEClimit} {
	    append text "DEC "
	}
	if {$gTCSHorzlimit} {
	    append text "HORIZON "
	}
	if {$gTCSFocDnlimit} {
	    append text "FOC(-) "
	}
	if {$gTCSFocUplimit} {
	    append text "FOC(+) "
	}
	if {$gTCSSoftlimit} {
	    append text "SOFT "
	}
	if {$gTCSDRlimit} {
	    append text "ROT "
	}
	append text "LIMIT "
	if {$gTCSlimoverride} {
	    append text "OVERRIDE "
	}
	append text " ***"
    } else {
	set state disabled
	set text "TELESCOPE WITHIN LIMITS"
    }

    set width [ string length $text ]
    $tel.llim  config -state $state -text $text -width $width

    # if telescope is disabled, disable other buttons too
    if {$gTCSDisabled} {
	set state disabled
	set state2 active
    } else {
	set state normal
	set state2 normal
    }

    set widgetlist "bslew bstop bstow era edec eaz eel rradec razel bdstop bfoc binit bdini efoc"
    foreach w $widgetlist {
	$tel.$w config -state $state
    }

    # move mode:
    if {!$gTCSDisabled} {
	if {$gMovemode == "azel"} {
	    set state1 "disabled"
	    set state2 "normal"
	} else {
	    set state1 "normal"
	    set state2 "disabled"
	}
	$tel.era  config -state $state1
	$tel.edec config -state $state1
	$tel.eaz  config -state $state2
	$tel.eel  config -state $state2
    }

    # paddle availability depends on telescope being enabled and connected to TCS...will be checked in SetPaddle proc:
    SetPaddle

    update idletasks
}
################################################################################
proc ShowPaddle {} {

    global paddleDisplayed

    if {$paddleDisplayed} {
	set paddleDisplayed 0
    } else {
	set paddleDisplayed 1
    }
}
################################################################################
proc ShowDome { which } {

    global rcentx rcenty top currentDome autoDomeTol movecolor rdomecolor 
    global motionDome gTCSConnected domeConst

    global domewidth halfwidth

    # this is expensive to draw...should only do it if we need to.  add yet another global variable
    # to indicate if we need to or not (check current=target?)

    set currentlinewidth 1
    set currcolor $rdomecolor
    set dashwidth 5

    # site-specific dome parameters like domewidth and halfwidth are calculated in the init proc

    if {$which == "current"} {

	if {![ info exists currentDome ] || $currentDome == ""} {
	    return
	}
	set pos $currentDome
	set tags "currentdome"
	if {$motionDome} {
	    set color $movecolor
	    set width 3
	} else {
	    set color $currcolor
	    set width 2
	}
    } else {
	puts "GUI: proc ShowDome bad arg '$which'"
	return
    }
    
    # get points at horizon:
    # get points beyond the radar screen:
    set currxy1 [ GetRadarXY [ expr {$pos + $halfwidth} ] -90 1 ]
    set currxy2 [ GetRadarXY [ expr {$pos - $halfwidth} ] -90 1 ] 
    
    # get points near (but not at) zenith:
    set dome1 [ expr {$pos + 128} ]
    set dome2 [ expr {$pos - 128} ]

    if {$dome1 >= 360} { set dome1 [ expr {$dome1 - 360} ] } elseif {$dome1 < 0} { set dome1 [ expr {$dome1 + 360} ] }
    if {$dome2 >= 360} { set dome2 [ expr {$dome2 - 360} ] } elseif {$dome2 < 0} { set dome2 [ expr {$dome2 + 360} ] }
    
    set zdist [ expr {90 - $domewidth} ]
    
    set currxy3 [ GetRadarXY $dome1 $zdist 1 ]
    set currxy4 [ GetRadarXY $dome2 $zdist 1 ]
    
    if {!$gTCSConnected} {
	set color grey25
    }

    $top.radar delete $tags
    $top.radar create line "$currxy1 $currxy3" -width $width -fill $color -tags $tags
    $top.radar create line "$currxy3 $currxy4" -width $width -fill $color -tags $tags
    $top.radar create line "$currxy4 $currxy2" -width $width -fill $color -tags $tags
}
################################################################################
proc ShowTel { which } {

    # global variables from aqtelemetry:
    global currentAZ currentEL targetAZ targetEL top rscale rtelcolor movecolor rtelsize rpixm1 rpixm2
    global motionRA motionDEC motionFoc gTCSConnected

    if {$which == "current"} {
	set thisAZ $currentAZ
	set thisEL $currentEL

	if {$thisAZ == "" || $thisEL == ""} {
	    puts "GUI: proc ShowTel found currentAZ = '$currentAZ', currentEL = '$currentEL'.  Exiting ShowTel."
	    return
	}

	if {$motionRA || $motionDEC} {
	    set color $movecolor
	    set width 3
	} else {
	    set color $rtelcolor
	    set width 2
	}

    } else {
	puts "GUI: proc ShowTel bad argument '$which'"
	return
    }

    set thisXY [ GetRadarXY $thisAZ $thisEL ]

    set x [ lindex $thisXY 0 ]
    set y [ lindex $thisXY 1 ]

    set color2 $rtelcolor
    set width2 2

    if {$motionFoc} {
	set color2 $movecolor
	set width2 3
    }

    if {!$gTCSConnected} {
	set color grey25
	set color2 grey25
    }

    $top.radar delete $which

    # rpixm1 and rpixm2 are the pre-calculated diameters of the primary and secondary

    DrawCircle $x $y $rpixm1 $color "" $width "$which"
    DrawCircle $x $y $rpixm2 $color2 "" $width2 "$which"
}
################################################################################
proc PadTel { dir } {
# valid directions are NORTH SOUTH EAST WEST NE NW SE SW.  Add guide/drift rate
# as second argument.  Unknown args will stop the motion.

    global top gGuideDrift currentGuide currentDrift

    set pad $top.pad

    if {$gGuideDrift == "guide"} {
	set rate $currentGuide
	set state1 active
	set state2 normal
    } elseif {$gGuideDrift == "drift"} {
	set rate $currentDrift
	set state1 normal
	set state2 active
    }

    switch $dir {

	"NORTH" { SendCommand "PAD" "NORTH $rate" }
	"SOUTH" { SendCommand "PAD" "SOUTH $rate" }
	"EAST"  { SendCommand "PAD" "EAST $rate" }
	"WEST"  { SendCommand "PAD" "WEST $rate" }

	default { SendCommand "PAD" "XX 0" }
    }

    set state3 active

    if {$dir == "STOP"} {
	set state1 normal
	set state2 normal
	set state3 normal
    }

    $pad.lcguide config -state $state1
    $pad.lcdrift config -state $state2

    $pad.ltel    config -state $state3
}
################################################################################
proc PadDome { dir } {
# arguments are LEFT, RIGHT; everything else should stop the motion

    global top

    set pad $top.pad

    set result [ SendCommand "DOME" "PADDLE $dir" ]

    # visual feedback on the paddle:
    set state normal
    if {$dir == "LEFT" || $dir == "RIGHT"} {
	set state active
    }

    $pad.ldome config -state $state

    return $result
}
################################################################################
proc PadFoc { value } {
# valid arguments are FOCUP FOCDN and FOCSTOP

    global top

    set pad $top.pad

    set result [ SendCommand $value ]

    # visual feedback on the paddle:
    set state normal
    if {$value == "FOCUP" || $value == "FOCDN"} {
	set state active
    }

    $pad.lfoc config -state $state

    return $result
}
################################################################################
proc BuildPaddle {} {

    global top engineering
    global pwidth pheight pbackcolor ptextcolor

    global gFocusSpeed gPaddleState gGuideDrift userBiasRA userBiasDEC userGuide userDrift
    global currentBiasRA currentBiasDEC currentGuide currentDrift currentCorrections

    set px 5
    set py 3
    
    set fg black
    set fg2 white
    set fg3 grey75
    set efg black
    set bw 4
    set bw2 10
    set bh 1
    set lw 6
    set lw2 6
    set lbg $pbackcolor
    set lbg2 grey10

    set ipy 10
    set ipy2 3

    set ew 6
    set ebg white
    set edbg grey50

    set relief "solid"
    set movecolor yellow

    set ht 3
    set hc white

    set pad $top.pad

    frame $pad -width $pwidth -height $pheight -background $pbackcolor

    # dome L/R buttons:
    label  $pad.ldome  -width $lw -bg $lbg -fg $fg3 -text "Dome" -activebackground $movecolor -activeforeground $fg
    button $pad.bdomel -width $bw -height $bh -text "Left" -highlightbackground $lbg
    button $pad.bdomer -width $bw -height $bh -text "Right" -highlightbackground $lbg
    ColorButton $pad.bdomer
    ColorButton $pad.bdomel

    # stop sending the commands on release of the button:
    bind $pad.bdomel <ButtonPress> { PadDome "LEFT" }
    bind $pad.bdomer <ButtonPress> { PadDome "RIGHT" }
    bind $pad.bdomel <ButtonRelease> { PadDome "STOP" }
    bind $pad.bdomer <ButtonRelease> { PadDome "STOP" }
    bind $pad.bdomel <Button-2> { NullRoutine }
    bind $pad.bdomel <Button-3> { NullRoutine }
    bind $pad.bdomer <Button-2> { NullRoutine }
    bind $pad.bdomer <Button-3> { NullRoutine }

    # telescope NSEW buttons:

    label  $pad.ltel   -width $lw -bg $lbg -fg $fg2 -activebackground $movecolor -activeforeground $fg -relief $relief -text "GUIDE"
    button $pad.bteln  -width $bw -height $bh -text "N" -highlightbackground $lbg
    button $pad.btels  -width $bw -height $bh -text "S" -highlightbackground $lbg
    button $pad.btele  -width $bw -height $bh -text "E" -highlightbackground $lbg
    button $pad.btelw  -width $bw -height $bh -text "W" -highlightbackground $lbg
    ColorButton $pad.bteln
    ColorButton $pad.btels
    ColorButton $pad.btele
    ColorButton $pad.btelw

    bind $pad.bteln <ButtonPress> { PadTel "NORTH" }
    bind $pad.btels <ButtonPress> { PadTel "SOUTH" }
    bind $pad.btele <ButtonPress> { PadTel "EAST" }
    bind $pad.btelw <ButtonPress> { PadTel "WEST" }
    bind $pad.bteln <ButtonRelease> { PadTel "STOP" }
    bind $pad.btels <ButtonRelease> { PadTel "STOP" }
    bind $pad.btele <ButtonRelease> { PadTel "STOP" }
    bind $pad.btelw <ButtonRelease> { PadTel "STOP" }
    bind $pad.bteln <Button-2> { NullRoutine }
    bind $pad.bteln <Button-3> { NullRoutine }
    bind $pad.btels <Button-2> { NullRoutine }
    bind $pad.btels <Button-3> { NullRoutine }
    bind $pad.btele <Button-2> { NullRoutine }
    bind $pad.btele <Button-3> { NullRoutine }
    bind $pad.btelw <Button-2> { NullRoutine }
    bind $pad.btelw <Button-3> { NullRoutine }

    # focus up/down/rate buttons:
    label  $pad.lfoc   -width $lw -bg $lbg -fg $fg3 -text "Focus" -activebackground $movecolor -activeforeground $fg
    button $pad.bfocup -width $bw -height $bh -text "Up" -highlightbackground $lbg
    button $pad.bfocdn -width $bw -height $bh -text "Down" -highlightbackground $lbg
    ColorButton $pad.bfocup
    ColorButton $pad.bfocdn

    # stop sending the commands on release of the button:
    bind $pad.bfocup <ButtonPress> { PadFoc "FOCUP" }
    bind $pad.bfocdn <ButtonPress> { PadFoc "FOCDN" }
    bind $pad.bfocup <ButtonRelease> { PadFoc "FOCSTOP" }
    bind $pad.bfocdn <ButtonRelease> { PadFoc "FOCSTOP" }
    bind $pad.bfocup <Button-2> { NullRoutine }
    bind $pad.bfocup <Button-3> { NullRoutine }
    bind $pad.bfocdn <Button-2> { NullRoutine }
    bind $pad.bfocdn <Button-3> { NullRoutine }

    # guide / drift / bias rates:

    radiobutton $pad.rdrift -bg $lbg -fg $fg2 -text "Drift" -variable gGuideDrift -value "drift" -command { ToggleGuideDrift } -activebackground $lbg -highlightthickness $ht -highlightbackground $lbg -highlightcolor $hc -activeforeground $fg2 -selectcolor $lbg2
    radiobutton $pad.rguide -bg $lbg -fg $fg2 -text "Guide" -variable gGuideDrift -value "guide" -command { ToggleGuideDrift } -activebackground $lbg -highlightthickness $ht -highlightbackground $lbg -highlightcolor $hc -activeforeground $fg2 -selectcolor $lbg2

    entry $pad.edrift  -width $ew -textvariable userDrift -fg $fg -bg $ebg -disabledbackground $edbg -justify center -highlightcolor $hc -highlightthickness $ht -highlightbackground $lbg -relief $relief
    entry $pad.eguide  -width $ew -textvariable userGuide -fg $fg -bg $ebg -disabledbackground $edbg -justify center -highlightcolor $hc -highlightthickness $ht -highlightbackground $lbg -relief $relief

    bind $pad.edrift <Return>   { SetRate "drift" }
    bind $pad.edrift <KP_Enter> { SetRate "drift" }
    bind $pad.edrift <Escape>   { set userDrift "" }
    bind $pad.eguide <Return>   { SetRate "guide" }
    bind $pad.eguide <KP_Enter> { SetRate "guide" }
    bind $pad.eguide <Escape>   { set userGuide "" }

    label $pad.lbira   -width $lw -bg $lbg -fg $fg2 -text "RA"
    label $pad.lbidec  -width $lw -bg $lbg -fg $fg2 -text "Dec"

    entry $pad.ebira   -width $ew -textvariable userBiasRA -fg $fg -bg $ebg -disabledbackground $edbg -justify center -highlightcolor $hc -highlightthickness $ht -highlightbackground $lbg -relief $relief
    entry $pad.ebidec  -width $ew -textvariable userBiasDEC -fg $fg -bg $ebg -disabledbackground $edbg -justify center -highlightcolor $hc -highlightthickness $ht -highlightbackground $lbg -relief $relief

    bind $pad.ebira  <Return>   { SetRate "biasra" }
    bind $pad.ebira  <KP_Enter> { SetRate "biasra" }
    bind $pad.ebira  <Escape>   { set userBiasRA "" }
    bind $pad.ebidec <Return>   { SetRate "biasdec" }
    bind $pad.ebidec <KP_Enter> { SetRate "biasdec" }
    bind $pad.ebidec <Escape>   { set userBiasDEC "" }

    label $pad.lcguide -width $lw2 -bg $lbg2 -fg $fg2 -relief $relief -textvariable currentGuide -activebackground $movecolor -activeforeground $fg
    label $pad.lcdrift -width $lw2 -bg $lbg2 -fg $fg2 -relief $relief -textvariable currentDrift -activebackground $movecolor -activeforeground $fg
    label $pad.lcbira  -width $lw2 -bg $lbg2 -fg $fg2 -relief $relief -textvariable currentBiasRA -activebackground $movecolor -activeforeground $fg
    label $pad.lcbidec -width $lw2 -bg $lbg2 -fg $fg2 -relief $relief -textvariable currentBiasDEC -activebackground $movecolor -activeforeground $fg

    # corrections string:
    label $pad.lcorr -width 12 -bg $lbg -fg $fg3 -textvariable currentCorrections

    button $pad.bbias  -width $bw2 -height $bh -text "Apply Bias" -highlightbackground $lbg -command { ToggleBias }
    ColorButton $pad.bbias

    # couple of blank labels for spacing:

    label $pad.lbla1 -width 2 -bg $lbg
    label $pad.lbla2 -width $lw -bg $lbg
    label $pad.lbla3 -width $lw -bg $lbg
    label $pad.lbla4 -width $lw -bg $lbg

    # grid config everything neatly:
    
    set row 0

    grid config $pad.lbla1  -column 3 -row $row

    incr row
    grid config $pad.bdomel -column 0 -row $row -padx {15 2} -pady $py -ipady $ipy -rowspan 2
    grid config $pad.ldome  -column 1 -row $row -padx $px -pady $py -ipady $ipy -rowspan 2
    grid config $pad.bdomer -column 2 -row $row -padx $px -pady $py -ipady $ipy -rowspan 2
    grid config $pad.bteln  -column 5 -row $row -padx $px -pady $py -ipady $ipy

    incr row
    grid config $pad.bfocdn -column 0 -row $row -padx {15 2} -pady $py -ipady $ipy -rowspan 2
    grid config $pad.lfoc   -column 1 -row $row -padx $px -pady $py -ipady $ipy -rowspan 2
    grid config $pad.bfocup -column 2 -row $row -padx $px -pady $py -ipady $ipy -rowspan 2
    grid config $pad.btelw  -column 4 -row $row -padx $px -pady $py -ipady $ipy
    grid config $pad.ltel   -column 5 -row $row -padx $px -pady $py -ipady $ipy
    grid config $pad.btele  -column 6 -row $row -padx $px -pady $py -ipady $ipy

    incr row
    grid config $pad.btels  -column 5 -row $row -padx $px -pady $py -ipady $ipy

    incr row

    incr row 
    grid config $pad.lbira  -column 0 -row $row -padx $px -pady $py
    grid config $pad.ebira  -column 1 -row $row -padx $px -pady $py -ipady $ipy2
    grid config $pad.lcbira -column 2 -row $row -padx $px -pady $py -ipady $ipy2
    grid config $pad.rguide -column 4 -row $row -padx $px -pady $py
    grid config $pad.eguide -column 5 -row $row -padx $px -pady $py -ipady $ipy2
    grid config $pad.lcguide -column 6 -row $row -padx $px -pady $py -ipady $ipy2

    incr row
    grid config $pad.lbidec -column 0 -row $row -padx $px -pady $py
    grid config $pad.ebidec -column 1 -row $row -padx $px -pady $py -ipady $ipy2
    grid config $pad.lcbidec -column 2 -row $row -padx $px -pady $py -ipady $ipy2
    grid config $pad.rdrift -column 4 -row $row -padx $px -pady $py
    grid config $pad.edrift -column 5 -row $row -padx $px -pady $py -ipady $ipy2
    grid config $pad.lcdrift -column 6 -row $row -padx $px -pady $py -ipady $ipy2

    incr row
    grid config $pad.lbla2 -column 0

    incr row
    grid config $pad.bbias  -column 0 -row $row -padx $px -pady $py -ipady $ipy2 -columnspan 3
    grid config $pad.lcorr  -column 3 -row $row -padx $px -pady $py -columnspan 3 -ipady $ipy

    incr row

    grid config $pad.lbla4 -column 0

    bind $pad.eguide <Leave> { CheckRate "guide" }
    bind $pad.edrift <Leave> { CheckRate "drift" }
    bind $pad.ebira  <Leave> { CheckRate "biasra" }
    bind $pad.ebidec <Leave> { CheckRate "biasdec" }
    
    return $pad
}
################################################################################
proc SetRate { rate } {

    global userGuide userDrift userBiasRA userBiasDEC

    set valid [ CheckRate $rate ]

    if {!$valid} {

	return
    }
    
    switch $rate {

	"guide"   {
	    set result [ SendCommand "PADGUIDE" $userGuide ]
	    set userGuide ""
	}
	"drift"   { 
	    set result [ SendCommand "PADDRIFT" $userDrift ] 
	    set userDrift ""
	}
	"biasra"  { 
	    set result [ SendCommand "BIASRA" $userBiasRA ] 
	    set userBiasRA ""
	}
	"biasdec" { 
	    set result [ SendCommand "BIASDEC" $userBiasDEC ] 
	    set userBiasDEC ""
	}

	default   { set result "ERROR" }
    }

    Refresh

    return $result
}
################################################################################
proc CheckRate { rate } {

    global userGuide userDrift userBiasRA userBiasDEC top

    set color red
    set ebg white

    set pad $top.pad

    if {$rate == "guide"} {
	set check $userGuide
	set entry $pad.eguide
    } elseif {$rate == "drift"} {
	set check $userDrift
	set entry $pad.edrift
    } elseif {$rate == "biasra"} {
	set check $userBiasRA
	set entry $pad.ebira
    } elseif {$rate == "biasdec"} {
	set check $userBiasDEC
	set entry $pad.ebidec
    } else {
	puts "GUI: CheckRate: unknown rate '$rate'"
	return
    }

    if {$check == ""} {
	return 0
    }

    if {![ string is double $check ]} {
	set state disabled
	$entry config -bg $color
	return 0
    }

    set toobig 10
    if {$rate == "biasra" && [ expr {abs($userBiasRA) > $toobig} ]} {
	set userBiasRA ""
	set result [ MessBox "Error" "Bias rate exceeds maximum of $toobig arcsec/s" ok ]
	return 0
    }
    if {$rate == "biasdec" && [ expr {abs($userBiasDEC) > $toobig} ]} {
	set userBiasDEC ""
	set result [ MessBox "Error" "Bias rate exceeds maximum of $toobig arcsec/s" ok ]
	return 0
    }
    

    set state normal
    $entry config -bg $ebg
    
    return 1
}
################################################################################
proc GetRate { rate } {

    if {$rate == "guide"} {

	set result [ SendCommand "PADGUIDE" ]

    } elseif {$rate == "drift"} {

	set result [ SendCommand "PADDRIFT" ]

    } elseif {$rate == "biasra"} {

	set xra [ SendCommand "XRA" ]
	set result [ lindex $xra 6 ]

    } elseif {$rate == "biasdec"} {

	set xdec [ SendCommand "XDEC" ]
	set result [ lindex $xdec 6 ]
    }

    return $result
}
################################################################################
proc SetPaddle {} {

    global gPaddleState top gTCSConnected gTCSDisabled

    # need to meet two conditions in order to enable paddle: connected to TCS and drives enabled.
    # Check to see if our state is correct:

    if {($gPaddleState && ($gTCSConnected && !$gTCSDisabled)) || (!$gPaddleState && (!$gTCSConnected || $gTCSDisabled))} {
	return
    }

    if {$gTCSConnected && !$gTCSDisabled} {
	set state "ON"
	set state2 normal
	set gPaddleState 1
    } else {
	set state "OFF"
	set state2 disabled
	set gPaddleState 0
    }

    # send the command to the TCS: will be either PADDLE ON or PADDLE OFF

    if {$gTCSConnected} {

	SendCommand "PADDLE" $state
    }

    # lock or unlock the rest of the paddle widgets:
    set widgetlist "rguide rdrift eguide edrift lbira ebira lbidec ebidec bbias lcorr ldome bdomel bdomer bteln btels btele btelw ltel lfoc bfocdn bfocup lcbira lcbidec lcguide lcdrift"

    set pad $top.pad

    foreach w $widgetlist {

	$pad.$w config -state $state2
    }

    # take care of the button bindings explicitly, since disabling doesn't seem to:

    if {$state2 == "normal"} {
	# directions:
	bind $pad.bteln <ButtonPress>    { PadTel "NORTH" }
	bind $pad.btels <ButtonPress>    { PadTel "SOUTH" }
	bind $pad.btele <ButtonPress>    { PadTel "EAST" }
	bind $pad.btelw <ButtonPress>    { PadTel "WEST" }
	bind $pad.bteln <ButtonRelease>  { PadTel "STOP" }
	bind $pad.btels <ButtonRelease>  { PadTel "STOP" }
	bind $pad.btele <ButtonRelease>  { PadTel "STOP" }
	bind $pad.btelw <ButtonRelease>  { PadTel "STOP" }
	# dome:
	bind $pad.bdomel <ButtonPress>   { PadDome "LEFT" }
	bind $pad.bdomer <ButtonPress>   { PadDome "RIGHT" }
	bind $pad.bdomel <ButtonRelease> { PadDome "STOP" }
	bind $pad.bdomer <ButtonRelease> { PadDome "STOP" }
	# focus:
	bind $pad.bfocup <ButtonPress>   { PadFoc "FOCUP" }
	bind $pad.bfocdn <ButtonPress>   { PadFoc "FOCDN" }
	bind $pad.bfocup <ButtonRelease> { PadFoc "FOCSTOP" }
	bind $pad.bfocdn <ButtonRelease> { PadFoc "FOCSTOP" }
    } else {
	# directions:
	bind $pad.bteln <ButtonPress>    { NullRoutine }
	bind $pad.btels <ButtonPress>    { NullRoutine }
	bind $pad.btele <ButtonPress>    { NullRoutine }
	bind $pad.btelw <ButtonPress>    { NullRoutine }
	bind $pad.bteln <ButtonRelease>  { NullRoutine }
	bind $pad.btels <ButtonRelease>  { NullRoutine }
	bind $pad.btele <ButtonRelease>  { NullRoutine }
	bind $pad.btelw <ButtonRelease>  { NullRoutine }
	# dome:
	bind $pad.bdomel <ButtonPress>   { NullRoutine }
	bind $pad.bdomer <ButtonPress>   { NullRoutine }
	bind $pad.bdomel <ButtonRelease> { NullRoutine }
	bind $pad.bdomer <ButtonRelease> { NullRoutine }
	# focus:
	bind $pad.bfocup <ButtonPress>   { NullRoutine }
	bind $pad.bfocdn <ButtonPress>   { NullRoutine }
	bind $pad.bfocup <ButtonRelease> { NullRoutine }
	bind $pad.bfocdn <ButtonRelease> { NullRoutine }
    }
}
################################################################################
proc ToggleBias {} {

    global top userBiasRA userBiasDEC currentBiasRA currentBiasDEC

    set pad $top.pad

    # first check to see if bias rates are on or off:

    set biascheck [ GetCorrection "b" ]

    # bias is on:
    if {$biascheck} {

	set result [ SendCommand "BIAS" "OFF" ]

	if {$result == "OK"} {

	    $pad.bbias configure -text "Apply Bias"
	    ColorButton $pad.bbias
	    $pad.lcbira config -state normal
	    $pad.lcbidec config -state normal
	    
	}
	return
    }

    # else we're commanded to turn the bias on.  First check to see if bias values are realistic:

    if {$userBiasRA == ""} {
	set userBiasRA $currentBiasRA
    }

    if {$userBiasDEC == ""} {
	set userBiasDEC $currentBiasDEC
    }

    if {$userBiasRA == "" || $userBiasDEC == "" || ![ CheckRate "biasra" ] || ![ CheckRate "biasdec" ]} {

	MessBox "Error" "Bias rates are invalid." ok
	return
    }

    set toobig 10

    if {[ expr {abs($userBiasRA) > $toobig} ] || [ expr {abs($userBiasDEC) > $toobig} ]} {

	set result [ MessBox "Error" "Bias rates exceed maximum of $toobig arcsec/s" ok ]
	return
    }

    set result [ MessBox "Bias Rates" "Are you sure you want to apply these bias rates?\n\nRA $userBiasRA arcsec/s\nDec $userBiasDEC arcsec/s" yesno ]
    if {$result == "NO"} {
	return "CANCELLED"
    }
    
    # passed all the tests, apply the bias:

    set result [ SendCommand "BIASRA" $userBiasRA ]
    set result [ SendCommand "BIASDEC" $userBiasDEC ]
    set result [ SendCommand "BIAS" "ON" ]

    # if successful, color the button, change the text and disable the rate entry windows:
    if {$result == "OK"} {

	$pad.bbias configure -text "STOP BIAS" -state normal
	$pad.lcbira config -state active
	$pad.lcbidec config -state active
    }
}
################################################################################
proc SetFocusSpeed { speed } {
# if speed is FAST, focspeed will be set to FAST, otherwise will be set to SLOW.
# don't enforce anything here, just pass it on to the TCS

    SendCommand "FOCSPEED" $speed
}
################################################################################
proc ColorButton { button } {
# colorButton sets default "enabled" active and passive foreground and background colors

    set bg darkblue
    set abg black
    set fg white
    set afg white
    set dfg grey35

    set relief raised

    set hc white
    set ht 3

    $button config -foreground $fg -background $bg -activeforeground $afg -activebackground $abg -disabledforeground $dfg -relief $relief -highlightcolor $hc -highlightthickness $ht

}
################################################################################
proc InitializeGUI {} {

    # declare globals:

    global currentEpoch radarDisplayed paddleDisplayed gPaddleState gTCSConnected gTCSDisabled
    global LOCATION SIMULATION verbose top pi version refreshrate tcswait gobslat gobslong
    global motionRA motionDEC motionFoc motionDome rpixm1 rpixm2

    global userGuide userDrift gGuideDrift userBiasRA userBiasDEC
    global cataloglist catalogindex currentcatalog

    global domewidth halfwidth

    set pi [ expr {2*asin(1.0)} ]
    set gTCSConnected 0

    # set initial state for GUI:
    set currentEpoch "2000.00"

    # don't display radar or paddle by default:
    set radarDisplayed 0
    set paddleDisplayed 0

    # paddle on by default:
    set gPaddleState 1

    if {$LOCATION == "V06"} {
	set gobslat "32.41693"
	set gobslong "110.73269"
	set m1diameter 1.54
	set m2diameter 0.33
	set telfov 0.25
	set domeradius 6.604
	# maxradius is the furthest the primary (or corrector) can be from the dome slit
	set maxradius $domeradius
	set maxradius 12
	set shutterwidth 3.048
    } elseif {$LOCATION == "703"} {
	set gobslat "32.41676"
	set gobslong "110.73257"
	set m1diameter 0.7366
	set m2diameter 0.25
	set telfov 6.2
	set domeradius 3.429
	# for a schmidt, measure max distance from corrector to dome - about 7.5 feet:
	set maxradius 2.286
	set shutterwidth 2.286
    } elseif {$LOCATION == "G96"} {
	set gobslat "32.44279"
	set gobslong "110.78877"
	set m1diameter 1.52
	set m2diameter 0.48
	set telfov 3.2
	set domeradius 5.334
	set maxradius $domeradius
	set shutterwidth 2.4384
    } elseif {$LOCATION == "E12"} {
	set gobslat "-31.2666"
	set gobslong "-149.0833"
	set m1diameter 0.5
	set m2diameter 0.2
	set telfov 2.9
	set domeradius 3.5
	set maxradius $domeradius
	set shutterwidth 2
    } elseif {$LOCATION == "I52"} {
	set gobslat "32.44258"
	set gobslong "110.78892"
	set m1diameter 1.0
	set m2diameter 0.4
	set telfov 0.6
	set domeradius 3.429
	set maxradius $domeradius
	set shutterwidth 2.286
    } elseif {$LOCATION == "21-IN"} {
	set gobslat "32.44275"
	set gobslong "110.7887"	
	set m1diameter 0.5
	set m2diameter 0.2
	set telfov 0.6
	set domeradius 3.5
	set maxradius $domeradius
	set shutterwidth 2.5
#	set m1diameter 1
#	set m2diameter 0.2
#	set telfov 0.0
#	set domeradius 3.5
#	set maxradius $domeradius
#	set shutterwidth 1.25
    }
    
    # globals for radar screen:
    global rwidth rheight rcentx rcenty rborder rscale dscale
    global rbackcolor rtextcolor relevcolor rtelcolor rlimitcolor rdomecolor rtelsize
    
    global verbose gTCSbusy gTCSport gTCSConnected gLatency

    set gTCSbusy 0
    set gTCSport 5750
    set gLatency(count) 0
    set gLatency(calibrated) 0
    set gLatency(repeat) 20

    # size
    global tcswidth tcsheight
    set tcsheight 900
    set tcswidth [ expr {$tcsheight * 1.333} ]
    
    set rshare 0.40
    set rborder 0
    set rwidth [ expr {int($tcswidth * $rshare)} ]
    set rheight $rwidth
    set rcentx [ expr {$rwidth / 2} ]
    set rcenty [ expr {$rheight / 2} ]
    set rscale [ expr {180 / double($rwidth - (2 * $rborder))} ]
    set dscale $rscale
    set rbackcolor black
    set rtextcolor white
    set relevcolor forestgreen
    set rdomecolor lightgrey
    set rtelcolor lightgrey
    set rlimitcolor darkred

    # calculate shutter width, in degrees, for later plotting.
    set domewidth [ Rad2Deg [ expr {2 * asin(($shutterwidth/2.0)/$domeradius)} ] ]
    # halfwidth misnamed ever since de-centered domes got projected off the plot
    set halfwidth [ expr {$domewidth / 4.0} ]

    # telescope aperture, projected to dome accounting for FoV
    set telproject [ expr {$m1diameter + 2 * (sin([ Deg2Rad $telfov ] / 2.0) * $maxradius)} ]
    set pm1 [ expr {(180 * ($telproject / ($domeradius * 2))) / $rscale} ]
    set pm2 [ expr {$pm1 * ($m2diameter / $m1diameter)} ]
   
    # diameter in pixels of m1 and m2, for later plotting
    set rpixm1 [ expr {int($pm1)} ]
    set rpixm2 [ expr {int($pm2)} ]

    # globals for paddle pane
    global pshare pwidth pheight pbackcolor ptextcolor
    global gFocusSpeed

    set userDrift ""
    set userGuide ""
    set gGuideDrift "guide"

    set userBiasRA  ""
    set userBiasDEC ""

    set motionRA 0
    set motionDEC 0
    set motionFoc 0
    set motionDome 0

    set gFocusSpeed "fast"

    set pwidth $rwidth
    set pheight [ expr {$tcsheight - $rheight} ]
    set pbackcolor grey25
    set ptextcolor black
    
    # make it obvious if we're in simulation mode - color the background a different color:
    if {$SIMULATION} {
	set pbackcolor grey25
    }

    # globals for entry pane:
    global ewidth eheight ebackcolor etextcolor oncolor offcolor offcolor movecolor alarmtext
    
    set ewidth [ expr {$tcswidth - $rwidth} ]
    set eheight $tcsheight
    set ebackcolor $pbackcolor
    set etextcolor white
    set oncolor green
    set offcolor red
    set alarmtext white
    set movecolor yellow

    set cataloglist [ glob -nocomplain ./*.cat ~/catalina/fieldlist.$LOCATION* ]
    set catalogindex [ llength $cataloglist ]
    set currentcatalog [ string toupper [ file rootname [ file tail [ lindex $cataloglist $catalogindex ] ] ] ]

    set ind [ string first "." $currentcatalog ]
    if {$ind != -1} {
	set currentcatalog [ string range $currentcatalog [ expr {1 + $ind} ] end ]
    }
}
################################################################################
proc GetTopTel {} {
    
    global top

    return $top.telescope
}
################################################################################
proc GetTopEng {} {
    
    global top

    return $top.engineering
}
################################################################################
proc BuildEntry {} {

    global top engineering SIMULATION fast
    global ewidth eheight ebackcolor etextcolor oncolor offcolor alarmtext movecolor

    global userRA userDEC userEpoch userAz userEl userDome userCommand tcsReply
    global currentRA currentDEC currentEpoch currentAZ currentEL currentDome currentFocus
    global currentLST currentUTtime currentDate currentLHA currentAirmass
    global gMovemode gListSource gLimitType 

    # initial state for the GUI:

    set gMovemode "azel"
    set userEpoch $currentEpoch
    set userRA ""
    set userDEC ""
    set userDome ""
    set userFocus ""

    set lbg $ebackcolor
    set lbg2 grey10
    set efg $etextcolor
    set fg black
    set fg2 white
    set fg3 grey75
    set ebg white
    set edbg grey50
    set relief "ridge"
    set relief2 "solid"

    set warncolor orange

    set tel $top.telescope
    set eng $top.engineering

    frame $tel -width $ewidth -height $eheight -background $ebackcolor
    frame $eng -width $ewidth -height $eheight -background $ebackcolor

    set lh 2

    set ew 11
    set eh 2
    set lw 12
    set bw 10

    set px 2
    set py 3
    set ipy 3
    set ipy2 8

    # highlight color and thickness:
    set hc white
    set hc2 black
    set ht 3

    # time info:
    # would be nice to include time of and time to next twilight (countdown).
    label  $tel.llst   -width $lw -bg $lbg -fg $fg3 -text "LST"
    label  $tel.lha    -width $lw -bg $lbg -fg $fg3 -text "HA"
    label  $tel.lut    -width $lw -bg $lbg -fg $fg3 -text "UT"
    label  $tel.lloc   -width $lw -bg $lbg -fg $fg3 -text "Local"
    label  $tel.ldate  -width $lw -bg $lbg -fg $fg3 -text "Date"

    label  $tel.lclst  -width $lw -bg $lbg2 -fg $fg2 -textvariable currentLST -relief $relief
    label  $tel.lcha   -width $lw -bg $lbg2 -fg $fg2 -textvariable currentLHA -relief $relief
    label  $tel.lcut   -width $lw -bg $lbg2 -fg $fg2 -textvariable currentUTtime -relief $relief
    label  $tel.lcdate -width $lw -bg $lbg2 -fg $fg2 -textvariable currentDate -relief $relief
    label  $tel.lrname -width $lw -bg $lbg  -fg $fg3 -text ""
    label  $tel.laname -width $lw -bg $lbg  -fg $fg3 -text ""

    # telescope coordinate entry:
    radiobutton $tel.rradec -bg $lbg -fg $fg2 -text "RA/Dec" -variable gMovemode -value "radec" -command { ToggleMoveMode } -activebackground $lbg -highlightthickness $ht -highlightbackground $lbg -highlightcolor $hc -selectcolor $lbg2 -activeforeground $fg2
    radiobutton $tel.razel  -bg $lbg -fg $fg2 -text "El/Az  " -variable gMovemode -value "azel" -command { ToggleMoveMode } -activebackground $lbg -highlightthickness $ht -highlightbackground $lbg -highlightcolor $hc -selectcolor $lbg2 -activeforeground $fg2

    # disabled state makes radiobutton text invisible

    label  $tel.lra   -width $lw -bg $lbg -fg $fg3 -text "RA"
    label  $tel.lcra  -width $lw -bg $lbg2 -fg $fg2 -textvariable currentRA -activebackground $movecolor -relief $relief
    entry  $tel.era   -width $ew -textvariable userRA -fg $fg -bg $ebg -disabledbackground $edbg -justify center -highlightcolor $hc -highlightthickness $ht -highlightbackground $lbg -relief $relief2
    label  $tel.ldec  -width $lw -bg $lbg -fg $fg3 -fg $fg3 -text "Dec"
    label  $tel.lcdec -width $lw -bg $lbg2 -fg $fg2 -textvariable currentDEC -activebackground $movecolor -relief $relief
    entry  $tel.edec  -width $ew -textvariable userDEC -fg $fg -bg $ebg -disabledbackground $edbg -justify center -highlightcolor $hc -highlightthickness $ht -highlightbackground $lbg -relief $relief2
    label  $tel.lepo  -width $lw -bg $lbg -fg $fg3 -text "Epoch"
    label  $tel.lcepo -width $lw -bg $lbg -fg $fg2 -textvariable currentEpoch -relief $relief2
    label  $tel.lel   -width $lw -bg $lbg -fg $fg3 -text "Elevation"
    label  $tel.lcel  -width $lw -bg $lbg2 -fg $fg2 -textvariable currentEL -activebackground $movecolor -relief $relief
    entry  $tel.eel   -width $ew -textvariable userEl -fg $fg -bg $ebg -disabledbackground $edbg -justify center -highlightcolor $hc -highlightthickness $ht -highlightbackground $lbg -relief $relief2
    label  $tel.laz   -width $lw -bg $lbg -fg $fg3 -text "Azimuth"
    label  $tel.lcaz  -width $lw -bg $lbg2 -fg $fg2 -textvariable currentAZ -activebackground $movecolor -relief $relief
    entry  $tel.eaz   -width $ew -textvariable userAz -fg $fg -bg $ebg -disabledbackground $edbg -justify center -highlightcolor $hc -highlightthickness $ht -highlightbackground $lbg -relief $relief2
    label  $tel.lam   -width $lw -bg $lbg -fg $fg3 -text "Airmass"
    label  $tel.lcam  -width $lw -bg $lbg -fg $fg2 -textvariable currentAirmass -relief $relief2 -activebackground $warncolor
 
    bind   $tel.era  <Return> { TelescopeSlew }
    bind   $tel.edec <Return> { TelescopeSlew }
    bind   $tel.eaz  <Return> { TelescopeSlew }
    bind   $tel.eel  <Return> { TelescopeSlew }
    bind   $tel.era  <KP_Enter> { TelescopeSlew }
    bind   $tel.edec <KP_Enter> { TelescopeSlew }
    bind   $tel.eaz  <KP_Enter> { TelescopeSlew }
    bind   $tel.eel  <KP_Enter> { TelescopeSlew }
    bind   $tel.era  <Escape> { set userRA "" }
    bind   $tel.edec <Escape> { set userDEC "" }
    bind   $tel.eaz  <Escape> { set userAz "" }
    bind   $tel.eel  <Escape> { set userEl "" }

    # limit warnings:
    label  $tel.llim  -width $lw -bg $lbg -fg $fg -text "" -activebackground $offcolor -activeforeground $alarmtext -state disabled

    # slew/stop buttons:
    button $tel.bslew -width $bw -text "SLEW" -command { TelescopeSlew } -highlightbackground $lbg
    button $tel.bstop -width $bw -text "STOP" -command { TelescopeStop } -highlightbackground $lbg
    ColorButton $tel.bslew
    ColorButton $tel.bstop

    bind $tel.bslew <Return>   { TelescopeSlew }
    bind $tel.bstop <Return>   { TelescopeStop }
    bind $tel.bslew <KP_Enter> { TelescopeSlew }
    bind $tel.bstop <KP_Enter> { TelescopeStop }

    # Dome and Focus movements
    label  $tel.ldome  -width $lw -bg $lbg -fg $fg3 -text "Dome Az."
    label  $tel.lcdome -width $lw -bg $lbg2 -fg $fg2 -textvariable currentDome -activebackground $movecolor -relief $relief
    button $tel.bdome  -width $bw -text "Rotate" -command { DomeRotate } -highlightbackground $lbg
    entry  $tel.edome  -width $ew -textvariable userDome -fg $fg -bg $ebg -disabledbackground $edbg -justify center -highlightcolor $hc -highlightthickness $ht -highlightbackground $lbg -relief $relief2
    ColorButton $tel.bdome

    bind $tel.bdome <Return>   { DomeRotate }
    bind $tel.bdome <KP_Enter> { DomeRotate }
    bind $tel.edome <Return>   { DomeRotate }
    bind $tel.edome <KP_Enter> { DomeRotate }
    bind $tel.edome <Escape>   { set userDome "" }

    label  $tel.lfoc   -width $lw -text "Focus" -bg $lbg -fg $fg3
    label  $tel.lcfoc  -width $lw -bg $lbg2 -fg $fg2 -textvariable currentFocus -activebackground $movecolor -relief $relief
    button $tel.bfoc   -width $bw -text "Focus" -command { FocusAdjust } -highlightbackground $lbg
    entry  $tel.efoc   -width $ew -textvariable userFocus -fg $fg -bg $ebg -disabledbackground $edbg -justify center -highlightcolor $hc -highlightthickness $ht -highlightbackground $lbg -relief $relief2
    ColorButton $tel.bfoc
    
    bind $tel.bfoc <Return>   { FocusAdjust }
    bind $tel.bfoc <KP_Enter> { FocusAdjust }
    bind $tel.efoc <Return>   { FocusAdjust }
    bind $tel.efoc <KP_Enter> { FocusAdjust }
    bind $tel.efoc <Escape>   { set userFocus "" }

    # disable:
    button $tel.bdis  -width $bw -text "ENABLE" -command { ToggleDisable } -highlightbackground $lbg
    label  $tel.ldis  -width $lw -text "" -bg $lbg -fg $oncolor -activeforeground $alarmtext -activebackground $offcolor
    ColorButton $tel.bdis

    bind $tel.bdis <Return>   { ToggleDisable }
    bind $tel.bdis <KP_Enter> { ToggleDisable }

    # tracking, PEC and AutoDome
    button $tel.btrack -width $bw -text "Tracking" -command { ToggleTracking } -highlightbackground $lbg
    label  $tel.ltrack -width $lw -text "" -bg $lbg -fg $oncolor -activeforeground $alarmtext -activebackground $offcolor
    ColorButton $tel.btrack

    bind $tel.btrack <Return>   { ToggleTracking }
    bind $tel.btrack <KP_Enter> { ToggleTracking }

    button $tel.bpec   -width $bw -text "PEC"  -command { TogglePEC } -highlightbackground $lbg
    label  $tel.lpec   -width $lw -text "" -bg $lbg -fg $oncolor -activeforeground $alarmtext -activebackground $offcolor
    ColorButton $tel.bpec

    bind $tel.bpec <Return>   { TogglePEC }
    bind $tel.bpec <KP_Enter> { TogglePEC }

    button $tel.bauto  -width $bw -text "AutoDome"  -command { ToggleAutoDome } -highlightbackground $lbg
    label  $tel.lauto  -width $lw -text "" -bg $lbg -fg $oncolor -activeforeground $alarmtext -activebackground $offcolor
    ColorButton $tel.bauto

    bind $tel.bauto <Return>   { ToggleAutoDome }
    bind $tel.bauto <KP_Enter> { ToggleAutoDome }

    # dome init, stop dome, tel init and focus zero:
    button $tel.bdini  -width $bw -text "Dome Init" -command { DomeInit } -highlightbackground $lbg
    ColorButton $tel.bdini
    button $tel.bstow -width $bw -text "STOW" -command { TelescopeStow } -highlightbackground $lbg
    ColorButton $tel.bstow
    button $tel.binit  -width $bw -text "Telescope Init" -command { TelescopeInit } -highlightbackground $lbg
    ColorButton $tel.binit
    button $tel.bdstop -width $bw -text "Stop Dome" -command { DomeStop } -highlightbackground $lbg
    ColorButton $tel.bdstop

    bind $tel.bdini <Return>   { DomeInit }
    bind $tel.bdini <KP_Enter> { DomeInit }
    bind $tel.binit <Return>   { TelescopeInit }
    bind $tel.binit <KP_Enter> { TelescopeInit }
    bind $tel.bstow <Return>   { TelescopeStow }
    bind $tel.bstow <KP_Enter> { TelescopeStow }
    bind $tel.bdstop <Return>   { DomeStop }
    bind $tel.bdstop <KP_Enter> { DomeStop }

    # blank labels:
    label  $tel.lbla1 -width $lw -bg $lbg -fg $fg -text ""
    label  $tel.lbla2 -width $lw -bg $lbg -fg $fg -text ""
    label  $tel.lbla3 -width $lw -bg $lbg -fg $fg -text ""
    label  $tel.lbla4 -width $lw -bg $lbg -fg $fg -text ""

    # listbox for catalog/preset positions:
    set lbox "$tel.list"
    frame $lbox -bg $lbg
    label $lbox.head -bg $lbg -fg $fg -text "" -font courier
    listbox $lbox.text -width 40 -height 6 -borderwidth 1 -fg black -bg grey75 -yscrollcommand "$lbox.scroll set" -selectmode single -font courier
    scrollbar $lbox.scroll -orient vertical -command "$lbox.text yview" -borderwidth 1

    bind $lbox.text <Double-Button-1> { ListSelectTarget }

    button $tel.bcat -width $bw -text "CATALOG" -command { ChooseCatalog } -highlightcolor $hc -highlightthickness $ht -highlightbackground $lbg
    ColorButton $tel.bcat
    label  $tel.lcat -width $lw -bg $lbg -fg $fg2 -relief solid -text "NONE"

    button $tel.bload -width $bw -text "APPLY" -command { ListSelectTarget } -highlightcolor $hc -highlightthickness $ht -highlightbackground $lbg
    ColorButton $tel.bload

    bind $tel.bcat  <Return>   { ChooseCatalog }
    bind $tel.bcat  <KP_Enter> { ChooseCatalog }

    pack $lbox.head   -side top
    pack $lbox.text   -side left
    pack $lbox.scroll -side right -expand yes -fill both
    
    # simulation warning:
    if {$fast} {
	set text "***  FAST SIMULATION  ***"
    } else {
	set text "***  SIMULATION MODE  ***"
    }
    label  $tel.lsim   -width [ expr {$lw * 2} ] -bg blue -fg $alarmtext -text $text

    label  $tel.ltele2  -width [expr {int(2*$lw + 2)}] -bg $lbg -fg $oncolor -text "TCS DISCONNECTED" -activebackground $offcolor -activeforeground $alarmtext -state active
    label  $tel.ltelem -width $lw -text "Telemetry" -bg $lbg -fg $fg3 
    button $tel.btelem -width $bw -text "CONNECT" -command { ToggleTelemetryGUI } -highlightbackground $lbg
    ColorButton $tel.btelem

    bind $tel.btelem <Return>   { ToggleTelemetryGUI }
    bind $tel.btelem <KP_Enter> { ToggleTelemetryGUI }

    # now arrange everything in a nice 4-column grid:

    # times:
    set row 0
    grid config $tel.ldate  -column 0 -row $row -padx $px -pady $py
    grid config $tel.lut    -column 1 -row $row -padx $px -pady $py
    grid config $tel.llst   -column 2 -row $row -padx $px -pady $py
    grid config $tel.lha    -column 3 -row $row -padx $px -pady $py

    incr row
    grid config $tel.lcdate -column 0 -row $row -padx $px -pady $py -ipady $ipy2
    grid config $tel.lcut   -column 1 -row $row -padx $px -pady $py -ipady $ipy2
    grid config $tel.lclst  -column 2 -row $row -padx $px -pady $py -ipady $ipy2
    grid config $tel.lcha   -column 3 -row $row -padx $px -pady $py -ipady $ipy2

    # ra, dec:
    incr row
    grid config $tel.lra    -column 1 -row $row -padx $px
    grid config $tel.ldec   -column 2 -row $row -padx $px
    grid config $tel.lepo   -column 3 -row $row -padx $px

    incr row
    grid config $tel.rradec -column 0 -row $row -padx $px -pady $py
    grid config $tel.lcra   -column 1 -row $row -padx $px -pady $py -ipady $ipy2
    grid config $tel.lcdec  -column 2 -row $row -padx $px -pady $py -ipady $ipy2
    grid config $tel.lcepo  -column 3 -row $row -padx $px -pady $py -ipady $ipy2

    incr row
    grid config $tel.lrname -column 0 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.era    -column 1 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.edec   -column 2 -row $row -padx $px -pady $py -ipady $ipy

    # az, el:
    incr row
    grid config $tel.laz    -column 2 -row $row -padx $px
    grid config $tel.lel    -column 1 -row $row -padx $px
    grid config $tel.lam    -column 3 -row $row -padx $px
    incr row
    grid config $tel.razel  -column 0 -row $row -padx $px -pady $py
    grid config $tel.lcaz   -column 2 -row $row -padx $px -pady $py -ipady $ipy2
    grid config $tel.lcel   -column 1 -row $row -padx $px -pady $py -ipady $ipy2
    grid config $tel.lcam   -column 3 -row $row -padx $px -pady $py -ipady $ipy2
    incr row
    grid config $tel.laname -column 0 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.eaz    -column 2 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.eel    -column 1 -row $row -padx $px -pady $py -ipady $ipy

    # limits:
    incr row
    grid config $tel.llim   -column 0 -row $row -padx $px -columnspan 4 -ipady $ipy

    # slew and stop:
    incr row
    grid config $tel.bslew  -column 1 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.bstop  -column 2 -row $row -padx $px -pady $py -ipady $ipy

    incr row
    # simulation warning if simulated, otherwise blank line:
    if {$SIMULATION} {
	grid config $tel.lsim   -column 1 -row $row -padx $px -pady $py -ipady $ipy -columnspan 2
    } else {
	grid config $tel.lbla1  -column 0 -row $row -padx $px -pady $py -ipady $ipy
    }
    # dome control and status:
    incr row
    grid config $tel.ldome  -column 0 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.bdome  -column 1 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.edome  -column 2 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.lcdome -column 3 -row $row -padx $px -pady $py -ipady $ipy2

    # focus control and status:
    incr row
    grid config $tel.lfoc   -column 0 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.bfoc   -column 1 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.efoc   -column 2 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.lcfoc  -column 3 -row $row -padx $px -pady $py -ipady $ipy2

    # blank line:
    incr row
    grid config $tel.lbla2  -column 0 -row $row -padx $px

    # toggle buttons:
    incr row
    grid config $tel.bdis   -column 0 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.btrack -column 1 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.bpec   -column 2 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.bauto  -column 3 -row $row -padx $px -pady $py -ipady $ipy

    # toggle button status labels:
    incr row
    grid config $tel.ldis   -column 0 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.ltrack -column 1 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.lpec   -column 2 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.lauto  -column 3 -row $row -padx $px -pady $py -ipady $ipy

    # dome init and focus zero:
    incr row
    grid config $tel.binit  -column 0 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.bdini  -column 1 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.bstow  -column 2 -row $row -padx $px -pady $py -ipady $ipy
    grid config $tel.bdstop -column 3 -row $row -padx $px -pady $py -ipady $ipy

    # listbox
    incr row
    grid config $lbox       -column 0 -row $row -padx $px -pady $py -columnspan 4

    incr row

    grid config $tel.bcat  -column 0 -row $row -padx $px -pady $py -ipady $ipy -columnspan 2
    grid config $tel.lcat  -column 1 -row $row -padx $px -pady $py -ipady $ipy -columnspan 2
    grid config $tel.bload  -column 2 -row $row -padx $px -pady $py -ipady $ipy -columnspan 2

    incr row
    grid config $tel.ltelem -column 0 -row $row -padx $px -pady $py
    grid config $tel.btelem -column 1 -row $row -padx $px -pady $py -ipady $ipy -ipady $ipy
    grid config $tel.ltele2 -column 2 -row $row -padx $px -pady $py -columnspan 2

    # for engineering, include a command line to the TCS
    if {$engineering} {

	label   $eng.lcom   -text "TCS Direct Input:" -fg $fg3 -bg $lbg
	entry   $eng.ecom   -width [ expr {int (6.5 * $ew)} ] -textvariable userCommand -fg $fg -bg $ebg -disabledbackground $edbg -highlightcolor $hc -highlightthickness $ht -highlightbackground $lbg -relief $relief2
	text   $eng.lfeed -width [ expr {int(6.75 * $ew)} ] -height 2 -fg $fg -bg grey75 -relief $relief -state disabled -wrap word -font {-family helvetica -weight normal -size 15}

	$eng.lfeed configure -state disabled

	bind    $eng.ecom <Return> SendCommandLine
	bind    $eng.ecom <KP_Enter> SendCommandLine
	bind    $eng.ecom <Escape> { set userCommand "" }

	incr row
	grid config $eng.lcom   -column 0 -row $row -padx {10 0} -pady $py
	grid config $eng.ecom   -column 1 -row $row -padx $px -columnspan 1

	incr row
	grid config $eng.lfeed  -column 0 -row $row -padx 20 -pady 10 -columnspan 2
    }

    # need to figure out how to un-bind the space bar from executing button commands - this should be <Return>
    if {$engineering} {
	pack $eng -side bottom -fill x
    }
    pack $tel

    return $tel
}
################################################################################
proc KeyboardTraversal {} {
# coding an intuitive keyboard traversal turns out to be excruciatingly explicit and lengthy...

    global top engineering gTCSConnected

    set tel $top.telescope
    set eng $top.engineering

    # button states for those that changes states:

    set pecstate     [ $tel.bpec   cget -state ]
    set trackstate   [ $tel.btrack cget -state ]
    set slewstate    [ $tel.bslew  cget -state ]
    set elazstate    [ $tel.eaz    cget -state ]
    set radecstate   [ $tel.era    cget -state ]
    set domestate    [ $tel.edome  cget -state ]
    if {$gTCSConnected} {
	set disabstate   [ SendCommand "DISABSTAT" ]
    } else {
	set disabstate 1
    }
    set catalogstate disabled

    # buttons which are always normal: enable, autodome, focus, dome init, focus zero, disconnect

    bind $tel <Button-1> { focus [ GetTopTel ].btelem }

    # ra/dec az/el radiobuttons
    if {$radecstate == "disabled"} {
	bind $tel.rradec <Right> { focus [ GetTopTel ].eaz }
    } else {
	bind $tel.rradec <Right> { focus [ GetTopTel ].era }
    }
    bind $tel.rradec <Down>  { focus [ GetTopTel ].razel }

    bind $tel.razel  <Up>    { focus [ GetTopTel ].rradec }
    if {$elazstate == "disabled"} {
	bind $tel.razel  <Down>  { focus [ GetTopTel ].bslew }
    } else {
	bind $tel.razel  <Down>  { focus [ GetTopTel ].eel }
    }
    if {$elazstate == "disabled"} {
	bind $tel.razel  <Right> { focus [ GetTopTel ].era }
    } else {
	bind $tel.razel  <Right> { focus [ GetTopTel ].eel }
    }

    # ra entry
    bind $tel.era   <Right> { focus [ GetTopTel ].edec }
    bind $tel.era   <Down>  { focus [ GetTopTel ].bslew }
    bind $tel.era   <Left>  { focus [ GetTopTel ].rradec }
    # dec entry
    bind $tel.edec  <Left>  { focus [ GetTopTel ].era }
    bind $tel.edec  <Down>  { focus [ GetTopTel ].bstop }
    # az entry
    bind $tel.eaz   <Down>  { focus [ GetTopTel ].bstop }
    bind $tel.eaz   <Left>  { focus [ GetTopTel ].eel }
    # el entry
    bind $tel.eel   <Left>  { focus [ GetTopTel ].razel }
    bind $tel.eel   <Right> { focus [ GetTopTel ].eaz }
    bind $tel.eel   <Down>  { focus [ GetTopTel ].bslew }
    bind $tel.eel   <Up>    { focus [ GetTopTel ].razel }

    # slew button
    bind $tel.bslew <Left>  { focus [ GetTopTel ].razel }
    bind $tel.bslew <Right> { focus [ GetTopTel ].bstop }
    if {$elazstate == "disabled"} {
	bind $tel.bslew <Up>    { focus [ GetTopTel ].era }
    } else {
	bind $tel.bslew <Up>    { focus [ GetTopTel ].eel }
    }
    if {$domestate == "disabled"} {
	bind $tel.bslew <Down>  { focus [ GetTopTel ].bfoc }
    } else {
	bind $tel.bslew <Down>  { focus [ GetTopTel ].bdome }
    }

    # stop button
    bind $tel.bstop <Left>  { focus [ GetTopTel ].bslew }
    if {$elazstate == "disabled"} {
	bind $tel.bstop <Up>    { focus [ GetTopTel ].edec }
    } else {
	bind $tel.bstop <Up>    { focus [ GetTopTel ].eaz }
    }
    if {$domestate == "disabled"} {
	bind $tel.bstop <Down>  { focus [ GetTopTel ].efoc }
    } else {
	bind $tel.bstop <Down>  { focus [ GetTopTel ].edome }
    }

    # dome button
    if {$slewstate == "disabled"} {
	bind $tel.bdome <Up>    { NullRoutine }
    } else {
	bind $tel.bdome <Up>    { focus [ GetTopTel ].bslew }
    }
    bind $tel.bdome <Down>  { focus [ GetTopTel ].bfoc }
    bind $tel.bdome <Right> { focus [ GetTopTel ].edome }

    # dome entry
    if {$slewstate == "disabled"} {
	bind $tel.edome <Up>  { NullRoutine }
    } else {
	bind $tel.edome <Up>  { focus [ GetTopTel ].bstop }
    }
    bind $tel.edome <Down>  { focus [ GetTopTel ].efoc }
    bind $tel.edome <Left>  { focus [ GetTopTel ].bdome }
    
    # focus button
    if {$domestate == "disabled"} {
	if {$slewstate == "disabled"} {
	    bind $tel.bfoc  <Up>    { NullRoutine }
	} else {
	    bind $tel.bfoc  <Up>    { focus [ GetTopTel ].bslew }
	}
    } else {
	bind $tel.bfoc  <Up>    { focus [ GetTopTel ].bdome }	
    }
    if {$trackstate == "disabled"} {
	bind $tel.bfoc  <Down>  { focus [ GetTopTel ].bdis }
    } else {
	bind $tel.bfoc  <Down>  { focus [ GetTopTel ].btrack }
    }
    bind $tel.bfoc  <Right> { focus [ GetTopTel ].efoc }

    # focus entry
    if {$domestate == "disabled"} {
	if {$slewstate == "disabled"} {
	    bind $tel.efoc  <Up>    { NullRoutine }
	} else {
	    bind $tel.efoc  <Up>    { focus [ GetTopTel ].bstop }
	}
    } else {
	bind $tel.efoc  <Up>    { focus [ GetTopTel ].edome }
    }
    if {$trackstate == "disabled"} {
	bind $tel.efoc  <Down>  { focus [ GetTopTel ].bauto }
    } else {
	bind $tel.efoc  <Down>  { focus [ GetTopTel ].bpec }
    }
    bind $tel.efoc  <Left>  { focus [ GetTopTel ].bfoc }

    # DISABLE button: right, up, down
    bind $tel.bdis  <Right> { focus [ GetTopTel ].btrack }
    if {!$disabstate} {
	bind $tel.bdis   <Up>    { focus [ GetTopTel ].bfoc }
	bind $tel.bdis   <Down>  { focus [ GetTopTel ].binit }
    } else {
	bind $tel.bdis   <Up>    { NullRoutine }
	bind $tel.bdis   <Down>  { focus [ GetTopTel ].bcat }
    }

    # tracking button: left, right, up, down
    if {!$disabstate} {
	bind $tel.btrack <Up>    { focus [ GetTopTel ].bfoc }
	bind $tel.btrack <Down>  { focus [ GetTopTel ].bdini }
    } else {
	bind $tel.btrack <Up>    { NullRoutine }
	bind $tel.btrack <Down>  { focus [ GetTopTel ].bcat }
    }
    bind $tel.btrack <Left>  { focus [ GetTopTel ].bdis }
    bind $tel.btrack <Right> { focus [ GetTopTel ].bpec }

    # PEC button: left, right, up, down
    if {!$disabstate} {
	bind $tel.bpec   <Up>    { focus [ GetTopTel ].efoc }
	bind $tel.bpec   <Down>  { focus [ GetTopTel ].bstow }
    } else {
	bind $tel.bpec   <Up>    { NullRoutine }
	bind $tel.bpec   <Down>  { focus [ GetTopTel ].bload }
    }
    bind $tel.bpec   <Left>  { focus [ GetTopTel ].btrack }
    bind $tel.bpec   <Right> { focus [ GetTopTel ].bauto }

    # autodome button: left, up, down
    if {!$disabstate} {
	bind $tel.bauto  <Up>    { focus [ GetTopTel ].efoc }
	bind $tel.bauto  <Down>  { focus [ GetTopTel ].bdstop }
    } else {
	bind $tel.bauto  <Up>    { NullRoutine }
	bind $tel.bauto  <Down>  { focus [ GetTopTel ].bload }
    }
    bind $tel.bauto  <Left>  { focus [ GetTopTel ].bpec }

    # telescope init button:
    bind $tel.binit <Right> { focus [ GetTopTel ].bdini }
    bind $tel.binit <Up> { focus [ GetTopTel ].bdis }
    if {$trackstate == "disabled"} {
	bind $tel.binit <Down>  { focus [ GetTopTel ].btelem }
    } else {
	bind $tel.binit <Down>  { focus [ GetTopTel ].bcat }
    }

    # dome init button:
    if {$trackstate == "disabled"} {
	bind $tel.bdini <Up>  { focus [ GetTopTel ].bdis }
    } else {
	bind $tel.bdini <Up>  { focus [ GetTopTel ].btrack }
    }

    bind $tel.bdini <Right> { focus [ GetTopTel ].bstow }
    bind $tel.bdini <Left> { focus [ GetTopTel ].binit }
    bind $tel.bdini <Down> { focus [ GetTopTel ].binit }
    if {$trackstate == "disabled"} {
	bind $tel.bdini <Down>  { focus [ GetTopTel ].btelem }
    } else {
	bind $tel.bdini <Down>  { focus [ GetTopTel ].bcat }
    }

    # stow button:
    if {$pecstate == "disabled"} {
	bind $tel.bstow <Up>  { focus [ GetTopTel ].bauto }
    } else {
	bind $tel.bstow <Up>  { focus [ GetTopTel ].bpec }
    }
    bind $tel.bstow <Left>  { focus [ GetTopTel ].bdini }
    bind $tel.bstow <Right> { focus [ GetTopTel ].bdstop }
    bind $tel.bstow <Down>  { focus [ GetTopTel ].bload }

    # stop dome button:
    bind $tel.bdstop <Left> { focus [ GetTopTel ].bstow }
    bind $tel.bdstop <Up>   { focus [ GetTopTel ].bauto }
    bind $tel.bdstop <Down> { focus [ GetTopTel ].bload }

    # listbox:
#    bind $tel.list  <Up>    { focus [ GetTopTel ].bstow }
#    bind $tel.list  <Down>  { focus [ GetTopTel ].bload }

    # catalog toggle button:
    if {!$disabstate} {
	bind $tel.bcat  <Up>    { focus [ GetTopTel ].binit }
    } else {
	bind $tel.bcat  <Up>    { focus [ GetTopTel ].bdis }
    }

    bind $tel.bcat  <Down>  { focus [ GetTopTel ].btelem }
    bind $tel.bcat  <Right> { focus [ GetTopTel ].bload }

    # load/apply catalog button:
    if {!$disabstate} {
	bind $tel.bload <Up>    { focus [ GetTopTel ].bdstop }
    } else {
	bind $tel.bload <Up>    { focus [ GetTopTel ].bauto }
    }
    bind $tel.bload <Down>  { focus [ GetTopTel ].btelem }
    bind $tel.bload <Left>  { focus [ GetTopTel ].bcat }

    # telemetry toggle button:
    if {$gTCSConnected} {
	bind $tel.btelem <Up>   { focus [ GetTopTel ].bcat }
    } else {
	bind $tel.btelem <Up>   { NullRoutine }
    }

    if {$engineering} {
	if {$gTCSConnected} {
	    bind $tel.btelem <Down>  { focus [ GetTopEng ].ecom }
	} else {
	    bind $tel.btelem <Down>   { NullRoutine }
	}
	bind $eng.ecom   <Up>    { focus [ GetTopTel ].btelem }
    }
}
################################################################################
proc TelescopeStow {} {

    global userEl userAz gTCSmovestatus

    set result [ MessBox "Stow Telescope" "Do you really want to stow the telescope and dome?" yesno ]
    
    if {$result == "NO"} {
	return
    }

    set userEl ""
    set userAz ""

    # save out the focus value and move to stow:
    set result [ SendCommand "SYSSAVE" ]
    set result [ SendCommand "MOVSTOW" ]
    set result [ SendCommand "PEC" "OFF" ]
    set result [ SendCommand "DOME" "STOW" ]

    # telescope and dome are moving to stow.  wait until no motion and disable the telescope

    set settled 0

    while {$settled < 5} {

	after 500
	UpdateStatus

	if {$gTCSmovestatus == 0} {
	    incr settled
	} else {
	    set settled 0
	}
    }

    set result [ SendCommand "DISABLE" ]

    return $result
}
################################################################################
proc TelescopeSlew {} {

    global userRA userDEC userEpoch userAz userEl gMovemode
    global currentRA currentDEC currentAZ currentEL top

    if {$gMovemode == "radec"} {

	if {$userRA == "" || $userDEC == "" || $userEpoch == ""} {

	    MessBox "Error" "Enter an RA and Dec to slew." ok
	    return
	}

	# check to see if RA and Dec are well-constructed:
	set result [ ValidateCoords $userRA $userDEC $userEpoch ]
	if {$result != "OK"} {
	    return
	}

	# passed the parsing test, now see if they pass the limit test:
	set result [ SendCommand "VERIFY" "$userRA $userDEC $userEpoch" ]

	if {$result != 1} {
	    MessBox "Error" "Slew rejected: will exceed limits." ok
	    return
	}

	# have the GUI do a little extra work here: set the "next" RA and Dec positions to these values too
	# just in case we get an "initnext" command from AutoPoint

	set result [ SendCommand "NEXTPOS" "$userRA $userDEC $userEpoch 0.0 0.0" ] 

	# TCS MOVRADEC format: MOVRADEC HH:MM:SS.ss -DD:MM:SS.ss EEEE.eeee  rr.rrrrrr dd.dddddd
	# typically don't care about proper motions, so just send zeroes:
	set result [ SendCommand "MOVRADEC" "$userRA $userDEC $userEpoch 0.0 0.0" ]
    
	if {$result != "OK"} {
	    MessBox "Error" "Command 'MOVRADEC $userRA $userDEC $userEpoch 0.0 0.0' returned '$result'" ok
	}

	# erase any previous targets
	$top.telescope.lrname config -text ""
	$top.telescope.laname config -text ""

	return $result

    } elseif {$gMovemode == "azel"} {

	# if both az and el are blank, don't do anything
	if {$userAz == "" && $userEl == ""} {
	    return
	}

	# if either az or el is blank, assume no motion in that axis
	if {$userAz == ""} {
	    set userAz $currentAZ
	}
	if {$userEl == ""} {
	    set userEl $currentEL
	}

	if {![ string is double $userAz ] || $userAz < 0 || $userAz > 360} {

	    MessBox "Error" "Azimuth must be between 0 and 360." ok
	    set userAz ""
	    return
	}
	set lowlim 20
	if {![ string is double $userEl ] || $userEl < $lowlim || $userEl > 90} {

	    MessBox "Error" "Elevation must be between $lowlim and 90." ok
	    set userEl ""
	    return
	}

	# need to add some GUI smarts here: sending an ELAZ command to the telescope will shut off
	# the tracking, which in turn shuts off the autodome.  Check the tracking and autodome
	# state before sending the move command, and preserve the state afterward

	set trackcheck [ GetCorrection "t" ]

	# would be nice to be able to do a VERIFY here before sending the command, but that requires
	# knowledge of where the RA,Dec is that corresponds to the Az/El ... not something for the gui to calculate

	# passed the tests: move telescope in Az

	set args "$userEl $userAz"
	if {$trackcheck} {
	    append args " TRACKON"
	}
	
	set result [ SendCommand "ELAZ" $args ]

	# deal w/ PEC here too? or in ToggleTracking proc?

	if {$result != "OK"} {

	    if {$result == "LIMIT"} {
		MessBox "Error" "Slew rejected: will exceed limits." ok
	    } else {
		MessBox "Error" "Command to slew returned '$result'" ok
	    }
	}

	set return $result

    } else {

	set return "Proc TelescopeSlew: Unrecognized move mode '$gMovemode'"
    }

    # if we made it here, we sent a slew command.

    $top.telescope.lrname config -text ""
    $top.telescope.laname config -text ""

    return $return
}
################################################################################
proc TelescopeStop {} {

    # not sure if CANCEL stops all motion - there is a separate FOCSTOP command
    # to stop focus

    set result [ SendCommand "CANCEL" ]
    return $result
} 
################################################################################
proc TelescopeInit {} {

    global currentRA currentDEC gMovemode userAz userEl

    set timeout 5

    # give the user some options - init to an az, el position or the traditional current -> commanded

    set message "Do you want to initialize the telescope to an estimated Azimuth, Elevation position?"
    set initazel [ MessBox "Telescope Init" $message yesno ]
    if {$initazel == "YES"} {
	
	set gMovemode "azel"

	set message "Enter the estimated El/Az position of the telescope in the GUI.  Then click OK in this message box to initialize the telescope to those values."

	set doinitazel [ MessBox "Az, El init" $message okcancel ]

	if {$doinitazel == "OK"} {
	    
	    # check to make sure the az, el make sense:

	    if {![ string is double $userAz ] || $userAz < 0 || $userAz > 360} {
		
		MessBox "Error" "Azimuth must be between 0 and 360.  Initialize cancelled." ok
		set userAz ""
		return
	    }
	    set lowlim 15
	    if {![ string is double $userEl ] || $userEl < $lowlim || $userEl > 90} {
		
		MessBox "Error" "Elevation must be between $lowlim and 90.  Initialize cancelled." ok
		set userEl ""
		return
	    }

	    set cmdline "~/bin/azel2radec.tcl $userAz $userEl"
	    
	    catch { eval exec $cmdline } radec

	    set ra [ lindex $radec 0 ]
	    set dec [ lindex $radec 1 ]
	    # add a leading positive sign if necessary:
	    set decsign [ string range $dec 0 0 ]
	    if {$decsign != "-" && $decsign != "+"} {
		set dec "+$dec"
	    }
	    set epoch 2000.0

	    # check to make sure we calculated a reasonable ra,dec:
	    set result [ ValidateCoords $ra $dec $epoch ]

	    if {$result == "OK"} {
		
		set result [ SendCommand "NEXTPOS" "$ra $dec $epoch 0 0" ]
		set result [ SendCommand "DECLARE" "INITNEXT" ]
		return $result

	    } else {
		
		set message "Could not calculate a valid (RA, Dec) - call to azel2radec.tcl returned '$radec'\n\nTelescope NOT initialized to ($az, $el)"
		MessBox "Error" $message ok
		return
	    }

	} else {
	    return
	}
    }

    # this should make the current position equal to the commanded position

    # query the current and commanded positions, to display for the user's review:

    set commandedRA [ lindex [ SendCommand "XRA" $timeout ] 0 ]
    set commandedDEC [ lindex [ SendCommand "XDEC" $timeout ] 0 ]

    # DumpTCS updates the currentRA and currentDEC vars - probably already current,
    # but fetch the latest and greatest
    DumpTCS

    set message "This operation will make the CURRENT telescope position:\n\n($currentRA, $currentDEC)\n\nequal to the COMMANDED position:\n\n($commandedRA, $commandedDEC)\n\nAre you sure you want to Initialize the telescope?"

#    set telresult [ MessBox "Telescope Init" "This will make the CURRENT position equal to the COMMANDED position.  Are you sure you want to Initialize the telescope?" yesno ]

    set telresult [ MessBox "Telescope Init" $message yesno ]

    if {$telresult == "NO"} {

	return
    }

    set result [ SendCommand "DECLARE" "INITCOM" ]
    return $result
}
################################################################################
proc ListSelectPreset {} {

    global top userDome userEl userAz gMovemode

    set timeout 10

    set lbox $top.telescope.list
    set index [ $lbox.text curselection ]

    if {$index == ""} {
	return
    }

    set line [ $lbox.text get $index ]
    scan $line "%s %s %s %s %s %s" name el az dome trackrequest autorequest

    set result [ MessBox "Confirm" "Are you sure you want to move to preset position $name?" yesno ]
    if {$result == "NO"} {
	return
    }

    $top.telescope.lrname config -text ""
    $top.telescope.laname config -text $name

    set movetel 0
    set null "-"

    # autodome toggle
    if {$autorequest != $null} {

	set autostatus [ GetAutoDome ]

	# autodome is off and requested to be on:
	if {$autorequest == "ON" && !$autostatus} {
	    ToggleAutoDome
	}
	# autodome is on and requested to be off:
	if {$autorequest == "OFF" && $autostatus} {
	    ToggleAutoDome
	}
    }

    # move telescope
    if {$el != $null} {
	set userEl $el
	set movetel 1
    }
    if {$az != $null} {
	set userAz $az
	set movetel 1
    }
    if {$movetel} {
	set gMovemode "azel"
	TelescopeSlew
    }

    set trackstatus [ GetCorrection "t" ]

    # tracking toggle
    if {$trackrequest != $null} {

	# tracking is off and requested to be on:
	if {$trackrequest == "ON" && !$trackstatus} {
	    ToggleTracking 1  
	}
	# tracking is on and requested to be off:
	if {$trackrequest == "OFF" && $trackstatus} {
	    ToggleTracking
	}
    }

    # move dome
    if {$dome != $null} {
	set userDome $dome
	DomeRotate
    }

    $top.telescope.lrname configure -text ""
    $top.telescope.laname configure -text $name

    Refresh
}
################################################################################
proc ListSelectTarget {} {

    global top userRA userDEC gMovemode currentcatalog LOCATION gMovemode

    set lbox $top.telescope.list
    set index [ $lbox.text curselection ]

    if {$index == ""} {
	return
    }

    set line [ $lbox.text get $index ]

    set name [ lindex $line 0 ]
    set ra [ lindex $line 1 ]
    set dec [ lindex $line 2 ]

    set disabstat [ SendCommand "DISABSTAT" ]
    if {$disabstat} {
	set result [ MessBox "Slew Failed" "TCS is disabled.  Slew to $currentcatalog $name cancelled." ok ]
	return
    }

    # do a quick verify to see if the TCS will allow it:
    set result [ SendCommand "VERIFY" "$ra $dec 2000.00" ]
    
    if {$result != 1} {
	MessBox "Error" "Object $currentcatalog $name is beyond the TCS limits." ok
	return
    }

    set result [ MessBox "Confirm" "Are you sure you want to slew to $currentcatalog $name?" yesno ]
    if {$result == "NO"} {
	return
    }

    # all tests passed...go ahead and slew to the catalog object

    if {[ string first $currentcatalog $LOCATION ] != -1} {

	# if this is a standard field list:
	set gMovemode "radec"
	set userRA $ra
	set userDEC $dec

	TelescopeSlew

    } else {

	set result [ SendCommand $currentcatalog $name ]
	set result [ SendCommand "MOVNEXT" ]
    }

    if {[ string first "FAILED" $result ] == -1} {
	# if the TCS gave the OK, then update the target name:
	set userRA $ra
	set userDEC $dec
	$top.telescope.lrname config -text "$currentcatalog $name"
	$top.telescope.laname config -text ""

    } else {

	set result [ MessBox "Slew Failed" "TCS is unable to slew to $currentcatalog $name" ok ]
    }
}
################################################################################
proc ListSelectSolar {} {

    global top

    set lbox $top.telescope.list
    set index [ $lbox.text curselection ]

    if {$index == ""} {
	return
    }

    set line [ $lbox.text get $index ]
    set name [ lindex $line 0 ]

    set result [ MessBox "Confirm" "Are you sure you want to slew to $name?" yesno ]
    if {$result == "NO"} {
	return
    }

    set disabstat [ SendCommand "DISABSTAT" ]
    if {$disabstat} {
	set result [ MessBox "Slew Failed" "TCS is disabled.  Slew to $name cancelled." ok ]
	return
    }

    set result [ SendCommand $name ]

    if {[ string first "FAILED" $result ] == -1} {
	# if the TCS gave the OK, then display the planet name:
	$top.telescope.lrname config -text $name
	$top.telescope.laname config -text ""

    } else {

	set result [ MessBox "Slew Failed" "TCS is unable to slew to $name" ok ]
    }
}
################################################################################
proc LoadPreset {} {

    global top

    set cat "preset.cat"
    set tel $top.telescope
    set lbox $top.telescope.list

    if {![ file isfile $cat ]} {
	puts "Cannot find catalog '$cat'."
	return
    }

    # clear the listbox:
    $lbox.text delete 0 end

    set channel [ open $cat r ]
    set line [ gets $channel ]

    set header "Preset     EL    AZ  Dome  Track  Auto  "
    $lbox.head config -text $header

    while {![ eof $channel ]} {

	scan $line "%s %s %s %s %s %s" name el az dome track autodome

	# skip comments:
	if {$name == "#"} {
	    set line [ gets $channel ]
	    continue
	}

	set textline [ format " %-9s %4s %5s %5s %5s %5s" $name $el $az $dome $track $autodome ]

	$lbox.text insert end $textline

	set line [ gets $channel ]
    }

    close $channel

    # change meaning of APPLY button:
    $tel.bload config -command { ListSelectPreset }
    bind $lbox.text <Double-Button-1> { ListSelectPreset }
}
################################################################################
proc LoadSolar {} {

    global top

    set cat "solar.cat"
    set lbox $top.telescope.list
    set tel $top.telescope

    if {![ file isfile $cat ]} {
	puts "Cannot find catalog '$cat'."
	return
    }

    # clear the listbox:
    $lbox.text delete 0 end

    set channel [ open $cat r ]
    set line [ gets $channel ]

    set header "Object                            "
    $lbox.head config -text $header

    while {![ eof $channel ]} {

	scan $line "%s" name

	# skip comments:
	if {$name == "#"} {
	    set line [ gets $channel ]
	    continue
	}

	set textline [ format "   %s" $name ]

	$lbox.text insert end $textline

	set line [ gets $channel ]
    }

    close $channel

    # change meaning of APPLY button:
    $tel.bload config -command { ListSelectSolar }
    bind $lbox.text <Double-Button-1> { ListSelectSolar }
}
################################################################################
proc LoadCatalog { stringcat { magthresh } } {
# magthresh is the maximum (faintest) magnitude allowed to be included

    global gobslat top

    set cat "$stringcat.cat"

    if {![ file isfile $cat ]} {
	puts "Cannot find catalog '$cat'."
	return
    }

    set lbox $top.telescope.list
    set tel $top.telescope

    # proper motion threshhold, arcsec per year...essentially turn off for now
#    set pmthresh 999
    # maximum allowable delta Dec...dependendent on latitude:
    set decthresh 70
    # absolute dec limits:
    set maxdec 90
    set mindec -90
    set highdec [ expr {$gobslat + $decthresh} ]
    set lowdec [ expr {$gobslat - $decthresh} ]
    if {$highdec > $maxdec} {
	set highdec $maxdec
    }
    if {$lowdec < $mindec} {
	set lowdec $mindec
    }

    # clear the listbox:
    $lbox.text delete 0 end

    set channel [ open $cat r ]
    set line [ gets $channel ]

    set header "ID        RA           Dec        V "
    $lbox.head config -text $header 

    while {![ eof $channel ]} {

#	scan $line "%s %s %s %s %f %f %s %i %c %s" num ra dec epoch pm1 pm2 mag num2 c type
	scan $line "%s %s %s %s" num ra dec mag
	
#	if {![ info exists mag ] || $mag == ""} {
#	    set mag 99
#	}

	# check for missing objects - these have RA,Dec of 00:00:00
	if {$ra == "00:00:00.000" && $dec == "00:00:00.00"} {
	    set line [ gets $channel ]
	    continue
	}

	# magnitude criteria:
	if {$mag > $magthresh} {
	    set line [ gets $channel ]
	    continue
	}

	# declination criteria:
	set thisdec [ sex2dec $dec ]
	if {$thisdec > $highdec || $thisdec < $lowdec} {
	    set line [ gets $channel ]
	    continue
	}

	# doctor up the coordinates:
	if {[ string range $dec 0 0 ] != "-"} {
	    set dec "+$dec"
	}
	set ra [ string range $ra 0 end-1 ]
	set dec [ string range $dec 0 end-1 ]

	# proper motion criteria:
#	set pm1 [ expr {abs($pm1)} ]
#	set pm2 [ expr {abs($pm2)} ]
#	if {$pm1 > $pmthresh || $pm2 > $pmthresh} {
#	    set line [ gets $channel ]
#	    continue
#	}

	incr count

	set textline [ format "%+5s  %-13s %-13s %4.2f" $num $ra $dec $mag ]

	# this entry passed all the tests...add it to the listbox:
	$lbox.text insert end $textline
	
	set line [ gets $channel ]
    }

    close $channel

    # change meaning of APPLY button:
    $tel.bload config -command { ListSelectTarget }
    bind $lbox.text <Double-Button-1> { ListSelectTarget }
}
################################################################################
proc LoadStandardFields { { cat "" } } {

    global top LOCATION

    # depends on fieldlist files being available in ~/catalina
    set cat "~/catalina/$cat.txt"

    if {![ file isfile $cat ]} {
	puts "Cannot find catalog '$cat'."
	return
    }

    set lbox $top.telescope.list
    set tel $top.telescope

    # clear the listbox:
    $lbox.text delete 0 end

    set channel [ open $cat r ]
    set line [ gets $channel ]
    set line [ gets $channel ]
    set line [ gets $channel ]

    set header " ID          RA             Dec     "
    $lbox.head config -text $header 

    while {![ eof $channel ]} {

	scan $line "%s %s %s" num ra dec 

	if {$num == "END"} {
	    break
	}

	set textline [ format "  %+6s    %-13s %-13s" $num $ra $dec ]

	# this entry passed all the tests...add it to the listbox:
	$lbox.text insert end $textline
	
	set line [ gets $channel ]
    }

    close $channel

    # change meaning of APPLY button:
    $tel.bload config -command { ListSelectTarget }
    bind $lbox.text <Double-Button-1> { ListSelectTarget }
}
################################################################################
proc ToggleGuideDrift {} {

    global gGuideDrift top 

    set pad $top.pad

    set state1 "normal"
    set state2 "disabled"

    if {$gGuideDrift == "drift"} {
	set state1 "disabled"
	set state2 "normal"
    }

    $pad.eguide config -state $state1
    $pad.edrift config -state $state2

    $pad.ltel   config -text [ string toupper $gGuideDrift ]
}
################################################################################
proc ToggleTelemetryGUI { { onoff "" } } {

    global top gTCSConnected LOCATION gLatency SIMULATION

    set tel $top.telescope

    # if we're not connected, then do some tests to see if we're able to connect:
    if {!$gTCSConnected} {

	TCSCommStart	
	set tcsalive [ PingTCS ]
	
	if {!$tcsalive} {

	    $tel.ltele2 config -state active -text "TCS DISCONNECTED"
	    
	    set result [ MessBox "Timeout" "The TCS GUI cannot find a valid TCS-NG.  Make sure TCS is on and available on the network, and try again to connect." ok ]
	    
	    $tel.btelem configure -state normal
	    
	    TCSCommStop

	    KeyboardTraversal
	    return
	}

	# successfully connected to the TCS.  Let's check the time before going any further:
	CheckTCSTime
    }

    # if we have explicit instructions to turn on/off, pre-set the gTCSconnected to toggle
    # the other way:
    if {$onoff == "on"} {
	set gTCSConnected 0
    } elseif {$onoff == "off"} {
	set gTCSConnected 1
    }

    # turn telemetry off:
    if {$gTCSConnected} {

	set gTCSConnected 0

	# one last refresh of the paddle before we quit...
	SetPaddle
	DisEnableGUI "disabled"
        TCSCommStop

    } else {

	# turn telemetry on:
	set gTCSConnected 1

        # Start TCS socket thread.
        TCSCommStart
	# get the current state of the TCS:
	DumpTCS
	GetDomeParam
	DisEnableGUI "normal"
	Refresh
    }

    # update telemetry-related widgets:

    if {$gTCSConnected} {

	set state normal
	set btext "DISCONNECT"
	set ltext "TCS CONNECTED ($LOCATION)"

    } else {

	set state active
	set btext "CONNECT"
	set ltext "TCS DISCONNECTED"
    }
    $tel.btelem config -state $state -text $btext
    $tel.ltele2 config -state $state -text $ltext

    Refresh

    # reset the latency variables:
    set gLatency(calibrated) 0
    set gLatency(count) 0

    # re-define the keyboard bindings:
    KeyboardTraversal
}
################################################################################
proc ChooseCatalog {} {

    global top cataloglist catalogindex currentcatalog

    set cataloglength [ llength $cataloglist ]

    incr catalogindex
    if {$catalogindex >= $cataloglength} {
	set catalogindex 0
    }

    set catalogstring [ lindex $cataloglist $catalogindex ]

    set catalog [ file rootname [ file tail $catalogstring ] ]
    set currentcatalog [ string toupper $catalog ]

    set ind [ string first "." $currentcatalog ]
    if {$ind != -1} {
	set currentcatalog [ string range $currentcatalog [ expr {1 + $ind} ] end ]
    }

    ToggleListBox $catalog
}
################################################################################
proc ToggleListBox { catalog } {

    global top

    set ybscthresh 5
    set fk5thresh 9
    set maxthresh 100

    set lbox $top.telescope.list

    bind $lbox.text <Double-Button-1> { ListSelectTarget }

    # configure label
    $top.telescope.lcat configure -text [ string toupper $catalog ]

    if {$catalog == "fk5"} {
	LoadCatalog $catalog $fk5thresh
    } elseif {$catalog == "ybsc"} {
	LoadCatalog $catalog $ybscthresh
    } elseif {$catalog == "ngc" || $catalog == "abell" || $catalog == "ic" || $catalog == "okestone" || $catalog == "zwicky"} {
	LoadCatalog $catalog $maxthresh
    } elseif {$catalog == "gcvs" || $catalog == "ppm" || $catalog == "sao"} {
	LoadCatalog $catalog $maxthresh
    } elseif {[ string first "fieldlist" $catalog ] != -1} {
	LoadStandardFields $catalog
    } elseif {$catalog == "preset"} {
	LoadPreset
    } elseif {$catalog == "solar"} {
	LoadSolar
    } else {
	puts "Proc ToggleListBox: Unrecognized source '$catalog'"
    }
}
################################################################################
proc ToggleMoveMode {} {

    global gMovemode userRA userDEC userEpoch userAz userEl

    if {$gMovemode == "radec"} {
	set userAz ""
	set userEl ""
    } elseif {$gMovemode == "azel"} {
	set userRA ""
	set userDEC ""
    } else {
	puts "Proc ToggleMoveMode: Unrecognized move mode '$gMovemode'"
    }

    Refresh
}
################################################################################
proc DomeRotate {} {

    global userDome

    if {$userDome == ""} {
	return
    }

    if {![ string is integer $userDome ] && ![ string is double $userDome ]} {

	MessBox "Error" "'$userDome' is not a valid azimuth." ok
	set userDome ""
	return
    }

    if {$userDome == "" || $userDome < 0 || $userDome > 360} {

	MessBox "Error" "Enter a valid dome azimuth, between 0 and 360 degrees" ok
	set userDome ""
	return
    }

    if {$userDome == 360} {
	set userDome 0
    }

    set result [ SendCommand "DOMEGOTO" $userDome ]

    set userDome ""

    return $result
}
################################################################################
proc DomeInit { { force 0 } } {

    global userDome

    if {!$force} {
	set domeresult [ MessBox "Dome Init" "Make sure the dome shutter is in the correct stow position.  Are you sure you want to Initialize the dome?" yesno ]
	
	if {$domeresult == "NO"} {
	    return
	}
    }

    # else go ahead and init:
    set userDome ""
    set result [ SendCommand "DOME" "INIT" ]
    
    Refresh
    
    return $result
}
################################################################################
proc DomeStop {} {

    set result [ SendCommand "DOME" "PADDLE STOP" ]
    set result [ SendCommand "DOME" "AUTO OFF" ]

    return $result
}
################################################################################
proc FocusZero {} {

    global userFocus

    set timeout 5

    set result [ MessBox "Focus Zero" "Are you sure you want to set the current focus encoder value to zero?" yesno ]

    if {$result == "NO"} {

	return
    }

    # else cleared to zero focus:

    set userFocus ""

    set result [ SendCommand "FOCZERO" ]

    Refresh

    return $result
}
################################################################################
proc FocusAdjust {} {

    global userFocus currentFocus

    set bigdiff 1000
    set relative 0

    if {$userFocus == ""} {
	return
    }

    if {![ string is integer $userFocus ]} {

	MessBox "Error" "Enter a valid integer to adjust the focus.  Relative offsets are indicated by a leading sign (i.e. +10 or -50).  Unsigned integers are interpreted as absolute focus commands." ok
	set userFocus ""
	return
    }

    # query the current focus:
    set currentFocus [ SendCommand "FOCUS" ]

    set tempuserFocus $userFocus

    set diff [ expr {abs($currentFocus - $tempuserFocus)} ]

    if {$diff >= $bigdiff} {
	set result [ MessBox "Large Focus Move" "You are commanding the focus to move $diff units.  Proceed?" yesno ]

	if {$result == "NO"} {
	    return
	}
    }

    # keep user focus value displayed, to be able to compare commanded w/ actual
    if {!$relative} {
	set userFocus ""
    }
    # passed all tests, so move the focus

    set result [ SendCommand "FOCUS" $tempuserFocus ]
    return $result
}
################################################################################
proc TogglePEC { { force 0 } } {

# pec mode 1 = on, 0 = off, 3 = waiting

    set peccheck [ SendCommand "PECSTAT" ]

    if {$peccheck == 0} {

	set result [ SendCommand "PEC" "ON" ]

    } else {

	# remove later:
	set force 1

	if {!$force} {

	    set pecresult [ MessBox "Warning" "Are you sure you want to turn PEC off?" yesno ]
	    if {$pecresult == "NO"} {
		return "CANCELLED"
	    }
	}

	set result [ SendCommand "PEC" "OFF" ]
    }

    Refresh

    return $result
}
################################################################################
proc ToggleTracking { { force 0 } } {

    set trackcheck [ GetCorrection "t" ]

    if {$trackcheck == 0} {

	set result [ SendCommand "TRACK" "ON" ]
	
	# GUI smarts: automatically turn on the PEC when turning on the tracking
	set peccheck [ SendCommand "PECSTAT" ]
	if {$peccheck == 0} {
	    TogglePEC 1 
	}
	
    } else {

	# remove later:
	set force 1

	# GUI smarts: the TCS shuts off autodome when turning off the tracking.  We go around this
	# weird behavior by turning the autodome back on, if necessary:

	# now fixed in TCS?

	if {!$force} {

	    set trackresult [ MessBox "Warning" "Are you sure you want to turn tracking off?" yesno ]
	    if {$trackresult == "NO"} {
		return "CANCELLED"
	    }
	}
	set result [ SendCommand "TRACK" "OFF" ]

     }

    Refresh

    return $result
}
################################################################################
proc ToggleAutoDome { { force 0 } } {
# TCS-NG undesirable behavior: does not allow autodome to be turned on w/o first
# initializing Dome.  We want this to be transparent to the user in the most common
# case of starting TCS gui when Dome is in the stow position.

    global userDome

    # check what is the state of autodome first?
    set autocheck [ GetAutoDome ]

    if {$autocheck != 1} {

	# check to see if dome is initialized already (TBD):
	
	# if not initialized, check to see if dome is reporting the stow position:

	set result [ GetDomePos ]

	# need to make this location dependent...don't assume dome stow pos. is always 90 (i.e. Schmidt)

	if {$result == 90} {
	    DomeInit 1
	}	

	# make sure we're always using the dome lookahead:
	set result [ SendCommand "DOME" "LOOKAHEAD 1" ]
	set result [ SendCommand "DOME" "AUTO ON" ]

    } else {

	# remove later:
	set force 1

	if {!$force} {

	    set result [ MessBox "Warning" "Are you sure you want to turn AutoDome off?" yesno ]
	    if {$result == "NO"} {
		return "CANCELLED"
	    }
	}

	set result [ SendCommand "DOME" "AUTO OFF" ]
    }

    set userDome ""

    Refresh

    return $result
}
################################################################################
proc ToggleDisable {} {

    global gTCSDisabled gPaddleState

    set disabcheck [ SendCommand "DISABSTAT" ]

    if {$disabcheck == 0} {
	set result [ SendCommand "DISABLE" ]
	set gTCSDisabled 1
    } else {
	set result [ SendCommand "ENABLE" ]
	set gTCSDisabled 0
    }
    
    Refresh

    KeyboardTraversal

    return $result
}
################################################################################
proc SendCommandLine {} {

    global userCommand tcsReply tcsreturn top docpage

    set eng $top.engineering

    # format the command:
    set upcaseCommand [ string toupper $userCommand ]

    $eng.lfeed delete 1.0 2.0

    # blink the reply, to show we're doing something new:
    set tcsReply ""
    update idletasks
    after 50

    # first arg is always the command or request:
    set firstarg [ lindex $upcaseCommand 0 ]

    # if there are any more args, they need to be sent separately:
    set args ""
    for {set i 1} {$i <= [ llength $upcaseCommand ]} {incr i} {

	append args [ lindex $upcaseCommand $i ]
	append args " "
    }   

    # intercept a help request here:
    if {$firstarg == "HELP"} {

	set result $docpage
	set tcsreturn $docpage

    } else {

	# send it to the TCS:
	set result [ SendCommand $firstarg $args ]
    }

    # capture the full return - publish to the textvariable
    set tcsReply $tcsreturn

    $eng.lfeed insert insert $tcsreturn

    # also dump to terminal:
    puts "sent '$upcaseCommand' received '$tcsreturn'"

    if {$result != ""} {
	set userCommand $upcaseCommand
    }
}
################################################################################
proc SendCommand { command { args "" } { timeout 5 } } {
# TCS-NG typically returns some extra stuff in response to a query/command.  For
# some queries the useful information is at a different index.  Deal with this here:

# some code refers to a timeout that should be sent as a final argument...it appears
# to be required but not used

    global tcsreturn verbose gTCSConnected

    # don't try to send commands if we're not connected:
#    if {!$gTCSConnected && $command != "TIME" && $command != "DATE"} {
#	return ""
#    }

    if {$args == ""} {
	set argstring $timeout
    } else {
	set argstring "$args $timeout"
    }

    if {$verbose} {
	puts ""
	puts "GUI: SendCommand: command '$command', argstring '$argstring'"
    }
    set result [ CommandTCS $command $argstring ]
    set firstword [ lindex $command 0 ]

    # Extract return values.
    set result [lrange [lindex $result 1] 2 end]

    set index 1

    # need to generalize this: simply return everything except the "OK" - from index 1 to end
    # let the caller figure out what they want

    # sometimes we get more info than we need...pinpoint which one(s) we want:
    switch $firstword {

	"DOME"      { 
	    if {[ string first "PARAM" $argstring ] == -1} {
		set index {2 5}
	    } else {
		set index "all"
	    }
	}
	"LIMIT"     { set index 1 }
	"PECSTAT"   { set index 1 }
	"XRA"       { set index "all" }
	"XDEC"      { set index "all" }
    }

    # return the desired index and trim the carriage return from the result:

    set returnstring [ string map { \n "" "{" "" "}" "" } $result ] 

    if {$verbose} {
	puts "GUI: SendCommand: returnstring = '$returnstring'"
	puts "GUI: SendCommand: result = '$result'"
    }

    # save the full output for later use...sometimes we'll want to display or log this
    set tcsreturn [ lrange $result 1 end ]

    if {$index == "all"} {

	set return $tcsreturn

    } elseif {[ llength $index ] == 1} {
	# we only care about one index:
	set return [ lindex $returnstring $index ]
    } else {
	# case where we want more than one index:
	set return ""
	foreach i $index {

	    append return [ lindex $returnstring $i ]
	    append return " "
	}
    }

    if {$verbose} {
	puts "GUI: Returned '$return' from '$returnstring'"
    }

    return $return
}
################################################################################
proc PingTCS {} {

    global verbose top LOCATION

    set tel $top.telescope

    if {$verbose} {
	puts "Pinging TCS............."
    }

    $tel.ltele2 config -state active -text "SEARCHING FOR $LOCATION TCS ..."
    $tel.btelem configure -state disabled


    set returnstring 1

    set result [ SendCommand "TIME" ]

    if {$verbose} {
	puts "PingTCS: '$result'"
    }

    if {$result == ""} {

	set returnstring 0
    }

    return $returnstring
}
################################################################################
proc CheckTCSTime {} {

    global verbose

    # get the date/time from the TCS:

    set tcsTime [ SendCommand "TIME" ]
    set tcsDate [ SendCommand "DATE" ]
    
    # TCS reports time to the hundredth of a second - chop this off:
    set tcsTime [ string range $tcsTime 0 7 ]

    # get the time from the local machine:
    
    set localUTTime [ clock format [ clock seconds ] -format "%T" -gmt 1 ]
    set localDate [ clock format [ clock seconds ] -format "%D" -gmt 1 ]

    # find seconds from 1970 for both times, 
    set tcsSec [ clock scan "$tcsTime $tcsDate" -gmt 1 ]
    set localSec [ clock scan "$localUTTime $localDate" -gmt 1 ]

    set diffsec [ expr {abs($tcsSec - $localSec)} ]

    # if less than 1356998399 seconds (Dec. 31 2012 23:59:59), it must be wrong:
    set minsec [ clock scan "12/31/2012 23:59:59" -gmt 1 ]

    if {$tcsSec < $minsec} {
	
	set message "TCS is reporting a time that is clearly in the past:\n\n$tcsDate $tcsTime\n\nThis must be corrected before moving the telescope."

    } elseif {$diffsec > 10} {

	set message "The TCS time is different from the time on this (GUI) machine, by $diffsec seconds.\n\nTCS time (UT): $tcsTime $tcsDate\n\nGUI time (UT): $localUTTime $localDate\n\nIncorrect TCS time can lead to pointing errors and mis-calculation of limits."

	# this is a dangerous condition: disable the GUI
#	DisEnableGUI disabled

	MessBox "Warning" $message ok
    }
}
################################################################################
# main:

# parse command line arguments:

global SIMULATION engineering LOCATION refreshrate refreshHertz

set SIMULATION 0
set engineering 0
set verbose 0
set fast 0
set LOCATION ""
set refreshrate 1000
set refreshHertz 1
set docpage "https://soweb.as.arizona.edu/~tscopewiki/doku.php?id=tcs:tcsng_command_list"

if {$argc <= 5} {

    for {set i 0} {$i < $argc} {incr i} {

	set arg [ string toupper [ lindex $argv $i ] ]

	if {([ string is double $arg ] || [ string is integer $arg ]) && $arg != "703"} {

	    if {$arg <= 0 || $arg > 5} {
		puts "GUI: Invalid arg Hz = '$arg'"
		HelpExit
	    }

	    set refreshrate [ expr { int(1000 / (1.0 * $arg))} ]
	    set refreshHertz $arg

	    continue
	}

	switch $arg {

	    "703"   { set LOCATION "703" }
	    "G96"   { set LOCATION "G96" }
	    "E12"   { set LOCATION "E12" }
	    "I52"   { set LOCATION "I52" }
	    "V06"   { set LOCATION "V06" }
	    "21-IN" { set LOCATION "21-IN" }
	    "-SIM"  { set SIMULATION 1 }
	    "-ENG"  { set engineering 1 }
	    "-VER"  { set verbose 1 }
	    "-FAST" { set fast 1 }
	    "HELP"  { HelpExit }

	    default {
		puts "\n  Unidentified argument '$arg'"
		HelpExit
	    }
	}
    }

} else {
    HelpExit
}

# default for the LOCATION - can be overridden on the command line:
if {$LOCATION == ""} {
    if { [info exists env(LOCATION)] } {
	set LOCATION $env(LOCATION)
    } else {
	if {$verbose} {
	    puts "GUI: No LOCATION specified.  Setting to 703."
	}
	set LOCATION "703"
    }
} else {

    if {$verbose} {
	puts "GUI: Set LOCATION to $LOCATION"
    }
}

if {$fast} {
    set refreshHertz 5
    set refreshrate 200
}

# only allowed to run -fast in combination with -sim:

if {$fast && !$SIMULATION} {
    puts "GUI: '-fast' argument only available with '-sim'.  Ignoring."
    set refreshHertz 1
    set refreshrate 1000
    set fast 0
}

# aqtelemetry.tcl contains SendCommand, ConvertRAtoTelemRA, ConvertDectoTelemDEC
# and all the protocol for parsing telemetry source it:
source aqtelemetry.tcl
source messbox.tcl

source globals0
source aqglob.tcl

global gobslat gobslong
global top pi version tcswait tcsreturn simpid
global motionRA motionDEC motionFoc motionDome

# a few globals that need to be initialized for aqtelemetry:
set guseTCSng 1
set gpausetelemetry 0
set gtelemetrystarted 0

# set the computer IP and tel ID - simulation mode will overwrite later
switch $LOCATION {

    "I52"   { set gTCSinterface 192.168.2.40   ; set gTCSngtelid TCSNG }
    "V06"   { set gTCSinterface 10.30.5.69     ; set gTCSngtelid BIG61 }
    "G96"   { set gTCSinterface 192.168.2.60   ; set gTCSngtelid LEM60 }
    "21-IN" { set gTCSinterface 10.130.132.182 ; set gTCSngtelid 21-IN }
    "703"   { set gTCSinterface 192.168.2.26   ; set gTCSngtelid SCHMI }
    "E12"   { set gTCSinterface 0.0.0.0        ; set gTCSngtelid SSS } 

    default { HelpExit }
}

if {$SIMULATION} {

    # look for existing tcs simulators - if found, connect to that one.  if not, start one up:

    set grepreturn [ exec ps -fea | grep "tcs-sim.tcl $LOCATION" ]

    set id [ lindex $grepreturn 1 ]
    set procname [ lindex $grepreturn 7 ]

    if {[ string first "-fast" $grepreturn ] != -1} {
	set fast 1
	set refreshHertz 5
	set refreshrate 200
    }

    if {$id == "" || $procname == "grep" || $procname == "emacs"} {

	# start the TCS simulator:

	if {$verbose} {
	    if {$fast} {
		set simpid [ exec ./tcs-sim.tcl $LOCATION -ver -fast & ]
	    } else {
		set simpid [ exec ./tcs-sim.tcl $LOCATION -ver & ]
	    }
	} else {
	    if {$fast} {
		set simpid [ exec ./tcs-sim.tcl $LOCATION -fast & ]
	    } else {
		set simpid [ exec ./tcs-sim.tcl $LOCATION & ]
	    }
	}
	
	if {$verbose} {
	    puts "TCS-NG simulator started with pid $simpid"
	}

    } else {

	set simpid $id

	if {$verbose} {
	    puts "Found TCS-NG simulator with pid $simpid"
	}
    }

    set gTCSngtelid TCSIM
    set gTCSinterface 0.0.0.0
}

# remove verbose and gTCSbusy from here later:
global verbose gTCSbusy gTCSport gTCSConnected gLatency
# size
global tcswidth tcsheight
# globals for radar screen:
global rwidth rheight rcentx rcenty rborder rscale radarDisplayed
global rbackcolor rtextcolor relevcolor rtelcolor rlimitcolor rdomecolor rtelsize
# globals for paddle pane
global pshare pwidth pheight pbackcolor ptextcolor
# globals for entry pane:
global ewidth eheight ebackcolor etextcolor oncolor offcolor offcolor movecolor alarmtext
# globals for paddle:
global paddleDisplayed userDrift userGuide gGuideDrift userBiasRA userBiasDEC currentBiasRA currentBiasDEC currentCorrections

global currentDate

# build the widgets
set top .tcs
set version "1.0.2 05-May-2016"
toplevel $top -background black

wm withdraw .
wm protocol $top WM_DELETE_WINDOW { ExitTCS }
set title "TCS-NG ($LOCATION), v. $version"
if {$SIMULATION} {
    append title "     *** SIMULATION MODE ***"
    append title "     *** SIMULATION MODE ***"
    append title "     *** SIMULATION MODE ***"
}
wm title $top $title
wm resizable $top 0 0

# need to ping TCS to see if we can connect.
set gTCSConnected 0

InitializeGUI

set radar [ BuildRadar ]
set paddle [ BuildPaddle ]
set entry [ BuildEntry ]

pack $entry -side left -fill both
pack $paddle -side bottom -fill both
pack $radar -side left -fill both

# default to show the standard field menu:
global currentcatalog

set gTCSConnected 0

DisEnableGUI disabled

# try to connect to the TCS.  This will ping the TCS, connect if it's alive, and check the time:
ToggleTelemetryGUI

tk_focusFollowsMouse

# funny behavior with messboxes: if multiple messboxes are raised, answering "yes" or "no" sometimes
# doesn't do what we expect - interference?  possibly modify messbox to not allow multiple messboxes to be raised

# add logging?

# modify commandtcs to not report TIME query result...should just silently return 1 or 0 based on whether it could talk to the TCS.  also preserve timeout (or shorten for all to ~3 sec)

# need to incorporate sysreset time date time - when automatic time check detects an offset

