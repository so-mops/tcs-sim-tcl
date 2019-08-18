#!/usr/bin/tclsh
################################################################################
proc ShowUsage {} {

    puts "\n  Usage: tcs-sim.tcl <LOCATION> \[-ver\]\n \[-fast\]"
}
################################################################################
proc deg2rad { deg } {
    
    global pi
    
    set rad [ expr {($deg * $pi) / 180.0} ]
    return $rad
}
################################################################################
proc sex2dec { sexvalue } {

    set place1 ""
    set place2 ""
    set place3 ""

    scan $sexvalue {%3d:%2d:%4f} place1 place2 place3

    # if the string is malformatted, return some diagnostics consistent w/ real TCS-NG

    if {$place1 == ""} {
	return -1
    }
    if {$place2 == ""} {
	return -2
    }
    if {$place3 == ""} {
	return -3
    }

    set decval [expr {abs($place1) + $place2 / 60.0 + $place3 / 3600.0}]

    if {[string index $sexvalue 0] == "-"} {
        set decval [ expr {$decval * -1.0} ]
    }
    
    return $decval
}
################################################################################
# proc to calculate julian date given the calendar date
# input is year (integer), month (integer) and UT day (decimal)
# returns decimal julian date                                                    
# From "Astronomical Algorithms" by J. Meeus                                               
# Copyright 1991, First Edition                                                            
# Page 61
proc cd2jd { y m d } {

    if {$m <= 2} {
        set y [expr {$y - 1}]
        set m [expr {$m + 12}]
    }

    set a [expr {int($y / 100)}]
    set b [expr {2 - $a + int($a / 4)}]

    set jd [ format "%14.6f" [expr {int(365.25 * ($y + 4716)) + int(30.600001 * ($m + 1)) + $d + $b - 1524.5}] ]

    return $jd
}
################################################################################
# proc to calculate MEAN siderial time at greenwich UT julian date
# returns decimal hours

# From "Astronomical Algorithms" by J. Meeus
# Copyright 1991, First Edition
# Page 83
proc SiderealTime { jdut } {

    set T  [expr {($jdut - 2451545.0) / 36525}]
    set T2 [expr {$T * $T}]
    set T3 [expr {$T2 * $T}]
    
    set mst [expr {280.46061837 + 360.98564736629 * ($jdut - 2451545.0) + 0.000387933 * $T2 - $T3 / 38710000}]

    if {$mst < 0} {
        set mst [expr {$mst - ((int($mst/360.0) - 1.0) * 360.0)}]
    } else {
        set mst [expr {$mst - (int($mst/360.0) * 360.0)}]
    }

    set mst [expr {$mst / 15.0}]

    return $mst
}
################################################################################
# proc to calculate RA given altitude and azimuth
# in decimal degrees, siderial time at greenwich in decimal hours,
# observatory latitude and longitude in decimal degrees
# RA and Dec is returned as a 2-element list:
# { RA (decimal hrs) Dec (decimal degrees) }

# From Patrick Wallace's H2E.f routine - SLALIB Library

# note Azimuth is reckoned as 0 degrees is North and moving
# through East, South, and then West

proc e2h { alt az st lat long } {

    global radian

    set cosphi [expr {cos($lat/$radian)}]
    set sinphi [expr {sin($lat/$radian)}]
    set cosA [expr {cos($az/$radian)}]
    set sinA [expr {sin($az/$radian)}]
    set cosh [expr {cos($alt/$radian)}]
    set sinh [expr {sin($alt/$radian)}]

    set x [expr {-$cosA * $cosh * $sinphi + $sinh * $cosphi}]
    set y [expr {-$sinA * $cosh}]
    set z [expr {$cosA * $cosh * $cosphi + $sinh * $sinphi}]

    set r [expr {sqrt($x * $x + $y * $y)}]

    if {$r == 0.0} {
	set H 0.0
    } else {
        set H [expr {atan2($y,$x) * $radian}]
    }

    set ra [expr {$st - $long/15.0 - $H/15.0}]
    if {$ra < 0} {
        set ra [expr {$ra + 24.0}]
    }
    set dec [expr {atan2($z,$r) * $radian}]

    return [list $ra $dec]
}
################################################################################
proc TCSradec2azel { ra dec } {
# distilled from A.R.Gibbs' skycoor class
# ra and dec given in decimal hours (ra) and decimal degrees (dec).
# az, el returned in decimal degrees

# renamed from radec2azel to not collide with definition in astrolib.tcl

    global gobslat gobslong pi simTime

    set lha [ SimLHA $simTime(currentLST) $ra ]

    set fMST [ deg2rad [ expr {15 * $simTime(currentGMST)} ] ]
    set fLat [ deg2rad $gobslat ]
    set fLon [ deg2rad $gobslong ]

    set fRA [ deg2rad [ expr {15 * $ra} ] ]
    set fDec [ deg2rad $dec ]
    set fHA [ expr {[ deg2rad [ expr {15 * $lha} ]]} ]

    set fX      [expr {cos ($fLat) * sin ($fDec) - sin ($fLat) * cos ($fDec) * cos ($fHA)}]
    set fY      [expr {-cos ($fDec) * sin ($fHA)}]
    set fAz     [expr {atan2 ($fY, $fX)}]
    if {$fAz < 0.0} { set fAz [expr {2.0 * $pi + $fAz}] }
    set fCosAlt [expr {sqrt ($fX * $fX + $fY * $fY)}]
    set fSinAlt [expr {sin ($fDec) * sin ($fLat) + cos ($fDec) * cos ($fLat) * cos ($fHA)}]
    set fAlt    [expr {atan2 ($fSinAlt, $fCosAlt)}]  

    set fAlt    [expr {$fAlt * 180.0 / $pi}]
    set fAz     [expr {$fAz  * 180.0 / $pi}]
#    set fHA     [expr {$fHA  * 180.0 / $pi}]
    
    return [list $fAz $fAlt]
}
################################################################################
proc GetJD { { date "now" } } {

    # get the UT date right now:
    
    if {$date == "now"} {

	set datestring [ SimReturnDate ]
	scan $datestring "%2u/%2u/%4u" month day year
	set timestring [ SimReturnTime ]
	scan $timestring "%2u:%2u:%2u" hr min sec

    } else {

	set year [ string range $date 0 3 ]
	set month [ string range $date 4 5 ]
	set day [ string range $date 6 7 ]

	set hr 0
	set min 0
	set sec 0
    }

    set julianday [ cd2jd $year $month $day ]
    set frac [ expr {($hr / 24.0) + ($min / 1440.0) + ($sec / 86400.0)} ]
    set jdnow [ expr {$julianday + $frac} ]
    
    return $jdnow
}
################################################################################
proc SimSysKill {} {
# Kills TCS process after disabling stopping all telescope motion.

    set result [ SimDisable 1 ]

    set vwait 100
    after $vwait { exit }

    # bye bye!  we just killed ourselves
    return "OK"
}
################################################################################
proc SimSysReset { args } {
# can be called with zero or multiple arguments:
# SYSRESET
# SYSRESET TIME MM/DD/YYYY HH:MM:SS

# note: in real TCS, SYSRESET TIME only allowed if time is before Feb. 2013

    global simTime simTrack gobslong

    set return "OK"

    # strip off curly braces:
    set args [ string map { "{" "" "}" "" } $args ]
    set argc [ llength $args ]

    if {$args == ""} {

	set simTime(offset) 0
	SimInitializeTCS
	return $return
    }

    if {$argc >= 1} {
	set firstarg [ lindex $args 0 ]
    }

    if {$firstarg == "TIME"} {

	# we'll simulate a time change by keeping track of an offset between the true current
	# time and the desired time, and applying the offset when queried

	# first zero out the offset to have the present as a reference:
	set simTime(offset) 0
	SimInitializeTCS

	set timearg ""
	set datearg ""

	set datearg [ lindex $args 1 ]
	set timearg [ lindex $args 2 ]

	# calculate the requested date in seconds since 1970:

	set result [ clock scan "$datearg $timearg" -gmt 1 ] 

	# get the current time in seconds since 1970:
	set timenow [ GetTimeSeconds ]

	if {![ string is integer $result ]} {
	    return "FAILED"
	}

	# update the time:
	set timediff [ expr {int($result - $timenow)} ]
	set simTime(offset) $timediff

	# time is changed, go ahead with the initialization
	SimInitializeTCS

	return "OK"

    } else {

	return "UNKNOWN_CMD"
    }

}
################################################################################
proc SimSysSave {} {
    
    SimWriteFocusFile [ SimReturnFocus ]

    return "OK"
}
################################################################################
proc SimReturnSim {} {
# returns "SIM", when queried with "SIM" - TCS-NG will not know how to respond to 
# this query, so it's a way to tell if you're talking to a real NG system or a simulator

    return "SIM"
}
################################################################################
proc SimReturnTime {} {
# returns the UT time:

    global simTime

    set timenow [ clock seconds ]

    # apply the offset, if nonzero:

    if {[ info exists simTime(offset) ]} {

	set seconds [ expr {$timenow + $simTime(offset)} ]
	set newtime [ clock format "$seconds" -format "%T" -gmt 1 ]

	set simTime(currentUT) $newtime

    } else {

	set simTime(currentUT) [ clock format $timenow -format "%T" -gmt 1 ]
    }

    # NG returns time to hundredths of a second.  Add that here (note that hundredths place
    # is truncated, not rounded, but nobody should care):

    set frac ".[ string range [ clock clicks -milliseconds ] end-2 end-1 ]"
    append simTime(currentUT) $frac
    
    return $simTime(currentUT)
}
################################################################################
proc SimReturnDate {} {

    global simTime

    set timenow [ clock seconds ]

    if {[ info exists simTime(offset) ]} {

	set seconds [ expr {$timenow + $simTime(offset)} ]
	set date [ clock format "$seconds" -format "%D" -gmt 1 ]

    } else {

	set date [ clock format [ clock seconds ] -format "%D" -gmt 1 ]
    }

    return $date
}
################################################################################
proc SimReturnJD {} {

    global simJD

    set simJD [ format "%7.1f" [ GetJD ] ]
    
    return $simJD
}
################################################################################
proc SimReturnEpoch {} {

    global simEpoch

    return $simEpoch
}
################################################################################
proc SimReturnStow { req } {

    global stowFile focusFile

    if {$req == "focus"} {
	set inchannel [ open $focusFile r ]
	set line [ gets $inchannel ]
	return [ lindex $line 0 ]
    }

    set inchannel [ open $stowFile r ]
    
    while {![ eof $inchannel ]} {
	
	set line [ gets $inchannel ]
	set firstword [ lindex $line 0 ]
	set value [ lindex $line 1 ]

	if {$firstword == $req} {
	    close $inchannel
	    return $value
	}
    }
    
    return "UNKNOWN_CMD"

    close $inchannel
}
################################################################################
proc SimWriteStowFile { dome az el } {
    
    global stowFile

    set outchannel [ open $stowFile w+ ]
    puts $outchannel "dome    $dome"
    puts $outchannel "az      $az"
    puts $outchannel "el      $el"
    close $outchannel
}
################################################################################
proc SimWriteFocusFile { value } {

    global focusFile

    set outchannel [ open $focusFile w+ ]
    puts $outchannel $value
    close $outchannel
}
################################################################################
proc SimReturnHA {} {

    global simTime

    return $simTime(sexLHA)
}
################################################################################
proc SimReturnFocusSpeed {} {

    global simFocSpeed

    return $simFocSpeed
}
################################################################################
proc SimReturnRate { rate } {

    global simRate

    switch $rate {

	"guide" { set return $simRate(guide) }
	"drift" { set return $simRate(drift) }

	default { set return "FAILED" }
    }

    return $return
}
################################################################################
proc SimReturnPad {} {

}
################################################################################
proc SimSetFocusSpeed { speed } {

    global simFocSpeed simFocus

    if {$speed == "FAST"} {

	set simFocSpeed "FAST"
	set simFocus(rate) 100

    } else {

	# should really test to see if arg is "SLOW", but TCS doesn't so anything that 
	# is not fast is therefore slow

	set simFocSpeed "SLOW"
	set simFocus(rate) 2
    }

    # if a move was in progress, re-start the move with the new focus speed:
    SimFocusMove $simFocus(target) 0

    return "OK"
}
################################################################################
proc SimSetCorrections { correction state } {

# there appears to be no way to toggle the OBJECT corrections...what are these anyway?

    # if state is ON, turn the correction on; any other arg will disable

    global simCorr

    if {$state == "ON"} {
	set value 1
    } else {
	set value 0
    } 

    switch $correction {

	"PROPMO"   { set simCorr(propmo) $value }
	"PRECES"   { set simCorr(preces) $value }
	"NUTAT"    { set simCorr(nutat) $value }
	"ABERRATE" { set simCorr(aberrate) $value }
	"REFRAC"   { set simCorr(refrac) $value }
	"FLEX"     { set simCorr(flex) $value }
	"PARALLAX" { set simCorr(parallax) $value }
	"BIAS"     { set simCorr(bias) $value }

	default    { return "FAILED" }
    }

    return "OK"
}
################################################################################
proc SimReturnCorrections {} {
# returns a string indicating the status of various corrections. if all corrections 
# are active, the string will look like this: MPNARFp+tob if no corrections active,
# will look like this: _______+___

    global simTrack simCorr

    set corr ""

    # proper motion:
    if {$simCorr(propmo)}    { append corr M } else { append corr _ }
    # precession:
    if {$simCorr(preces)}    { append corr P } else { append corr _ }
    # nutation:
    if {$simCorr(nutat)}     { append corr N } else { append corr _ }
    # aberration:
    if {$simCorr(aberrate)}  { append corr A } else { append corr _ }
    # refraction:
    if {$simCorr(refrac)}    { append corr R } else { append corr _ }
    # flexure:
    if {$simCorr(flex)}      { append corr F } else { append corr _ }
    # parallax:
    if {$simCorr(parallax)}  { append corr p } else { append corr _ }
    # add a +
    append corr +
    # sidereal tracking:
    if {$simTrack(tracking)} { append corr t } else { append corr _ }
    # object rate:
    if {$simCorr(object)}    { append corr o } else { append corr _ }
    # bias rates active:
    if {$simCorr(bias)}      { append corr b } else { append corr _ }

    return $corr
}
################################################################################
proc SimReturnBeam {} {
# dummy proc: always return "A"

    return "A"
}
################################################################################
proc SimReturnMotion {} {
# TCS-NG encodes the motor motion status in an 8-bit integer:
# 64 = ??
# 32 = ??
# 16 = Derotator?
# 8  = Dome
# 4  = Focus
# 2  = Dec
# 1  = RA

    global simRA simDEC simDome simFocus

    set int 0

    if {$simRA(moving)} {
	incr int 1
    }
    if {$simDEC(moving)} {
	incr int 2
    }
    if {$simFocus(moving)} {
	incr int 4
    }
    if {$simDome(moving)} {
	incr int 8
    }

    # convert to hexadecimal (?)
    set int [ format "%x" $int ]

    return $int
}
################################################################################
proc SimReturnLimits {} {
# TCS-NG encodes the limit status in an 8-bit integer:
# 64 = focus high limit
# 32 = focus low limit
# 16 = horizon soft limit (ignoring this)
# 8  = horizon hard limit (use this)
# 4  = derotator limit (ignore)
# 2  = declination limit (high and low?)
# 1  = RA/HA limit (high and low?)

    global simLimit

    set int 0

    if {$simLimit(FOC+)} {
	incr int 64
    }
    if {$simLimit(FOC-)} {
	incr int 32
    }
    if {$simLimit(HOR)} {
	incr int 8
    }
    if {$simLimit(DEC)} {
	incr int 2
    }
    if {$simLimit(RA)} {
	incr int 1
    }

    set hex [ format "%x" $int ]

    # not sure what the second argument refers to:
    # "%d %d",limstatus(),inp(DIO_PortC1));

    return "$hex 0"
}
################################################################################
proc SimReturnLimitProf {} {

    global HorizonList

    set numentries [ llength $HorizonList ]
    set returnstring ""

    for {set i 0} {$i < $numentries} {incr i} {
	set index [ expr {$i * 2} ]
	append returnstring " [ lindex [ lindex $HorizonList $i ] 1 ]"
    }

    return $returnstring
}
################################################################################
proc SimReturnFlexFile {} {

    set returnstring "dummy pointing model text"

    return $returnstring
}
################################################################################
proc SimReturnPEC {} {

    global simPEC

    # TCS returns 4 PEC-related quantities: pec_condition, pec_count, pec_index, and pec_mode
    # pec_condition is the status: 0=OFF/1=ON/2=TRAINING/3=WAITING
    # pec_count - how many times the index pulse is passed - unimportant for simulator
    # pec_index is whether or not it's synced: 0=WAITING/1=INDEXED
    # pec_train is what it was commanded to do: 0=OFF/1=ON/2=TRAINING

    # Check and see if the count is high enough to be considered indexed...simulated shortcut, rather
    # than simulating an actual index point.  Once high enough, change condition from "waiting" to "on"

    # condition 3 = WAITING
    if {$simPEC(condition) == 3} {

	if {$simPEC(count) >= 10} {
	    set simPEC(index) 1
	    # condition 1 = ON
	    set simPEC(condition) 1
	}
    }

    return "$simPEC(condition) $simPEC(count) $simPEC(index) $simPEC(train)"
}
################################################################################
proc SimReturnFocus {} {

    global simFocus

    SimUpdateFocus

    return [ expr {int($simFocus(current))} ]
}
################################################################################
proc SimReturnDome { arg } {
# request for DOME to TCS returns 6 arguments: dome_del_pos, dome_mode, dome_initialized,
# tel_az, dome_az, and dome_home.  Dome mode is autodome on/off

#    Control Info: [DEL] [MOD] [INIT] [TELAZ] [AZ] [HOME]
#    DEL(Delta Position) = +XXX.XXXXXXX
#    MOD(Mode) = XX
#    INIT(Initialized) = XX
#    TELAZ(Telescope Azimuth) =  +XXX.XXXXXXX
#    AZ(Dome Azimuth) = +XXX.XXXXXXX
#    HOME(Home Position) = +XXX.XXXXXXX

# optional argument "PARAM" means to dump out the dome setup parameters:
#    Parameters: [CPD] [SD] [W] [SDW] [NU] [RHO] [PHI] [LOOK] [HOLD]
#    CPD(Counts Per Degree) = XXX.xxxxxxx
#    SD(Stow Degrees)= XXX.xxxxxxx
#    W(Dome Width) = XXX.xxxxxxx
#    SDW(Stow Dome Width)= XXX.xxxxxxx
#    NU = XXX.xxxxxxx
#    RHO = XXX.xxxxxxx
#    PHI = XXX.xxxxxxx
#    LOOK(Lookahead) = XX
#    HOLD(Hold Dome) = XX

    global simDome

    if {$arg == "PARAM"} {
	
	set return [ format "%10.6f %10.6f %10.6f %10.6f %10.6f %10.6f %10.6f %s %s" $simDome(cpd) $simDome(sd) $simDome(w) $simDome(sdw) $simDome(nu) $simDome(rho) $simDome(phi) $simDome(look) $simDome(hold) ]

    } else {

	SimUpdateDome

	set domeaz $simDome(current)
	set telaz [ SimReturnAZ ]
	set delta [ SimMinDist $domeaz $telaz ]
	
	set return [ format "%11.7f %s %s %11.7f %11.7f %11.7f" $delta $simDome(auto) 0 $telaz $domeaz 0 ]
    }

    return $return
}
################################################################################
proc SimReturnRA {} {
# returns sexagesimal RA

    global simRA

    SimUpdateRA
    set return [ dec2sex $simRA(current) 2 ]

    return $return
}
################################################################################
proc SimReturnDEC {} {
# returns sexagesimal DEC

    global simDEC

    SimUpdateDEC
    set return [ dec2sex $simDEC(current) 1 ]
    if {$simDEC(current) >= 0} {
	set return "+$return"
    }

    return $return
}
################################################################################
proc SimReturnAZ {} {

    set azel [ SimUpdateAZEL ]
    set az [ format "%0.1f" [ lindex $azel 0 ] ]

    return $az
}
################################################################################
proc SimReturnEL {} {

    set azel [ SimUpdateAZEL ]
    set el [ format "%0.1f" [ lindex $azel 1 ] ]

    return $el
}
################################################################################
proc SimReturnEQ {} {

    return "XXXXX.x"
}
################################################################################
proc SimReturnAirmass {} {

    set el [ SimReturnEL ]
    set airmass [ SimAirmass $el ]

    return $airmass
}
################################################################################
proc SimReturnDisable {} {

    global simDisabled

    return $simDisabled
}
################################################################################
proc SimReturnAll {} {

    global simLimit simTime simRA simDEC simDome simFocus simEpoch

#    set line1 "0         1         2         2         4         5         6         7         "
#    set line2 "01234567890123456789012345678901234567890123456789012345678901234567890123456789"

    # most values are updated only when requested...do this now:

    set ra  [ SimReturnRA ]
    set dec [ SimReturnDEC ]   
    # could call SimUpdateAZEL directly, to save us an radec2azel calculation...
    set az  [ SimReturnAZ ]
    set el  [ SimReturnEL ]

    set motion [ SimReturnMotion ]

    set airmass [ SimReturnAirmass ]

    set sexLST [ SimReturnLST ]

    set decimalLHA [ SimLHA $simTime(currentLST) $simRA(current) ]
    set sexLHA [ dec2sex $decimalLHA -1 ]
    if {$decimalLHA >= 0} {
	set sexLHA "+$sexLHA"
    }
    set simTime(sexLHA) $sexLHA
    set simTime(currentLHA) $decimalLHA
    
    # update to see if we're in a limit:
    set result [ SimCheckLimits $az $el $decimalLHA $simDEC(current) $simFocus(current) ]
    # if so, disable, unless limit override is actives
    if {$simLimit(RA) || $simLimit(DEC) || $simLimit(HOR)} {
	if {!$simLimit(inhibit)} {
	    SimDisable 1
	}
    }

    set epoch $simEpoch

#    set ngtext [ format "  %2s %-9s %-9s %-9s %-8s %5.2f %6.2f %5.2f %7.2f" $motion $ra $dec $sexLHA $sexLST $el $az $airmass $epoch ]
    set ngtext [ format "  %02s %-9s %-9s %-9s %-8s %5.2f %6.2f %5.2f %7.2f" $motion $ra $dec $sexLHA $sexLST $el $az $airmass $epoch ]

    return $ngtext
}
################################################################################
proc SimReturnXra {} {
# Returns: [COM] [NEXT] [REF] [OFF] [WOB] [DIFF] [BIAS] [GUIDE] [DRIFT]
#COM(Commanded Position) = HH:MM:SS.ss
#NEXT(Next Position)     = HH:MM:SS.ss
#REF(Reference Position) = HH:MM:SS.ss
#OFF(Offset Position)    = +HH:MM:SS.ss
#WOB(Wobble)             = +HH:MM:SS.ss
#DIFF(Difference)        = +XXXXXXXXX.xxx
#BIAS(Bias Rate)         = +XXXXXXXXX.xxx
#GUIDE(Guide Rate)       = +XXXXXXXXX.xxx
#DRIFT(Drift Rate)       = +XXXXXXXXX.xxx

    global simRA simRate

    # simulator doesn't keep track of everything; return placeholders for some values:

    set com [ dec2sex $simRA(current) ]
    set next [ dec2sex $simRA(next) ]
    set ref "ref"
    set off "off"
    set wob "wob"
    set diff "diff"
    set bias $simRate(biasra)
    set guide $simRate(guide)
    set drift $simRate(drift)

    set return [ format "%s %s %s %s %s %s %s %s %s" $com $next $ref $off $wob $diff $bias $guide $drift ]
}
################################################################################
proc SimReturnXdec {} {
# Returns: [COM] [NEXT] [REF] [OFF] [WOB] [DIFF] [BIAS] [GUIDE] [DRIFT]
#COM(Commanded Position) = HH:MM:SS.ss
#NEXT(Next Position)     = HH:MM:SS.ss
#REF(Reference Position) = HH:MM:SS.ss
#OFF(Offset Position)    = +HH:MM:SS.ss
#WOB(Wobble)             = +HH:MM:SS.ss
#DIFF(Difference)        = +XXXXXXXXX.xxx
#BIAS(Bias Rate)         = +XXXXXXXXX.xxx
#GUIDE(Guide Rate)       = +XXXXXXXXX.xxx
#DRIFT(Drift Rate)       = +XXXXXXXXX.xxx

    global simDEC simRate

    # simulator doesn't keep track of everything; return placeholders for some values:

    set com [ dec2sex $simDEC(current) ]
    set next [ dec2sex $simDEC(next) ]
    set ref "ref"
    set off "off"
    set wob "wob"
    set diff "diff"
    set bias $simRate(biasdec)
    set guide $simRate(guide)
    set drift $simRate(drift)

    set return [ format "%s %s %s %s %s %s %s %s %s" $com $next $ref $off $wob $diff $bias $guide $drift ]
}
################################################################################
proc SimReturnXall {} {

    global simDome simFocus

    SimUpdateFocus
    SimUpdateDome

    set focus $simFocus(current)
    set dome $simDome(current)

    # current version of TCS-NG actually has these values hard-coded in...TBD:

    set iis -224.4
    set pa -145.6

    set jd [ SimReturnJD ]
    set date [ SimReturnDate ]
    
    set return [ format "%7s %6.1f %6.1f %6.1f %10s %7.1f" $focus $dome $iis $pa $date $jd ] 

    return $return
}
################################################################################
proc GetFinishTime { start distance rate } {
# rate must be in units per second

    return [ expr {$start + abs($distance / ($rate * 1.0))} ]
}
################################################################################
proc GetTimeSeconds {} {
# returns number of seconds from 1970, with precision to milliseconds

    global simTime

    # need to report the TCS time, not necessarily the true time (if restarted
    # with SYSRESET TIME for example)

    set ms [ clock clicks -milliseconds ]

    if {[ info exists simTime(offset) ]} {
	set offset $simTime(offset)
    } else {
	set offset 0
    }

    set seconds [ expr {$offset + ($ms / 1000.0)} ] 

    return $seconds
}
################################################################################
proc UpdateTime { initialtime seconds } {
# initialtime comes to us in decimal hours; seconds in integer seconds

    return [ expr {$initialtime + ($seconds/3600.0)} ]    
}
################################################################################
proc SimDeclare { value } {
# valid values are either INITNEXT or INITCOM.

    global simRA simDEC simTrack simEL simAZ simDome

    if {$value != "INITCOM" && $value != "INITNEXT" && $value != "STOW"} {
	return "FAILED"
    }

    # INITCOM (set current to commanded) not very useful since (target) is always 
    # kept the same as (current) - even when tracking, stopping, etc.

    # don't do any syntax vetting - this should have been done by now

    if {$value == "INITCOM"} {

	if {$simRA(target) != ""} {
	    set simRA(current) $simRA(target)
	}
	if {$simDEC(target) != ""} {
	    set simDEC(current) $simDEC(target)
	}

    } elseif {$value == "INITNEXT"} {

	set simRA(current) $simRA(next)
	set simRA(target) $simRA(next)
	set simDEC(current) $simDEC(next)
	set simDEC(target) $simDEC(next)
	
	# workaround for declare as next position while not tracking:
	if {!$simTrack(tracking)} {
	    SimTracking "ON"
	    SimUpdateRA
	    SimTracking "OFF"
	}
    } elseif {$value == "STOW"} {
	# set the current position of the telescope (and dome?) to the stow position:
	set simDome(sd) $simDome(current)
	set simEL(stow) $simEL(current)
	set simAZ(stow) $simAZ(current)

	SimWriteStowFile $simDome(sd) $simAZ(stow) $simEL(stow) 
    }

    return "OK"
}
################################################################################
proc SimStepRA { value } {

    return "OK"
}
################################################################################
proc SimStepDEC { value } {

    return "OK"
}
################################################################################
proc SimPEC { value } {
# argument ON turns on the PEC, any other argument shuts it off
#    pec_condition -> 0=off, 1=on, 2=training, 3=waiting
#    pec_count -> decimal number of indexes
#    pec_index -> 0=waiting, 1=indexed
#    pec_train -> 0=off, 1=on, 2=training

    global simPEC

    if {$value == "ON"} {
	set simPEC(condition) 3
    } else {
	# turn PEC off and lose the index:
	set simPEC(condition) 0
	set simPEC(index) 0
	set simPEC(count) 0
    }

    return "OK"
}
################################################################################
proc SimUpdateTracking {} {
# should be called when stopping the tracking, also somewhat non-intuitively after
# arriving to an az-el with the tracking off (preset positions like Zenith)

    global simTrack simRA simPEC
    
    # not tracking...this means we need to "slew" slowly in RA/Dec space, to
    # keep az/el stationary.
    
    # get a time stamp:
    set simTrack(timeStart) [ GetTimeSeconds ]
    
    # destination: 12 hours away from where we are right now...we'll never get there so tracking won't stop
    set plus12 [ expr {$simRA(current) + 12} ]
    if {$plus12 > 24} {
	    set plus12 [ expr {$plus12 - 24} ]
	}
    set simTrack(target) $plus12
    set simTrack(distance) 180
    
    # remember where and when we started from
    set simTrack(original) $simRA(current)
	
    # predict when we should finish:
    set simTrack(timeFinish) [ GetFinishTime $simTrack(timeStart) $simTrack(distance) $simTrack(rate) ]

}
################################################################################
proc SimTracking { value } {
# argument ON turns tracking on, all other args turns tracking off
# note that TCS-NG has RA motion bit = 1 if tracking...change from legacy?

# we're going to treat *not tracking* kind of like a move in RA.

    global simRA simTrack simDisabled

    if {$value == "ON" && !$simDisabled} {

	set simTrack(tracking) $value

    } else {

	set simTrack(tracking) 0
	if {!$simRA(slewing)} {
	    set simRA(moving) 0
	}

	# TCS-NG undesirable behavior: turning tracking off also turns off PEC and autodome:
#	SimPEC OFF
    }

    # if we're not tracking, then update the RA/DEC:
    if {!$simTrack(tracking)} {
	SimUpdateTracking
    }

    return "OK"
}
################################################################################
proc SimReturnLST {} {
# instead of re-calculating the LST every time we update (which is expensive), just add
# the number of sidereal seconds since we started the simulator.
    
    global simTime

    # ratio of calendar to sidereal time:
    set siderealFactor 1.002737896
    
    set timeNow [ GetTimeSeconds ]
    set timeDiff [ expr {$siderealFactor * ($timeNow - $simTime(zero))} ]
    
    set simTime(currentGMST) [ UpdateTime $simTime(initialGMST) $timeDiff ]
    set simTime(currentLST) [ expr {$simTime(currentGMST) + $simTime(offset2)} ]

    # make sure we don't end up with a negative LST:
    if {$simTime(currentLST) < 0} {
	set simTime(currentLST) [ expr {$simTime(currentLST) + 24} ]
    }
    # or an LST greater than 24h:
    if {$simTime(currentLST) > 24} {
	set simTime(currentLST) [ expr {$simTime(currentLST) - 24} ]
    }

    set simTime(sexLST) [ dec2sex $simTime(currentLST) -1 ]
    
    return $simTime(sexLST)
}
################################################################################
proc SimUpdateAZEL {} {
# assume we are moving in RA/DEC and need to update the corresponding AZ/EL

    global simAZ simEL simRA simDEC

    set azel [ TCSradec2azel $simRA(current) $simDEC(current) ]

    set az [ lindex $azel 0 ]
    set el [ lindex $azel 1 ]

    set simAZ(current) $az
    set simEL(current) $el

    return "$az $el"
}
################################################################################
proc SimStow {} {

    global simEL simAZ

    set return [ SimSlewElAz $simEL(stow) $simAZ(stow) "" ]

    return $return
}
################################################################################
proc SimSlewElAz { el az trackon } {

    global simTime gobslat gobslong telemNextRA telemNextDEC simDisabled

    # basic checks:
    if {$el > 90 || $el < 0} {
	return "FAILED"
    }
    if {$az < 0 || $az > 360} {
	return "FAILED"
    }
    
    if {$trackon != "TRACKON" && $trackon != ""} {
	return "FAILED"
    }

    # note: if we are turning or leaving tracking on, we will never arrive to the exact
    # az,el commanded, since we will have tracked away from it by the time the telescope arrives.
    # this might be a problem when going to STOW, for example

    # we have received a command to go to an az,el position, but this is an equatorial
    # telescope.  Therefore, figure out what the corresponding RA,Dec is at this time
    # and go there instead.

    set radec [ e2h $el $az $simTime(currentGMST) $gobslat $gobslong ]
    set ra [ lindex $radec 0 ]
    set dec [ lindex $radec 1 ]

    set sexRA [ dec2sex $ra 2 ]
    set sexDEC [ dec2sex $dec ]
    if {$dec >= 0} {
	set sexDEC "+$sexDEC"
    }

    # slew!
    set result [ SimSlewRADec $sexRA $sexDEC ]

    # shut off the tracking if not requested to turn/leave on:
    if {$trackon == ""} {
	SimTracking OFF    
    } else {
	SimTracking ON
    }

    return $result
}
################################################################################
proc SimSlewNext {} {

    global simRA simDEC

    SimSlewRADec [ dec2sex $simRA(next) ] [ dec2sex $simDEC(next) ]
}
################################################################################
proc SimVerify { args } {

    global simTime simFocus

    # strip off curly braces:
    set args [ string map { "{" "" "}" "" } $args ]

    # better have 3 things in args: RA, Dec and epoch:
    set ra [ lindex $args 0 ]
    set dec [ lindex $args 1 ]
    set epoch [ lindex $args 2 ]

    # convert to decimal degrees:

    set ra [ sex2dec $ra ]
    set dec [ sex2dec $dec ]

    # simulate the TCS returns for various failures in parsing:
    if {$ra == -1 || $ra == -2 || $ra == -3} {
	return $ra
    }
    if {$dec == -1 || $dec == -2 || $dec == -3} {
	return [ expr {$dec - 3} ]
    }

    # check that we're not being told to do something dumb:

    set targetAZEL [ TCSradec2azel $ra $dec ]
    set targetAZ [ lindex $targetAZEL 0 ]
    set targetEL [ lindex $targetAZEL 1 ]
    set targetLHA [ SimLHA $simTime(currentLST) $ra ]

    set return 1
    set result [ SimCheckLimits $targetAZ $targetEL $targetLHA $dec $simFocus(current) target ]

    if {$result != "OK"} {

	set return 0
    }

    return $return
}
################################################################################
proc SimSlewRADec { targetRA targetDEC } {

    global simRA simDEC simDome simAZ simEL simFocus simTime telemNextRA telemNextDEC
    global gobslat gobslong simDisabled verbose

    # original MOVRADEC command had RA, Dec, Epoch, and 2 proper motion args, but the 
    # simulator ignores epoch and proper motions, hence only 2 args here

    # don't go anywhere if we're disabled, but acknowledge the command:
    if {$simDisabled} {
	return "OK"
    }

    # convert to decimal degrees:

    set targetRA [ sex2dec $targetRA ]
    set targetDEC [ sex2dec $targetDEC ]

    if {$targetRA < 0 || $targetRA > 24} {
	if {$verbose} {
	    puts "SIM: SimSlewRADec: RA OUT OF RANGE: $targetRA"
	}
	return "FAILED"
    }
    if {$targetDEC < -90 || $targetDEC > 90} {
	if {$verbose} {
	    puts "SIM: SimSlewRADec: DEC OUT OF RANGE: $targetDEC"
	}
	return "FAILED"
    }

    # check that we're not being told to do something dumb:

    set targetAZEL [ TCSradec2azel $targetRA $targetDEC ]
    set targetAZ [ lindex $targetAZEL 0 ]
    set targetEL [ lindex $targetAZEL 1 ]
    set targetLHA [ SimLHA $simTime(currentLST) $targetRA ]

    set result [ SimCheckLimits $targetAZ $targetEL $targetLHA $targetDEC $simFocus(current) target ]

    # if we're commanded to go into a limit, refuse:
    if {$result != "OK"} {
	if {$verbose} {
	    puts "SIM: SimSlewRADec: result from SimCheckLimits = '$result'"
	}
	return "FAILED"
    }

    # set the target azimuth + elevation for autodome:
    set simAZ(target) $targetAZ
    set simEL(target) $targetEL

    # update the position of the dome if we should:
    if {$simDome(auto)} {
	if {$simDome(look)} {
	    SimAutoDomeUpdate $simEL(target) $simAZ(target)
	} else {
	    SimAutoDomeUpdate $simEL(current) $simAZ(current)
	}
    }

    # if we're slewing to an RA/Dec, then turn the tracking and PEC on if not on already:

    SimTracking ON

    set timeNow [ GetTimeSeconds ]

    # Right Ascension:

    # first thing we do is stop any moves in progress.  This will also update the current position
    # to the moment of stopping
    SimStop "ra"
    # our target:
    set simRA(target) $targetRA
    # get a time stamp and indicate that we're moving:
    set simRA(timeStart) $timeNow
    set simRA(moving) 1
    set simRA(slewing) 1
    # remember where and when we started from
    set simRA(original) $simRA(current)
    # figure out how far and in which direction we need to go:
    set simRA(distance) [ SimMinDist $simRA(current) $simRA(target) 24 ]
    # predict when we should finish:
    set simRA(timeFinish) [ GetFinishTime $simRA(timeStart) $simRA(distance) $simRA(rate) ]

    # Declination:

    # stop the DEC:
    SimStop "dec"
    # our target:
    set simDEC(target) $targetDEC
    # get a time stamp and indicate that we're moving the dome:
    set simDEC(timeStart) $timeNow
    set simDEC(moving) 1
    # remember where and when we started from
    set simDEC(original) $simDEC(current)
    # figure out how far and in which direction we need to go:
    set simDEC(distance) [ SimMinDist $simDEC(current) $simDEC(target) -1 ]
    # predict when we should finish:
    set simDEC(timeFinish) [ GetFinishTime $simDEC(timeStart) $simDEC(distance) $simDEC(rate) ]

    # setup is done, and the move is now virtually underway!  The position will be updated
    # only when queried

    if {!$simDome(auto)} {
	SimStop "dome"
    } else {

	# set the target azimuth for autodome:
	set simAZ(target) $targetAZ
	set simEL(target) $targetEL

	# update the position of the dome if we should:
	if {$simDome(auto)} {
	    if {$simDome(look)} {
		SimAutoDomeUpdate $simEL(target) $simAZ(target)
	    } else {
		SimAutoDomeUpdate $simEL(current) $simAZ(current)
	    }
	}
    }

    return "OK"
}
################################################################################
proc SimSlewSolarSystem { object } {
# simulate slewing to a solar system object.  TCS knows about:
# SUN MOON MERCURY VENUS MARS JUPITER SATURN URANUS NEPTUNE PLUTO

    global simDisabled

    # don't go anywhere if we're disabled, but acknowledge the command:
    if {$simDisabled} {
	return "OK"
    }

    # we're not going to calculate ephemerides...just pick a random RA, Dec and go
    # constrain DEC between -20 and +20

    set randRA [ expr { rand() * 24.0 } ]
    set randDEC [ expr { (rand() * 40) - 20 } ]

    set result [ SimSlewRADec [ dec2sex $randRA ] [ dec2sex $randDEC ] ]
    
    return $result
}
################################################################################
proc SimSlewCatalog { catalog object } {

    # check to see if we have this catalog:

    set catfile "[ string tolower $catalog ].cat"

    if {![ file exists $catfile ]} {
	return "FAILED"
    }

    # got the catalog, now pull out the object:
    set inchannel [ open $catfile r ]
    set line [ gets $inchannel ]

    while {![ eof $inchannel ]} {
	
	scan $line "%s %s %s %s %s %s %s" name ra dec epoch pmra pmdec mag

	if {$name == $object} {
	    # we have a match!

	    set result [ SimSlewRADec $ra $dec ]
	    return "OK"
	}

	set line [ gets $inchannel ]
    }

    # we got to the end without finding a match...shouldn't happen!

    return "FAILED"
}
################################################################################
proc SimUpdateRA {} {
# updates RA position if telescope is slewing or not tracking

# if tracking, checks to see if autodome needs to adjust the dome

    global simRA simDEC simAZ simEL simTrack simDome simDisabled simPEC

    # two conditions for updating the RA: slewing or not tracking

    # only tracking:

    if {!$simRA(slewing) && $simTrack(tracking)} {

	# don't need to update the RA, but need to update the targetAZ:
	# this is where autodome gets its current information about the targetAZ, when
	# tracking but not slewing:
	
	if {$simDome(auto) && !$simDEC(moving)} {

	    set targetAZEL [ TCSradec2azel $simRA(current) $simDEC(current) ]
	    set targetAZ [ lindex $targetAZEL 0 ]
	    set targetEL [ lindex $targetAZEL 1 ]

	    # don't care about lookahead here, since we know we're not slewing, only tracking.
	    SimAutoDomeUpdate $targetEL $targetAZ
	}

	# sneak in a PEC count update if PEC is on:
	if {$simPEC(condition) == 1 || $simPEC(condition) == 3} {
	    incr simPEC(count)
	    if {$simPEC(count) > 999} {
		set simPEC(count) 0
	    }
	}

	return
    }

    # we're either slewing or not tracking...proceed with updating the RA coords:

    set timeNow [ GetTimeSeconds ]
    
    # slewing and not tracking are mutually exclusive, i.e. only do one case.  If both are true,
    # then only deal with slewing
    
    # slewing but not tracking:

    if {$simRA(slewing)} {

	# if we're commanded to slew to where we already are, might get a divide-by-zero error:
	set timetofinish [ expr {$simRA(timeFinish) - $simRA(timeStart)} ]

	# if we have less than a hundredth of a second to go, then say we're there:
	if {$timetofinish < 0.01} {
	    set elapsedFraction 1
	} else {
	    # otherwise, we're in the middle of a move.  See how far along we are:
	    set elapsedFraction [ expr {($timeNow - $simRA(timeStart)) / $timetofinish} ]
	}
	if {$elapsedFraction > 1} {
	    # the move already completed when we weren't looking, but don't overshoot
	    set elapsedFraction 1
	}
	
	# update the position:
	set current [ expr {$simRA(original) + ($elapsedFraction * $simRA(distance))} ]
	
	# also update autodome position:
	if {$simDome(auto) && !$simDEC(moving)} {
	    
	    SimAutoDomeUpdate $simEL(target) $simAZ(target)
	}

	# check to see if we wrapped:
	if {$current >= 24} {
	    set current [ expr {$current - 24} ]
	} elseif {$current < 0} {
	    set current [ expr {$current + 24} ]
	}
	set simRA(current) $current
	
	# did we make it?  fuzzy matching to find out:
	set arcsec 0.0000185185
	set inpos [ SimInPos $simRA(current) $simRA(target) 24 $arcsec ]
	
	if {$inpos} {
	    # yes! re-initialize to get ready for the next move command.
	    set simRA(original) $simRA(current)

	    set simRA(moving) 0
	    set simRA(slewing) 0

	    # update some tracking-related variables:
	    SimUpdateTracking
	}

    # not-tracking case:
	
    } elseif {!$simTrack(tracking)} {

	# need to handle the case where we stopped a slew here:

	set elapsedFraction [ expr {($timeNow - $simTrack(timeStart)) / ($simTrack(timeFinish) - $simTrack(timeStart))} ]
	set current [ expr {$simTrack(original) + ($elapsedFraction * $simTrack(distance))} ]
	
	# check to see if we wrapped:
	if {$current >= 24} {
	    set current [ expr {$current - 24} ]
	} elseif {$current < 0} {
	    set current [ expr {$current + 24} ]
	}
	set simRA(current) $current
    }
}
################################################################################
proc SimUpdateDEC {} {

    global simDEC

    # if we're not moving, then there is nothing to update
    if {!$simDEC(moving)} {
	return
    }

    # otherwise, we're in the middle of a move.  See how far along we are:
    set timeNow [ GetTimeSeconds ]

    # if we're commanded to slew to where we already are, might get a divide-by-zero error:
    set timetofinish [ expr {$simDEC(timeFinish) - $simDEC(timeStart)} ]
    
    # if we have less than a hundredth of a second to go, then say we're there:
    if {$timetofinish < 0.01} {
	set elapsedFraction 1
    } else {
	# otherwise, we're in the middle of a move.  See how far along we are:
	set elapsedFraction [ expr {($timeNow - $simDEC(timeStart)) / $timetofinish} ]
    }
    if {$elapsedFraction > 1} {
	# the move already completed when we weren't looking, but don't overshoot
	set elapsedFraction 1
    }
    
    # update the position:
    set simDEC(current) [ expr {($simDEC(original) + ($elapsedFraction * $simDEC(distance)))} ]

    # did we make it?  need to use some fuzzy matching to be sure:
    set arcsec [ expr {1 / 3600.0} ]
    set inpos [ SimInPos $simDEC(current) $simDEC(target) -1 $arcsec ]

    if {$inpos} {
	# yes! re-initialize to get ready for the next move command.
	set simDEC(original) $simDEC(current)
	set simDEC(moving) 0
    }
}
################################################################################
proc SimSetEpoch { value } {
# this is essentially a dummy procedure, since we will not use the Epoch to precess
# coordinates.  Simply required because TCS requires it.

    global simEpoch

    if {![ string is double $value ]} {
	return "FAILED"
    }
    if {$value < 1900 || $value > 2100} {
	return "FAILED"
    }

    set simEpoch [ format "%0.1f" $value ]    

    return "OK"
}
################################################################################
proc SimSetNextRA { value } {

    global simRA

    # value should be a valid sexagesimal string...let's make sure.
    set decimal [ sex2dec $value ]

    if {$decimal == -1 || $decimal == -2 || $decimal == -3} {
	return "FAILED"
    }

    # however simRA(next) needs to be decimal:
    set simRA(next) $decimal

    return "OK"
}
################################################################################
proc SimSetNextDec { value } {

    global simDEC

    # value should be a valid sexagesimal string...let's make sure.
    set decimal [ sex2dec $value ]

    if {$decimal == -1 || $decimal == -2 || $decimal == -3} {
	return "FAILED"
    }

    # however simDEC(next) needs to be decimal:
    set simDEC(next) $decimal

    return "OK"
}
################################################################################
proc SimSetNextPos { RA Dec } {

    set return [SimSetNextRA  $RA]
    if {$return != "OK"} {
      return $return
    }
    set return [SimSetNextDec $Dec]
    if {$return != "OK"} {
      return $return
    }

    return "OK"
}
################################################################################
proc SimAirmass { el } {
# algorithm from Pickering (2002):

    # we might have a negative elevation if declared that way...don't break downstream code:
    if {$el < 0} {
	return "99"
    }

    set radel [ deg2rad $el ]

    set degnum [ expr {$el + 244/(165 + pow((47*$el),1.1))} ]
    set radnum [ deg2rad $degnum ]
    set am [ expr {1 / (sin($radnum))} ]

    return [ format "%0.2f" $am ]
}
################################################################################
proc SimLHA { lst ra } {

    set decimalLST $lst
    set decimalRA $ra

    set decimalHA [ expr {$decimalLST - $decimalRA} ]

    # detect 0/24h boundary crossings:

    if {$decimalHA > 12} {
	set decimalHA [ expr {$decimalHA - 24} ]
    }
    if {$decimalHA < -12} {
	set decimalHA [ expr {$decimalHA + 24} ]
    }    
    
    return $decimalHA
}
################################################################################
proc dec2sex { decvalue { decimalplaces 1 } } {

    set isneg 0

    if {$decvalue < 0.0 } {
        set isneg 1
	set decvalue [expr {$decvalue * -1} ]
    }

    set place1 [ expr {int($decvalue)} ]
    set decvalue [ expr {$decvalue - $place1} ]
    set decvalue [ expr {$decvalue * 60.0} ]
    set place2 [ expr {int($decvalue)} ]
    set decvalue [ expr {$decvalue - $place2} ]
    set place3 [ expr {$decvalue * 60.0} ]
    set place1 [ expr {$place1} ]

    if {$place3 > 59.9999 && $place3 <= 60} {
	set place3 "59.9999"
    }

    # now build up the value

    if {$isneg} {
        set result [format {-%2.2d:%2.2d:%07.4f} $place1 $place2 $place3]
    } else {
        set result [format {%2.2d:%2.2d:%07.4f} $place1 $place2 $place3]
    }

    # we formatted to avoid round off, now trim to one decimal place

    set len [string length $result]
    set result [string range $result 0 [ expr {$len - 5 + $decimalplaces}]]

    return $result
}
################################################################################
proc SimUpdateFocus {} {

    global simFocus

    # if we're not moving, then there is nothing to update
    if {!$simFocus(moving)} {
	return
    }

    # otherwise, we're in the middle of a move.  See how far along we are:
    set timeNow [ GetTimeSeconds ]
    set elapsedFraction [ expr {($timeNow - $simFocus(timeStart)) / ($simFocus(timeFinish) - $simFocus(timeStart))} ]
    if {$elapsedFraction > 1} {
	# the move already completed when we weren't looking, but don't overshoot
	set elapsedFraction 1
    }
    
    set simFocus(current) [ expr {($simFocus(original) + ($elapsedFraction * $simFocus(distance)))} ]

    # did we make it?
    if {$simFocus(current) == $simFocus(target)} {
	# yes! re-initialize to get ready for the next move command.
	set simFocus(original) $simFocus(current)
	set simFocus(moving) 0
    }
    # no need to check boundaries since focus doesn't wrap
}
################################################################################
proc SimUpdateDome {} {

    global simDome simAZ

    # if we're not moving, then there is nothing to update
    if {!$simDome(moving)} {
	return
    }

    # otherwise, we're in the middle of a move.  See how far along we are:
    set timeNow [ GetTimeSeconds ]
    set elapsedFraction [ expr {($timeNow - $simDome(timeStart)) / ($simDome(timeFinish) - $simDome(timeStart))} ]
    if {$elapsedFraction > 1} {
	# the move already completed when we weren't looking, but don't overshoot
	set elapsedFraction 1
    }
    
    # update the position:
    set current [ expr {$simDome(original) + ($elapsedFraction * $simDome(distance))} ]

    # check to see if we wrapped:
    if {$current >= 360} {
	set current [ expr {$current - 360} ]
    } elseif {$current < 0} {
	set current [ expr {$current + 360} ]
    }
    set simDome(current) $current

    # did we make it?
    
    # make sure we're there to within a degree:
    if {[ expr {int($simDome(current))} ] == [ expr {int($simDome(target))} ] } {

	# yes! re-initialize to get ready for the next move command.
	set simDome(original) $simDome(current)
	set simDome(moving) 0
    }
}
################################################################################
proc SimPaddle { value } {

    global simPaddle

    if {$value == "ON"} {
	set simPaddle 1
    } else {
	set simPaddle 0
    }

    return "OK"
}
################################################################################
proc SimPadGuide { value } {

    global simRate

    if {![ string is double $value ]} {
	return "FAILED"
    }

    set simRate(guide) $value

    return "OK"
}
################################################################################
proc SimSetRate { rate value } {

    global simRate

    if {![ string is double $value ]} {
	return "FAILED"
    }

    if {$rate != "guide" && $rate != "drift" && $rate != "biasra" && $rate != "biasdec"} {
	return "FAILED"
    }

    set simRate($rate) $value

    return "OK"
}
################################################################################
proc SimPadDrift { value } {

    global simRate

    if {![ string is double $value ]} {
	return "FAILED"
    }

    set simRate(drift) $value

    return "OK"
}
################################################################################
proc SimFocusStop {} {

    SimStop "focus"

    return "OK"
}
################################################################################
proc SimFocusPaddle { value } {
# might get a FOCUP, FOCDN or FOCSTOP from the paddle

    global simPaddle

    if {!$simPaddle} {
	return "FAILED"
    }

    if {$value == "FOCUP"} {
	SimFocusMove "max" 0
    } elseif {$value == "FOCDN"} {
	SimFocusMove "min" 0
    } elseif {$value == "FOCSTOP"} {
	SimFocusStop
    } else {
	return "UNKNOWN_CMD"
    }

    return "OK"
}
################################################################################
proc SimFocusMove { value relative } {

    global simFocus limitFocLow limitFocHigh

    if {$value == "max"} {
	set value $limitFocHigh
    } elseif {$value == "min"} {
	set value $limitFocLow
    }
    
    # round off to nearest int if a double:
    if {[ string is double $value ]} {
	set value [ expr {int($value)} ]
    }

    # basic checks on value and relative:
    if {![ string is integer $value ]} {
	return "FAILED"
    }
    if {$relative != 0 && $relative != 1} {
	return "FAILED"
    }

    # first thing we do is stop any moves in progress.  This will also update the current position
    # to the moment of stopping
    SimStop "focus"

    # get a time stamp and indicate that we're moving the focus:
    set simFocus(timeStart) [ GetTimeSeconds ]
    set simFocus(moving) 1

    if {$relative} {
	set simFocus(target) [ expr {$simFocus(current) + $value} ]
    } else {
	set simFocus(target) $value
    }

    # remember where and when we started from
    set simFocus(original) $simFocus(current)

    # figure out how far and in which direction we need to go:
    set simFocus(distance) [ SimMinDist $simFocus(current) $simFocus(target) -1 ]

    # predict when we should finish:
    set simFocus(timeFinish) [ GetFinishTime $simFocus(timeStart) $simFocus(distance) $simFocus(rate) ]

    # setup is done, and the move is now virtually underway!  The position will be updated
    # only when focus is queried

    return "OK"
}
################################################################################
proc SimZeroFocus {} {

    global simFocus

    set simFocus(current) 0

    return "OK"
}
################################################################################
proc SimLimitInhibit { value1 } {
# Limit override >> USE WITH EXTREME CAUTION!!!!!
#    Args: "INHIBIT" will override limits, all other strings will enable limits
#    Returns: "OK" or "FAILED"

    global simLimit

    if {$value1 == "INHIBIT"} {
	set simLimit(inhibit) 1
    } else {
	set simLimit(inhibit) 0
    }

    return "OK"
}
################################################################################
proc SimDomeParse { value1 value2 value3 } {

    # moving dome directly is handled with the "DOMEGOTO" command

    switch $value1 {

	"AUTO"      { set return [ SimAutoDome $value2 ] }
	"INIT"      { set return [ SimDomeInit ] }
	"STOW"      { set return [ SimDomeStow ] }
	"LOOKAHEAD" { set return [ SimDomeLookahead $value2 ] }
	"PARAM"     { set return [ SimDomeSetParam $value2 $value3 ] }
	"PADDLE"    { set return [ SimDomePaddle $value2 ] }

	default     { set return "FAILED" }
    }

    return $return
}
################################################################################
proc SimDomeInit {} {

    global simDome
    
    set simDome(current) $simDome(sd)
    set simDome(init) 1

    return "OK"
}
################################################################################
proc SimDomeStow {} {
    
    global simDome

    set simDome(auto) 0
    SimDomeMove $simDome(sd)

    return "OK"
}
################################################################################
proc SimDomePaddle { value } {

    global simDome

    if {$value == "RIGHT" || $value == "LEFT"} {

	SimDomeMove $value

    } else {
	# stop the dome motion:

	SimStop "dome"
    }

    return "OK"
}
################################################################################
proc SimDomeSetParam { kw val } {
#    PARAMSet Dome Parameters
#    Args: [CPD] [SD] [W] [SDW] [NU] [RHO] [PHI] [LOOK] [HOLD]
#    CPD(Counts Per Degree) = XXX.xxxxxxx
#    SD(Stow Degrees)= XXX.xxxxxxx
#    W(Dome Width) = XXX.xxxxxxx
#    SDW(Stow Dome Width)= XXX.xxxxxxx
#    NU = XXX.xxxxxxx
#    RHO = XXX.xxxxxxx
#    PHI = XXX.xxxxxxx
#    LOOK(Lookahead) = XX
#    HOLD(Hold Dome) = XX
#    Returns:   "OK" or "FAILED"

    global simDome

    switch $kw {

	"CPD"  { set simDome(cpd) $val }
	"SD"   { set simDome(sd) $val }
	"W"    { set simDome(w) $val }
	"SDW"  { set simDome(sdw) $val }
	"NU"   { set simDome(nu) $val }
	"RHO"  { set simDome(rho) $val }
	"PHI"  { set simDome(phi) $val }
	"LOOK" { set simDome(look) $val }
	"HOLD" { set simDome(hold) $val }

	default { return "FAILED" }
    }
    
    return "OK"
}
################################################################################
proc SimDomeLookahead { value } {
# any positive nonzero number = enable, anything else = disable    

    global simDome

    if {$value == ""} {
	return "UNKNOWN_CMD"
    }

    if {![ string is integer $value ] && ![ string is double $value ]} {
	return "FAILED"
    }

    if {[ string is integer $value ] && $value >= 1} {
	set simDome(look) 1
    } else {
	set simDome(look) 0
    }

    return "OK"
}
################################################################################
proc SimDomeMove { value } {
# value will be either an azimuth, or LEFT or RIGHT if coming from the paddle

    global simDome

    # first thing we do is stop any moves in progress.  This will also update the current position
    # to the moment of stopping
    SimStop "dome"

    # if this is a paddle command, come up with a new target azimuth...this is a shortcut,
    # only allowing moves of up to 179 deg. in azimuth from the paddle...
    if {$value == "LEFT"} {
	set value [ expr {$simDome(current) - 179} ]
    } elseif {$value == "RIGHT"} {
	set value [ expr {$simDome(current) + 179} ]
    }

    # enforce azimuth between 0 and 360
    if {$value < 0} {
	set value [ expr {$value + 360} ]
    } elseif {$value > 360} {
	set value [ expr {$value - 360} ]
    }

    # get a time stamp and indicate that we're moving the dome:
    set simDome(timeStart) [ GetTimeSeconds ]
    set simDome(moving) 1

    # destination:
    set simDome(target) $value

    # remember where and when we started from
    set simDome(original) $simDome(current)

    # figure out how far and in which direction we need to go:
    set simDome(distance) [ SimMinDist $simDome(current) $simDome(target) 360 ]

    # predict when we should finish:
    set simDome(timeFinish) [ GetFinishTime $simDome(timeStart) $simDome(distance) $simDome(rate) ]

    # setup is done, and the move is now virtually underway!  The position will be updated
    # only when queried

    return "OK"
}
################################################################################
proc SimAutoDome { value } {
# ON = enable, anything else = disable

    global simDome simAZ simEL simRA simDEC simTrack

    if {$value == "ON"} {
	set simDome(auto) 1
    } else {
	set simDome(auto) 0
    }
    if {$simDome(auto)} {

	# if we're slewing, go to the telescope's destination az:
	if {$simDome(look)} {
	    SimAutoDomeUpdate $simEL(target) $simAZ(target)
	} else {
	    # otherwise, we are either tracking or stationary: go to the position for the telescope's current AZ
	    SimAutoDomeUpdate $simEL(current) $simAZ(current)
	}
    } else {
	# stop the motion of the dome, if it's in motion:
	SimStop "dome"
    }

    return "OK"
}
################################################################################
proc SimAutoDomeUpdate { telel telaz } {
# checks to see if the dome is out of position, and updates it if so

    global simDome

    # if autodome isn't engaged, we have nothing to do:
    if {!$simDome(auto)} {
	return
    }

    # we're passed el, az of the current or target telescope position - can't just match
    # azimuths, need to consider dome nu rho phi parameters:

    set commanded [ DomeIntersect $simDome(rho) $simDome(phi) $simDome(nu) 1 $telaz $telel domeAz domeEl ]
    set domeoff [ expr {abs([ SimMinDist $simDome(current) $domeAz 360 ])} ]

    if {$domeoff > $simDome(autotol)} {
	SimDomeMove $domeAz
    }    
}
################################################################################
proc SimMinDist { p1 p2 { bound 360 } } {

    # returns shortest path between two points, accounting for boundary wrap.
    # if no boundary wrap (i.e. focus), use -1 for 3rd argument

    # shortest can be positive or negative, with the sign indicating the direction
    # of the shortest path.  use caution when testing > or < against return value

    set diff [ expr {$p2 - $p1} ]

    if {$bound == -1} {
	return $diff
    }
    
    set halfbound [ expr {$bound / 2} ]
    set neghalfbound [ expr {$halfbound * -1} ]

    if {$diff > $halfbound} {
	set dist [ expr {$diff - $bound} ]
    } elseif {$diff < $neghalfbound} {
	set dist [ expr {$diff + $bound} ]
    } else {
	set dist [ expr {$diff} ]
    }

    return $dist
}
################################################################################
proc SimInPos { p1 p2 bound tol } {

    set return 0

    set mindist [ SimMinDist $p1 $p2 $bound ]

    # return from SimMinDist could be positive or negative...we care about absolute
    # distance in this case.

    if {[ expr {abs($mindist)} ] <= $tol} {
	set return 1
    }
    return $return
}
################################################################################
proc SimDisable { value } {

    global simDisabled

    if {$value != 1 && $value != 0} {
	return "FAILED"
    }
    set simDisabled $value

    # stop the RA and Dec, and turn off tracking and PEC if we're disabling:
    if {$value == 1} {

	SimStop "ra"
	SimStop "dec"
	SimStop "focus"

	SimTracking OFF
    }

    return "OK"
}
################################################################################
proc SimStop { { system "all" } } {

    global simAZ simEL simFocus simDome simRA simDEC simTrack simDisabled

    # stop the RA if requested:
    if {$system == "ra" || $system == "all"} {
	# one last update to get the current position, before we kill the move:
	SimUpdateRA
	# now indicate that we are not moving, and bring the target and current into sync:
	set simRA(moving) 0
	set simRA(slewing) 0
	if {$simTrack(tracking) && !$simDisabled} {
	    set simRA(moving) 0
	} else {
	    # this is necessary to get the ra position correct when stopping a slew and not tracking:
	    SimUpdateTracking
	}
	set simRA(target) $simRA(current)
    }
    # stop the DEC if requested:
    if {$system == "dec" || $system == "all"} {
	if {$simDEC(moving)} {
	    SimUpdateDEC
	    set simDEC(moving) 0	
	    set simDEC(target) $simDEC(current)
	}
    }
    # stop the dome if requested:
    if {$system == "dome" || $system == "all"} {
	if {$simDome(moving)} {
	    SimUpdateDome
	    set simDome(moving) 0
	    set simDome(target) $simDome(current)
	}
    }
    # stop the focus if requested:
    if {$system == "focus" || $system == "all"} {
	if {$simFocus(moving)} {
	    SimUpdateFocus
	    set simFocus(moving) 0
	    set simFocus(target) $simFocus(current)
	}
    }
    # if we're told to cancel a move, update the target az for autodome's sake:
    if {$system == "all"} {
	SimUpdateAZEL
	set simAZ(target) $simAZ(current)
	set simEL(target) $simEL(current)
    }
}
################################################################################
proc SimReturnLimitInhibit {} {

    global simLimit

    return $simLimit(inhibit)
}
################################################################################
proc SimCheckLimits { testAZ testEL testHA testDEC testFOC { state "current" } } {
# default is to check the limits against the current state of the telescope.
# optional argument "target" will check against predicted position...used when
# deciding whether or not to slew

    global HorizonList limitDecHigh limitDecLow limitRAHigh limitRALow limitFocLow limitFocHigh
    global simLimit

    if {$state != "current" && $state != "target"} {
	return "proc SimCheckLimits: UNKNOWN ARGUMENT '$limits'"
    }

    # check Dec limits:
    set decLimit 0
    if {$testDEC > $limitDecHigh || $testDEC < $limitDecLow} {
	set decLimit 1
    }

    # check RA limits:
    set raLimit 0
    if {$testHA > $limitRAHigh || $testHA < $limitRALow} {
	set raLimit 1
    }

    # check focus limits:
    set focHiLimit 0
    if {$testFOC >= $limitFocHigh} {
	set focHiLimit 1
    }
    set focLoLimit 0
    if {$testFOC <= $limitFocLow} {
	set focLoLimit 1
    }

    # check Horizon limits:
    # lifted from aqtelescope.tcl, proc PgmVerifyCoords
    
    set calcaz $testAZ
    set calcalt $testEL

    # figure out which list elements from the HorizonList straddle calcaz
    # note that this calculation assumes the HorizonList has a redundant entry
    # for 360 degrees that is identical to 0 degress. This avoids the need to
    # worry about the wrap-around calculations between 345 and 0

    set n [expr {int($calcaz / 15)}]
    set np1 [expr {$n + 1}]
    # prevent trouble when azimuth is exactly 0:
    if {$np1 > 24} {
	set np1 [ expr {$np1 - 24} ]
    }

    # get the altitude values from each of those elements
    set alt(n) [lindex [lindex $HorizonList $n] 1]
    set alt(np1) [lindex [lindex $HorizonList $np1] 1]

    # and get the azimuth value for the first element (second is just 15 + first)
    set az(n) [lindex [lindex $HorizonList $n] 0]

    # interpolate the altitude value
    set limit [expr {$alt(n) + ((($alt(np1) - $alt(n)) / 15.0) * ($calcaz - $az(n)))}]

    # if calculated altitude is less than slew limit, we have a problem.
    if {$limit > $calcalt} {
	set horLimit 1
    } else {
	set horLimit 0
    }

    # if we're checking a potential target for limits, just alert if we found one or more
    if {$state == "target"} {
	if {$raLimit || $decLimit || $horLimit || $focHiLimit || $focLoLimit} {
	    return "LIMIT"
	}
	return "OK"
    }

    # otherwise we're checking the current position and need to set the limit bits appropriately:

    set simLimit(RA)   $raLimit
    set simLimit(DEC)  $decLimit
    set simLimit(HOR)  $horLimit
    set simLimit(FOC+) $focHiLimit
    set simLimit(FOC-) $focLoLimit

    if {$raLimit || $decLimit || $horLimit || $focHiLimit || $focLoLimit} {
	return "LIMIT"
    }    
    return "OK"
}
################################################################################
proc SimQueryTCS { query { args "" } } {
# dummy proc that acts like a TCS query, with invented syntax just for testing

    global simPEC simDome simTrack
    set default "FAILED"

    switch $query {

	"ALL"         { set return [ SimReturnAll ] }
	"AZ"          { set return [ SimReturnAZ ] }
	"BEAM"        { set return [ SimReturnBeam ] }
	"CORRECTIONS" { set return [ SimReturnCorrections ] }
	"DATE"        { set return [ SimReturnDate ] }
	"DEC"         { set return [ SimReturnDEC ] }
	"DISABLE"     { set return [ SimReturnDisable ] }
	"DISEPOCH"    { set return [ SimReturnEpoch ] }
	"DOME"        { set return [ SimReturnDome $args ] }
	"EL"          { set return [ SimReturnEL ] }
	"EQ"          { set return [ SimReturnEQ ] }
	"FLEXFILE"    { set return [ SimReturnFlexFile ] }
	"FOCUS"       { set return [ SimReturnFocus ] }
	"FOCSPEED"    { set return [ SimReturnFocusSpeed ] }
	"HA"          { set return [ SimReturnHA ] }
	"JD"          { set return [ SimReturnJD ] }
	"LIMIT"       { set return [ SimReturnLimits ] }
	"LIMITINHIBIT" { set return [ SimReturnLimitInhibit ] }
	"LIMITPROF"   { set return [ SimReturnLimitProf ] }
	"MOTION"      { set return [ SimReturnMotion ] }
	"PADGUIDE"    { set return [ SimReturnRate "guide" ] }
	"PADDRIFT"    { set return [ SimReturnRate "drift" ] }
	"PAD"         { set return [ SimReturnPad ] }
	"PECSTAT"     { set return [ SimReturnPEC ] }
	"PECPROG"     { set return [] }
	"RA"          { set return [ SimReturnRA ] }
	"SECZ"        { set return [ SimReturnAirmass ] }
	"SIM"         { set return [ SimReturnSim ] }
	"ST"          { set return [ SimReturnLST ] }
	"TIME"        { set return [ SimReturnTime ] }
	"VERIFY"      { set return [ SimVerify $args ] }
	"XALL"        { set return [ SimReturnXall ] }
	"XDEC"        { set return [ SimReturnXdec ] }
	"XRA"         { set return [ SimReturnXra ] }
	"CON"         { set return [] }
	"INDEX"       { set reutrn [] }
	"PP"          { set return [] }
	"SAMDATA"     { set return [] }
	"SAMDONE"     { set return [] }
	"SERVO"       { set return [] }
	"SHOWP"       { set return [] }
	"SRVFRQ"      { set return [] }
	"TEST1"       { set return [] }
	"GETSATELAZ"  { set return [] }
	"GETSATECI"   { set return [] }
	"GETSATECEF"  { set return [] }

        default       { set return $default }
    }

    return $return
}
################################################################################
proc SimCommandTCS { command args } {

# most TCS commands are KEYWORD VALUE pairs, but some are single-word commands, and a few
# have 2 or more values.  by the time we get here, any timeout has been stripped off

# CommandTCS simulator does not vet values, assumes they are correct

    global verbose

    # get rid of the braces, otherwise it looks like a single-element list with multiple args
    set args [ string map { "{" "" "}" "" } $args ]

    if {$verbose} {
	puts "SIM: Received '$command' '$args'"
    }

    set argc [ llength $args ]

    # only deal with 2-arg commands...should make this more general for up to 5 args
    set value1 ""
    set value2 ""
    set value3 ""
    set keyword $command

    if {$argc >= 1} {
	set value1 [ lindex $args 0 ]
    }
    if {$argc >= 2} {
	set value2 [ lindex $args 1 ]
    }
    if {$argc >= 3} {
	set value3 [ lindex $args 2 ]
    }

    set default "FAILED"

    if {$verbose} {
	puts "SIM: Commanding '$command' '$value1' '$value2'"
    }

    # CLEARDIFF could be useful - should clear RA, Dec difference after arriving at object.
    # how to display?  Need to monitor in case of drift?

    switch $keyword {

	"BIASDEC"     { set return [ SimSetRate "biasdec" $value1 ] }
	"BIASRA"      { set return [ SimSetRate "biasra" $value1 ] }
	"CANCEL"      { set return [ SimStop "all" ] }
	"CLEARDIFF"   { set return [] }
	"DECLARE"     { set return [ SimDeclare $value1 ] }
	"DELAY9513"   { set return [] }
	"DISABLE"     { set return [ SimDisable 1 ] }
	"DISEPOCH"    { set return [ SimSetEpoch $value1 ] }
	"DOME"        { set return [ SimDomeParse $value1 $value2 $value3 ] }
	"DOMEGOTO"    { set return [ SimDomeMove $value1 ] }
	"ELAZ"        { set return [ SimSlewElAz $value1 $value2 $value3 ] }
	"ENABLE"      { set return [ SimDisable 0 ] }
	"FOCDN"       { set return [ SimFocusPaddle $keyword ] }
	"FOCSTOP"     { set return [ SimFocusPaddle $keyword ] }
	"FOCUP"       { set return [ SimFocusPaddle $keyword ] }
	"FOCUS"       { set return [ SimFocusMove $value1 0 ] }
	"FOCSPEED"    { set return [ SimSetFocusSpeed $value1 ] }
	"FOCZERO"     { set return [ SimZeroFocus ] }
	"LIMIT"       { set return [ SimLimitInhibit $value1 ] }
	"MOVNEXT"     { set return [ SimSlewNext ] }
	"MOVOFF"      { set return [] }
	"MOVRADEC"    { set return [ SimSlewRADec $value1 $value2 ] }
	"MOVREF"      { set return [] }
	"MOVSTOW"     { set return [ SimStow ] }
	"MOVWOB"      { set return [] }
	"NEXTPOS"     { set return [ SimSetNextPos $value1 $value2 ] }	
	"OFFSET"      { set return [] }
	"PAD"         { set return [] }
	"PADDLE"      { set return [ SimPaddle $value1 ] }
	"PADDRIFT"    { set return [ SimSetRate "drift" $value1 ] }
	"PADGUIDE"    { set return [ SimSetRate "guide" $value1 ] }
	"PARAM"       { set return [] }
	"PEC"         { set return [ SimPEC $value1 ] }
	"PECFILE"     { set return [] }
	"REFPOS"      { set return [] }
	"RELFOCUS"    { set return [ SimFocusMove $value1 1 ] }
	"STEPRA"      { set return [ SimStepRA $value1 ] }
	"STEPDEC"     { set return [ SimStepDEC $value1 ] }
	"SYSKILL"     { set return [ SimSysKill ] }
	"SYSRESET"    { set return [ SimSysReset $args ] }
	"SYSSAVE"     { set return [ SimSysSave ] }
	"TRACK"       { set return [ SimTracking $value1 ] }
	"WOBBLE"      { set return [] }
	"ABERRATE"    { set return [ SimSetCorrections ABERRATE $value1 ] }
	"BIAS"        { set return [ SimSetCorrections BIAS $value1 ] }
	"FLEX"        { set return [ SimSetCorrections FLEX $value1 ] }
	"NUTAT"       { set return [ SimSetCorrections NUTAT $value1 ] }
	"PARALLAX"    { set return [ SimSetCorrections PARALLAX $value1 ] }
	"PRECES"      { set return [ SimSetCorrections PRECES $value1 ] }
	"PROPMO"      { set return [ SimSetCorrections PROPMO $value1 ] }
	"REFRAC"      { set return [ SimSetCorrections REFRAC $value1 ] }
	"JUPITER"     { set return [ SimSlewSolarSystem JUPITER ] }
	"MARS"        { set return [ SimSlewSolarSystem MARS ] }
	"MERCURY"     { set return [ SimSlewSolarSystem MERCURY ] }
	"MOON"        { set return [ SimSlewSolarSystem MOON ] }
	"NEPTUNE"     { set return [ SimSlewSolarSystem NEPTUNE ] }
	"PLUTO"       { set return [ SimSlewSolarSystem PLUTO ] }
	"SATURN"      { set return [ SimSlewSolarSystem SATURN ] }
	"SUN"         { set return [ SimSlewSolarSystem SUN ] }
	"URANUS"      { set return [ SimSlewSolarSystem URANUS ] }
	"VENUS"       { set return [ SimSlewSolarSystem VENUS ] }
	"ABELL"       { set return [ SimSlewCatalog ABELL $value1 ] }
	"FK5"         { set return [ SimSlewCatalog FK5 $value1 ] }
	"IC"          { set return [ SimSlewCatalog IC $value1 ] }
	"NGC"         { set return [ SimSlewCatalog NGC $value1 ] }
	"OKESTONE"    { set return [ SimSlewCatalog OKESTONE $value1 ] }
	"PPM"         { set return [ SimSlewCatalog PPM $value1 ] }
	"SAO"         { set return [ SimSlewCatalog SAO $value1 ] }
	"YBSC"        { set return [ SimSlewCatalog YBSC $value1 ] }
	"ZWICKY"      { set return [ SimSlewCatalog ZWICKY $value1 ] }
	"TLE"         { set return [] }
	"SATTRACK"    { set return [] }
	"DMAX"        { set return [] }
	"DUMPSAM"     { set return [] }
	"GD"          { set return [] }
	"GP"          { set return [] }
	"GPI"         { set return [] }
	"PERMAX"      { set return [] }
	"SAMABORT"    { set return [] }
	"SAMPLE"      { set return [] }
	"SAMSTART"    { set rerutn [] }
	"SERVO"       { set return [] }
	"VMAX"        { set return [] }
	"WCON"        { set return [] }

	default       { set return $default }
    }

    return $return
}
################################################################################
proc SimInitializeTCS {} {

    global simPEC simDisabled simCurrentAirmass gobslat gobslong LOCATION pi radian
    global HorizonList limitDecHigh limitDecLow limitRAHigh limitRALow limitFocLow limitFocHigh
    global simFocus simEpoch simDome simRA simDEC simTime simAZ simEL simTrack simLimit simCorr
    global simFocSpeed simJD simPaddle simRate fast

    set pi [ expr {2*asin(1.0)} ]
    set radian 57.2957795131

    # focus units: steps per second
    set simFocus(rate)  100
    set simFocSpeed     "FAST"
    # DEC and dome units: degrees per second
    set simDome(rate)   2.0
    set simDEC(rate)    2.0
    # RA units: degrees per second:
    set degsec          2.0
    set simRA(rate)     [ expr {$degsec / 15.0} ]
    # tracking rate: 360 degrees per sidereal day (in seconds)
    set simTrack(rate)  [ expr {360 / (86164.092 * 15)} ]
    # guide and drift rates, arcsec/sec:
    set simRate(guide)     5.000
    set simRate(drift)   200.000
    set simRate(biasra)    0
    set simRate(biasdec)   0

    # fast simulation - speed up by a factor of 5:
    if {$fast} {
	set f 5
# can't change the simulated track rate without also speeding up simulated time - maybe later
#	set simTrack(rate) [ expr {$simTrack(rate) * $f} ]
	set simFocus(rate) [ expr {$simFocus(rate) * $f} ]
	set simDome(rate) [ expr {$simDome(rate) * $f} ]
	set simDEC(rate) [ expr {$simDEC(rate) * $f} ]
	set simRA(rate) [ expr {$simRA(rate) * $f} ]
    }

    # initial state
    set simTrack(tracking) 0
    set simPEC(condition)  0
    set simPEC(train)      0
    set simPEC(index)      0
    set simPEC(count)      0
    set simDome(auto)      0
    set simDome(moving)    0
    set simDome(init)      0
    
#    Parameters: [CPD] [SD] [W] [SDW] [NU] [RHO] [PHI] [LOOK] [HOLD]
#    CPD(Counts Per Degree) = XXX.xxxxxxx
#    SD(Stow Degrees)= XXX.xxxxxxx
#    W(Dome Width) = XXX.xxxxxxx
#    SDW(Stow Dome Width)= XXX.xxxxxxx
#    NU = XXX.xxxxxxx
#    RHO = XXX.xxxxxxx
#    PHI = XXX.xxxxxxx
#    LOOK(Lookahead) = XX
#    HOLD(Hold Dome) = XX

    set simDome(cpd)       0
    set simDome(sd) [ SimReturnStow "dome" ]
    set simDome(w)         5.0
    set simDome(sdw)       3.5
    set simDome(nu)        0
    set simDome(rho)       0
    set simDome(phi)       0
    set simDome(look)      1
    set simDome(hold)      0

    set simDisabled        1
    set simLimit(FOC+)     0
    set simLimit(FOC-)     0
    set simLimit(HOR)      0
    set simLimit(DEC)      0
    set simLimit(RA)       0
    set simLimit(inhibit)  0

    set simCorr(propmo)    1
    set simCorr(preces)    1
    set simCorr(nutat)     1
    set simCorr(aberrate)  1
    set simCorr(refrac)    1
    set simCorr(flex)      1
    set simCorr(parallax)  1
    set simCorr(object)    0
    set simCorr(bias)      0

    set simDome(autotol)   5

    # import horizon limits:
    set limitfile ObsHorizon.tcl
    if {![ file isfile $limitfile ]} {
	puts "SIM: Limit file '$limitfile' does not exist."
	exit
    }
    source $limitfile

    # latitude and longitude of observatory in decimal degrees
    # Dec limits (no RA limits for now?)
    if {$LOCATION == "703"} {
	set gobslat "32.3427"
	set gobslong "111.0000"
	set limitDecHigh 75.0
	set limitDecLow -40.0
	set limitRAHigh 6.0
	set limitRALow -6.0
	# (phi) used by TCS:
	set simDome(phi) -0.15
	# calculated (phi): - ~1.25" offset
#	set simDome(phi) -0.14074
    } elseif {$LOCATION == "V06"} {
	set gobslat "32.3427"
	set gobslong "111.0000"
	set limitDecHigh 62.0
	set limitDecLow -40.0
	set limitRAHigh 6.0
	set limitRALow -6.0
    } elseif {$LOCATION == "G96"} {
	set gobslat "32.44275"
	set gobslong "110.7887"
	set limitDecHigh 60.0
	set limitDecLow -40.0
	set limitRAHigh 6.0
	set limitRALow -6.0
	# (phi) used by TCS:
	set simDome(phi) -0.2222
	# calculated (phi): - ~10" offset
#	set simDome(phi) -0.27143
    } elseif {$LOCATION == "E12"} {
	set gobslat "-31.2666"
	set gobslong "-149.0833"
	set limitDecHigh 35.0
	set limitDecLow -80.0
	set limitRAHigh 6.0
	set limitRALow -6.0
	set simDome(rho) -0.25757
    } elseif {$LOCATION == "I52"} {
	set gobslat "32.44275"
	set gobslong "110.7887"
	set limitDecHigh 80.0
	set limitDecLow -35.0
	set limitRAHigh 6.0
	set limitRALow -6.0
	# (rho) used by TCS:
	set simDome(rho) -0.25757
	# calculated (rho): - ~1" offset
#	set simDome(rho) -0.26667
    } elseif {$LOCATION == "21-IN"} {
	set gobslat "32.44275"
	set gobslong "110.7887"
	set limitDecHigh 80.0
	set limitDecLow -40.0
	set limitRAHigh 6.0
	set limitRALow -6.0
    }

    set limitFocLow -20000
    set limitFocHigh 20000

    # local, UT and LST times:

    # need to get the local time for the site, not from where the program is running.
    # make an estimate based on longitude.  timeoffset2 is the decimal hours from GMT, while
    # timeoffset is the integer hours (proxy for time zone).

    # seconds since 1970:
    set simTime(zero) [ GetTimeSeconds ]
    set simTime(currentUT) [ SimReturnTime ]
    set timeoffset2 [ expr {(-1 * $gobslong / 15.0)} ]
    set simTime(offset2) $timeoffset2

    set simJD [ GetJD ]
    set simTime(initialGMST) [ SiderealTime $simJD ]
    set simTime(currentGMST) $simTime(initialGMST)
    set simTime(currentLST) [ expr {$simTime(initialGMST) + $timeoffset2} ]
    set simTime(sexLST) [ dec2sex $simTime(currentLST) -1 ]

    set simAZ(stow) [ SimReturnStow "az" ]
    set simEL(stow) [ SimReturnStow "el" ]

    # az, el dome and focus initial state:
    set simAZ(current) $simAZ(stow)
    set simEL(current) $simEL(stow)

    set simAZ(target) $simAZ(current)
    set simEL(target) $simEL(current)

    set simEpoch 2000.0

    set simDome(current) $simDome(sd)
    set simDome(target) $simDome(current)
    set simDome(original) $simDome(current)
    set simDome(distance) 0
    set simFocus(current) [ SimReturnStow "focus" ]
    set simFocus(target) $simFocus(current)
    set simFocus(original) $simFocus(current)
    set simFocus(distance) 0

    # initial paddle state (0 = paddle ignored):
    set simPaddle 0

    # get the RA and DEC that correspond to the current AZ, EL:
    set radec [ e2h $simEL(current) $simAZ(current) $simTime(currentGMST) $gobslat $gobslong ]

    set simRA(current) [ lindex $radec 0 ]
    set simDEC(current) [ lindex $radec 1 ]

    set simRA(next) $simRA(current)
    set simDEC(next) $simDEC(current)

    set simRA(sex) [ dec2sex $simRA(current) 2 ]
    set simDEC(sex) [ dec2sex $simDEC(current) 1 ]
    if {$simDEC(current) >= 0} {
	set simDEC(sex) "+$simDEC(sex)"
    }

    set simRA(target) ""
    set simDEC(target) ""

    set simCurrentAirmass [ SimReturnAirmass ]

    set simTime(currentLHA) [ SimLHA $simTime(currentLST) $simRA(current) ]
    set sexLHA [ dec2sex $simTime(currentLHA) -1 ]
    if {$simTime(currentLHA) >= 0} {
	set sexLHA "+$sexLHA"
    }
    set simTime(sexLHA) $sexLHA
 
    # indicate that there are no moves in progress:
    set simRA(slewing) 0
    set simRA(moving) 0
    set simDEC(moving) 0
    set simFocus(moving) 0
    set simDome(moving) 0

    # probably not tracking, so need to initialize a few things:
    if {!$simTrack(tracking)} {
	SimTracking OFF
    }
}
################################################################################
proc TCSServerSim {channel clientaddr clientport} {
# by A.R.Gibbs

    global verbose

    if {$verbose} {
	puts "Connection from $clientaddr:$clientport"
    }

    gets $channel sIn
    set sOut "UNKNOWN_CMD"
    
    set lIn [split $sIn " "]
    if {[llength $lIn] >= 5} {
	
	set sType [lindex $sIn 3]
	set sCommand [lindex $sIn 4]
	set sArgs [lindex $sIn 5]
	set sArgs [lrange $sIn 5 end]
	
	# probably also have a timeout value at lindex 6?

	if {$verbose} {
	    puts "*********** $sIn *************"
	}

	if {$sType == "REQUEST"} {
	    
	    set sOut [ SimQueryTCS $sCommand $sArgs ]
	    
	} elseif {$sType == "COMMAND"} {
	    
	    set sOut [ SimCommandTCS $sCommand $sArgs ]
	}
    }

    puts $channel "TCSNG TCS 1 $sOut"

    close $channel
}
################################################################################
# main:

#source /home/observer/lib/v1/tcl/astrolib.tcl
source ./astrolib.tcl
global simPEC simFocus simRA simDEC simDome simTime simJD

global simDisabled simLimit simTrack
global simAZ simEL simCurrentAirmass
global pi radian gobslat gobslong LOCATION verbose fast HorizonList
global limitDecHigh limitDecLow limitRAHigh limitRALow limitFocHigh limitFocLow

global simPaddle simRate 
global stowFile focusFile

# see if location was provided; if not, find location:
#SimSetLocation $argv $argc

if {$argc < 1} {

    ShowUsage
    exit
}

set LOCATION [ lindex $argv 0 ]

# make sure obtained location is a good one
set goodlocations [list 703 G96 I52 E12 V06 21-IN]
    
if {[lsearch $goodlocations $LOCATION] == -1} {
    
    puts "\nBad location '$LOCATION' specified. Exiting.\n"
    exit
}

set verbose 0
set fast 0

# have two possible optional arguments - verbose and fast
if {$argc > 1} {

    set arg2 [ lindex $argv 1 ]
    if {$arg2 == "-ver"} {
	set verbose 1
    } elseif {$arg2 == "-fast"} {
	set fast 1
    } else {
	puts "\nSIM: Unknown arg '$arg2'"
	ShowUsage
	exit
    }

    set arg3 [ lindex $argv 2 ]

    if {$arg3 == ""} {
	# do nothing, don't exit
    } elseif {$arg3 == "-ver"} {
	set verbose 1
    } elseif {$arg3 == "-fast"} {
	set fast 1
    } else {
	puts "\nSIM: Unknown arg '$arg2'"
	ShowUsage
	exit
    }    
}

#set stowFile "./simstow.txt"
set stowFile "./simstow.$LOCATION"
set focusFile "./simfocus.txt"

if {![ file isfile $stowFile ]} {
    puts "  Stow defintion file '$stowFile' does not exist."
    exit
}
if {![ file isfile $focusFile ]} {
    puts "  Focus defintion file '$focusFile' does not exist."
    exit
}

# set initial conditions:
SimInitializeTCS
SimReturnLimits

# enter the forever loop to wait for commands or requests:
set result [ socket -server TCSServerSim 5750 ]
vwait forever

# how to cancel a move?  sim is currently assuming CANCEL stops the RA, Dec, Dome and focus - maybe not correct

# shouldn't track forever...should calculate how long it will take to track into the limit, and then track for just
# that long - that way if left unattended, simulated telescope will end up in a limit, just like a real telescope,
# and not pointed at the ground
# or...if negative elevation, set it back up to the limit

# autodome needs work sorting out lookahead / non-lookahead - not so urgent since GUI will always enforce LOOKAHEAD

