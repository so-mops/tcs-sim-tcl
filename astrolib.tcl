set astro_exe "/home/observer/bin/astro"
set M_PI 3.14159265359

#############################################################################
# Gets the date string for the current time, switches over at local noon
# Uses shell environment variable SITECODE if mpc_code is not passed
# If neither is defined then 703 is used
proc datestr { {mpc_code ""} } {

  global astro_exe

  # string to be returned on error
  set retstr "00Jan01"

  set code_switch ""
  if {$mpc_code != ""} {
    set code_switch " -c $mpc_code"
  }

  set cmdline "$astro_exe $code_switch obsdate now"
  if { [catch {eval exec $cmdline} retstr] } {
    puts "Failed to execute '$cmdline' defaulting to date $retstr"
    return $retstr
  }

  return $retstr
}

##########################################################################################
# Returns the distance between two ra,dec points
# Note ra is in hours dec is in degrees
proc RaDecDist { ra1 dec1 ra2 dec2 } {

    set radianDec1 [ expr {$dec1 * 3.1415926 / 180.0} ]
    set radianDec2 [ expr {$dec2 * 3.1415926 / 180.0} ]

    set avgRadianDec [ expr {(($radianDec1 - $radianDec2) / 2) + $radianDec2} ]

    # Ra is in hours, Dec is in degrees
    set dRa [ expr {fmod((24.0 + $ra1 - $ra2), 24.0)} ]
    if {$dRa > 12.0} {set dRa [expr {$dRa - 24.0}]}
    set dRA [ expr {$dRa * 15.0 *  cos($avgRadianDec)} ]
    set dDec [ expr {($dec1 - $dec2)} ]

    set delta [ expr {sqrt($dRA * $dRA + $dDec * $dDec )} ]

    return $delta
}

#############################################################################
# Gets the date string for the current time, switches over at local noon
# Uses shell environment variable SITECODE if mpc_code is not passed
# If neither is defined then 703 is used.  Date return format is all digits
# yyyymmdd
proc yyyymmddstr { {mpc_code ""} } {

  global astro_exe
  # string to be returned on error
  set retstr "19000101"

  set code_switch ""
  if {$mpc_code != ""} {
    set code_switch " -c $mpc_code"
  }

  set cmdline "$astro_exe $code_switch obsyyyymmdd now"
  if { [catch {eval exec $cmdline} retstr] } {
    puts "Failed to execute '$cmdline' defaulting to date $retstr"
    return $retstr
  }

  return $retstr
}

#############################################################################
# Take a julian day and convert it into a modified julian day
proc jd2mjd {jd} {
  return [expr {$jd - 2400000.5}]
}

#############################################################################
# Take a modified julian day and convert it into a julian day
proc mjd2jd {mjd} {
  return [expr {$mjd + 2400000.5}]
}

#############################################################################
# Take a julian day timetag and turn it into a year month dayofmoth.fraction
proc jd2cd {jd y m d} {

    upvar $y year
    upvar $m month
    upvar $d day

    set z [expr {int($jd + 0.5)}]
    set f [expr {$jd + 0.5 - $z}]

    if {$z < 2299161} {
        set a $z
    } else {
        set alpha [expr {int(($z - 1867216.25)/36524.25)}]
        set a [expr {$z + 1 + $alpha - int($alpha/4)}]
    }

    set b [expr {$a + 1524}]
    set c [expr {int(($b - 122.1)/365.25)}]
    set d [expr {int(365.25 * $c)}]
    set e [expr {int(($b - $d)/30.6001)}]

    set day [expr {$b - $d - int(30.601 * $e) + $f}]

    if {$e < 14 } {
        set month [expr {$e -1}]
    } else {
        set month [expr {$e - 13}]
    }

    if {$month > 2} {
        set year [expr {$c - 4716}]
    } else {
        set year [expr {$c - 4715}]
    }

    return
}

#############################################################################
# Take a julian day timetag and turn it into a year month dayofmoth.fraction
proc mjd2cdt {mjd y m d t} {

  upvar $y year
  upvar $m month
  upvar $d day
  upvar $t timeofday

  set int_part [expr {floor($mjd)}]
  set timeofday [expr {$mjd - $int_part}]
  set jd [mjd2jd [expr {$int_part + 0.01}] ]
  jd2cd $jd year month day
  set day [expr { floor($day) + $timeofday}]

  return
}

########################################################################################
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

#############################################################################
# Convert a year, day of year.fraction into Modified Julian day
proc mjlnday { year doy } {
  set mjd 0

  set a 0
  set b 0
  set CAL_REFORM_YR  1582
  set CAL_REFORM_DAY 288
  set JD0           -678578.0
  set JYEAR_LEN      365.25

  if {($year > $CAL_REFORM_YR) || (($year == $CAL_REFORM_YR) && ($doy >= $CAL_REFORM_DAY))} {
    set a [expr { int(($year - 1) / 100.0) }]
    set b [expr { 2.0 - $a + int($a / 4.0)}]
  }

  set mjd [expr {$JD0 + $b + int($JYEAR_LEN * ($year-1)) + $doy}]

  return $mjd
}

#####################################################
# Convert an DEG:MM:SS.sssss string into whatever unit
# DEG is in, could be hours or degrees
proc sex2dec { sexvalue } {

    set place1 0
    set place2 0
    set place3 0

    set argCnt [scan $sexvalue {%3d:%2d:%6f} place1 place2 place3]

    set decval [expr {abs($place1) + $place2 / 60.0 + $place3 / 3600.0}]

    if {[string index $sexvalue 0] == "-"} {
        set decval [ expr {$decval * -1.0} ]
    }

    return $decval
}

#####################################################
# Convert an DEG:MM:SS.sssss string into whatever unit
# DEG is in, could be hours or degrees
proc sex2decSpace { sexvalue } {

    set place1 0
    set place2 0
    set place3 0

    set argCnt [scan $sexvalue {%d %d %f} place1 place2 place3]

    set decval [expr {abs($place1) + $place2 / 60.0 + $place3 / 3600.0}]

    if {[string index $sexvalue 0] == "-"} {
        set decval [ expr {$decval * -1.0} ]
    }

    return $decval
}

#############################################################################
# convert a civil year month day.fraction into a modified julian day (MJD)
proc civil2mjd { year month day } {

  set day2month [list 0 31 28 31 30 31 30 31 31 30 31 30 31]
  set mjd 0
  set doy 0

  for {set j 1} {$j < $month} {incr j} {
    set doy [expr {$doy + [lindex $day2month $j]}]
  }
  if { ([expr {$year % 1000}] == 0) || ([expr {$year % 100}] != 0) } {
    if {$month > 2 && [expr {$year % 4}] == 0} {
      set doy [expr {$doy + 1}]
    }
  }
  set doy [expr {$doy + $day}]
  set mjd [mjlnday $year $doy]

  return $mjd
}

#############################################################################
# Convert DATE-OBS TIME-OBS strings from FITS headers into mjd time
# returns an error string if something is wrong with format
# otherwise real number mjd time is returned
proc OBSdatetime2mjd { date_obs time_obs } {
  set mjd 0

  if {[string length $date_obs] != 10} {
    return "DATE-OBS '$date_obs' string must be 10 characters long, found [string length $date_obs]"
  }
  if {[string index $date_obs 4] != "-" } {
    return "DATE-OBS '$date_obs' fifth character must be a -"
  }
  if {[string index $date_obs 7] != "-" } {
    return "DATE-OBS '$date_obs' eighth character must be a -"
  }
  set argcnt [scan $date_obs "%u-%u-%u" year month dom]
  if {$argcnt != 3} {
    return "DATE-OBS '$date_obs' failed to parse yyyy-mm-dd"
  }

  if {![string is integer $year]} {
    return "DATE-OBS '$date_obs' year($year) is not an integer"
  }
  if {![string is integer $month]} {
    return "DATE-OBS '$date_obs' month($month) is not an integer"
  }
  if {![string is integer $dom]} {
    return "DATE-OBS '%date_obs' day of month($dom) is not an integer"
  }

  if {[string length $time_obs] < 8} {
    return "TIME-OBS '$time_obs' string must be at least 8 characters long"
  }
  if {[string index $time_obs 2] != ":" } {
    return "TIME-OBS '$time_obs' third character must be a :"
  }
  if {[string index $time_obs 5] != ":" } {
    return "TIME-OBS '$time_obs' sixth character must be a :"
  }
  set argcnt [scan $time_obs {%2d:%2d:%f} hour minute second]
  if {$argcnt != 3} {
    return "TIME-OBS '$time_obs' failed to parse hh:mm:seconds"
  }

  set dom [expr {$dom + $hour/24.0 + $minute/24.0/60.0 + $second/24.0/3600.0}]
  set mjd [civil2mjd $year $month $dom]
  return $mjd
}

#############################################################################
# Convert a degrees or hours value into segidecimal format ie HH:MM:SS.ssss
# decimal places says how manny digits to the right of the decimal for seconds
# Values from 1 to 4 for decimal places are expected
# The routine rounds the result to the closest decimal
proc dec2sex { decvalue { decimalplaces 1 } } {

  set isneg 0

  # Round result to the desired decimalplaces before conversion
  set tounits [expr {60 * 60 * pow(10, max(0,$decimalplaces))}]
  set units [expr {round($decvalue * $tounits)} ]

  if {$units < 0.0 } {
    set isneg 1
    set units [expr {-1 * $units}]
  }

  set place1 [expr {int($units / $tounits)}]
  set units  [expr {$units - ($place1*$tounits)}]
  set place2 [expr {int($units * 60 / $tounits)}]
  set units  [expr {$units - ($place2 * $tounits / 60)}]
  set place3 [expr {$units /  pow(10, max(0,$decimalplaces))}]

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

#############################################################################
# Expects all arguments to be in degrees
# returns PA in degrees
proc calcPA { ra1 dec1 ra2 dec2 } {
  set M_PI 3.14159265359

  set pa 0.0

  if {$ra1 != $ra2 || $dec1 != $dec2} {
    set deltaRa [expr {$ra2 - $ra1}]
    set deltaDec [expr {$dec2 - $dec1}]
    set cosMidDec [expr {cos(($dec1 + $dec2)*$M_PI/180.0/2.0)}]
#   set pa [expr {atan2($deltaDec*$M_PI/180.0, $deltaRa*$cosMidDec*$M_PI/180.0)*180/$M_PI}]
    set pa [expr {atan2($deltaRa*$cosMidDec*$M_PI/180.0, $deltaDec*$M_PI/180.0) * 180/$M_PI}]

    if {$pa < 0} {
      set pa [expr {$pa + 360.0}]
    }
  }

  return $pa
}

###############################################################################
proc AsteroidType { input } {

# for a translation of these decimal integers to asteroid type, see
# http://cfa-www.harvard.edu/iau/info/MPOrbitFormat.html, columns 162 - 165.

    set returnstring ""

    set typeint "32768 16384 8192 4096 2048 1024 512 256 128 64 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1"

    set typestring(32768) "PHA"
    set typestring(16384) "CriticalNumberedObj"
    set typestring(8192)  "Prev1-opp"
    set typestring(4096)  "1+km"
    set typestring(2048)  "NEO"
    set typestring(1024)  ""
    set typestring(512)   ""
    set typestring(256)   ""
    set typestring(128)   ""
    set typestring(64)    ""
    set typestring(17)    "ScatteredDisk"
    set typestring(16)    "Cubewano"
    set typestring(15)    "ResonantTNO"
    set typestring(14)    "Plutino"
    set typestring(13)    ""
    set typestring(12)    ""
    set typestring(11)    ""
    set typestring(10)    "Centaur"
    set typestring(9)     "JupiterTrojan"
    set typestring(8)     "Hilda"
    set typestring(7)     "Phocaea"
    set typestring(6)     "Hungaria"
    set typestring(5)     "q<1.665 AU"
    set typestring(4)     "Amor"
    set typestring(3)     "Apollo"
    set typestring(2)     "Aten"
    set typestring(1)     "Atira"

    foreach type $typeint {

        if {[ expr {$input - $type} ] >= 0} {

            if {$typestring($type) != ""} {
                append returnstring $typestring($type)
            }

            set input [ expr {$input - $type} ]

            if {$input == 0} {
               break
            }
        }
    }

    return $returnstring
}

###############################################################################
# Input mpc code
# output parameters site east longitude north latitude altitude in radians and km
proc getSiteCoords {mpc_code p_n_latitude p_e_longitude p_altitude} {
  upvar $p_n_latitude ret_n_latitude 
  upvar $p_e_longitude ret_e_longitude
  upvar $p_altitude ret_altitude

  global astro_exe
  set PI 3.14159265359

  set code_switch ""
  if {$mpc_code != ""} {
    set code_switch " -c $mpc_code"
  }

  set success 0
  set cmdline "$astro_exe $code_switch site"
  if { [catch {eval exec $cmdline} retstr] } {
    puts "Failed to execute '$cmdline' defaulting to site 703"
  } else {
    set resultLines [split $retstr "\n"]
    if {[llength $resultLines] < 6} {
      puts "Failed to return 6 lines from '$cmdline' defaulting to site 703"
    } else {
      set e_longitude [string range [lindex $resultLines 1] 15 end]
      set n_latitude [string range [lindex $resultLines 2] 15 end]
      set altitude [string range [lindex $resultLines 3] 9 end-3]
      if {[string is double $e_longitude] && [string is double $n_latitude] && [string is double $altitude]} {
        set success 1
      } else {
        puts "Failed to parse N latitude and E longitude lines from '$cmdline' defaulting to site 703"
      }
    }
  }

  if {!$success} {
    set n_latitude 32.41707
    set e_longitude 249.26736
    set altitude 2.4936
  }
  set ret_n_latitude [expr {$PI * $n_latitude / 180.0}]
  set ret_e_longitude [expr {$PI * $e_longitude / 180.0}]
  set ret_altitude $altitude
  return
}

###############################################################################
# Input ha and dec in radians, local sidereal time radians, MPC code
# return parameters az,el in radians
proc hadec2azel { ha dec mpc_code p_az p_el } {
  upvar $p_az az
  upvar $p_el el
  
  set PI 3.14159265359

  getSiteCoords $mpc_code n_latitude e_longitude altitude
  set sindec [expr {sin($dec)}]
  set cosdec [expr {cos($dec)}]
  set sinha  [expr {sin($ha)}]
  set cosha  [expr {cos($ha)}]
  set sinlat [expr {sin($n_latitude)}]
  set coslat [expr {cos($n_latitude)}]

  # From Astronomical Almanac
  set x [expr {$sindec * $coslat - $cosdec * $cosha * $sinlat}]
  set y [expr {-1*$cosdec * $sinha}]
  set sinel [expr {$sindec * $sinlat + $cosdec * $cosha * $coslat}]
  set azimuth [expr {atan2($y, $x)}]
  if {$azimuth < 0} {
    set azimuth [expr {$azimuth + 2.0*$PI}]
  }
  if {$sinel > 1.0} {
    set sinel 1.0
  }
  if {$sinel < -1.0} {
    set sinel -1.0
  }
  set elevation [expr {asin($sinel)}]
  
  set az $azimuth
  set el $elevation
  return
}

###############################################################################
# Input ra and dec in radians, local sidereal time radians, MPC code
# return parameters az,el in radians
proc radec2azel { ra dec sidereal mpc_code p_az p_el } {
  upvar $p_az az
  upvar $p_el el

  set PI 3.14159265359

  set ha [expr {$sidereal - $ra}]
  hadec2azel $ha $dec $mpc_code azimuth elevation

  set az $azimuth
  set el $elevation
  return
}

###############################################################################
# Given an mjd time, find the closest twilight times in MJD
proc getTwilight { mjd obscode p_evening_astro p_morning_astro p_evening_naut p_morning_naut p_sunset p_sunrise} {

  upvar $p_evening_astro evening_astro
  upvar $p_morning_astro morning_astro
  upvar $p_evening_naut evening_naut
  upvar $p_morning_naut morning_naut
  upvar $p_sunset sunset
  upvar $p_sunrise sunrise

  global astro_exe

  set cmdline "$astro_exe -c $obscode twilight $mjd"
  if { [catch {eval exec $cmdline} retstr] } {
    puts "Failed to execute '$cmdline' defaulting to date $retstr"
    return $retstr
  }

  set resultLines [split $retstr "\n"]
  if {[llength $resultLines] < 3} {
    puts "Failed to return 3 lines from '$cmdline' twilight times not set"
  } else {
    set day_evening_astro [sex2dec [string range [lindex $resultLines 0] 27 35]]
    set day_morning_astro [sex2dec [string range [lindex $resultLines 0] 39 47]]
    set day_evening_naut [sex2dec [string range [lindex $resultLines 1] 23 31]]
    set day_morning_naut [sex2dec [string range [lindex $resultLines 1] 35 43]]
    set day_sunset [sex2dec [string range [lindex $resultLines 2] 20 28]]
    set day_sunrise [sex2dec [string range [lindex $resultLines 2] 32 40]]
    if {[string is double $day_evening_astro] && [string is double $day_morning_astro] && [string is double $day_evening_naut]  && [string is double $day_morning_naut]  && [string is double $day_sunset]  && [string is double $day_sunrise] } {
      set success 1
      set day_evening_astro [expr {$day_evening_astro/24.0 + round($mjd)}]
      set day_morning_astro [expr {$day_morning_astro/24.0 + round($mjd)}]
      set day_evening_naut [expr {$day_evening_naut/24.0 + round($mjd)}]
      set day_morning_naut [expr {$day_morning_naut/24.0 + round($mjd)}]
      set day_sunset [expr {$day_sunset/24.0 + round($mjd)}]
      set day_sunrise [expr {$day_sunrise/24.0 + round($mjd)}]
      if {[expr {$day_evening_astro - $mjd > 0.5}]} {
        set day_evening_astro [expr {$day_evening_astro - 1.0}]
      }
      if {[expr {$day_morning_astro - $mjd > 0.5}]} {
        set day_morning_astro [expr {$day_morning_astro - 1.0}]
      }
      if {[expr {$day_evening_naut - $mjd > 0.5}]} {
        set day_evening_naut [expr {$day_evening_naut - 1.0}]
      }
      if {[expr {$day_morning_naut - $mjd > 0.5}]} {
        set day_morning_naut [expr {$day_morning_naut - 1.0}]
      }
      if {[expr {$day_sunset - $mjd > 0.5}]} {
        set day_sunset [expr {$day_sunset - 1.0}]
      }
      if {[expr {$day_sunrise - $mjd > 0.5}]} {
        set day_sunrise [expr {$day_sunrise - 1.0}]
      }
      set evening_astro $day_evening_astro
      set morning_astro $day_morning_astro
      set evening_naut $day_evening_naut
      set morning_naut $day_morning_naut
      set sunset $day_sunset
      set sunrise $day_sunrise
    } else {
      puts "Failed to parse all twilight values, return parameters not set"
    }
  }
  

  return $retstr
}

###############################################################################
# Given an mjd time, find the closest twilight times in MJD
proc getMoon { mjd obscode p_ra p_dec p_az p_el p_illumination } {

  upvar $p_ra  ra
  upvar $p_dec dec
  upvar $p_az az
  upvar $p_el el
  upvar $p_illumination illumination

  global astro_exe
  set cmdline "$astro_exe -c $obscode moon $mjd"
  if { [catch {eval exec $cmdline} retstr] } {
    puts "Failed to execute '$cmdline' defaulting to date $retstr"
    return $retstr
  }

  set resultLines [split $retstr "\n"]
  if {[llength $resultLines] < 1} {
    puts "Failed to return 1 lines from '$cmdline' moon values not set"
  } else {
    set argcnt [scan  [lindex $resultLines 0] {Moon ra dec %f %f az el %f %f Illumination %f} scan_ra scan_dec scan_az scan_el scan_illumination]
    if {$argcnt != 5} {
      puts "Only scanned $argcnt values from '[lindex $resultLines 0]', moon values not set"
      return $resultLines
    }
    if {[string is double $scan_ra] && [string is double $scan_dec] && [string is double $scan_az]  && [string is double $scan_el]  && [string is double $scan_illumination] } {
      set success 1
      set ra $scan_ra
      set dec $scan_dec
      set az $scan_az
      set el $scan_el
      set illumination $scan_illumination
    } else {
      puts "Failed to parse all twilight values, return parameters not set"
    }
  }
  

  return $retstr
}

############################################################################################
# get the astronomical PA using ra,dec rates
# NOTE that this calculation does not correct rarate by the cos(declination) this
# should be done before passing deltaRA to this routine
proc getPosAngle { deltaRA deltaDEC } {

    # make sure we don't divide by zero:
    if {$deltaDEC == 0} {
        set deltaDEC 0.0000000001
    }

    set posAngle [ expr {atan($deltaRA / $deltaDEC)} ]
    set posAngle [ rad2deg $posAngle ]

    # correction if deltaDEC is negative:
    if {$deltaDEC < 0} {
        set posAngle [ expr {$posAngle + 180} ]
    }

    # make sure $posAngle is between 0 and 360
    if {$posAngle < 0} {
        set posAngle [ expr {360 + $posAngle} ]
    }

    return $posAngle
}

############################################################################################
# Input an ra (degrees) and dec (degrees) and sets p_glat and p_glong to
# galactic longitude and latitude in degrees
proc radec2glonglat { ra dec p_glong p_glat} {
  upvar $p_glong glong
  upvar $p_glat  glat

  set RPD 0.0174533
  set DPR 57.295828

  set work_eclipticlong [expr {$DPR * atan2(((sin($ra*$RPD)*cos(23.4419*$RPD))+(tan($dec*$RPD)*sin(23.4410*$RPD))),cos($ra*$RPD))}]
  if {$work_eclipticlong < 0} {set work_eclipticlong [expr {$work_eclipticlong + 360.0}]}

  set work_eclipticlat  [expr {$DPR * asin((sin($dec*$RPD)*cos(23.4419*$RPD))-(cos($dec*$RPD)*sin(23.4419*$RPD)*sin($ra*$RPD)))}]

  set ra [expr {$ra - 192.25}]
  set work_galacticlat  [expr {$DPR * asin((cos($dec*$RPD)*cos(27.4*$RPD)*cos($ra*$RPD))+(sin(27.4*$RPD)*sin($dec*$RPD)))}]
  set work_galacticlong [expr {33.0 + $DPR * atan2((sin($dec*$RPD)-(sin($work_galacticlat*$RPD)*sin(27.4*$RPD))),(cos($dec*$RPD)*sin($ra*$RPD)*cos(27.4*$RPD)))}]
  if {$work_galacticlong < 0} {set work_eclipticlong [expr {$work_eclipticlong + 360.0}]}

  set glong $work_galacticlong
  set glat $work_galacticlat
}
############################################################################################
# Input an ra (degrees) and dec (degrees) and sets p_glat and p_glong to
# galactic longitude and latitude in degrees
proc radec2elonglat { ra dec p_elong p_elat} {
  upvar $p_elong elong
  upvar $p_elat  elat

  set RPD 0.0174533
  set DPR 57.295828

  set work_eclipticlong [expr {$DPR * atan2(((sin($ra*$RPD)*cos(23.4419*$RPD))+(tan($dec*$RPD)*sin(23.4410*$RPD))),cos($ra*$RPD))}]
  if {$work_eclipticlong < 0} {set work_eclipticlong [expr {$work_eclipticlong + 360.0}]}

  set work_eclipticlat  [expr {$DPR * asin((sin($dec*$RPD)*cos(23.4419*$RPD))-(cos($dec*$RPD)*sin(23.4419*$RPD)*sin($ra*$RPD)))}]

  set elong $work_eclipticlong
  set elat $work_eclipticlat
}

############################################################################################
# given a telescope pointing vector, determine the dome azimuth and elevation where the
# optical path intersects the dome sphere
# The elevation can then be used by the caller to modify the dome settle tolerances
proc DomeIntersect { tel_x tel_y tel_z dome_radius tel_az tel_el p_domeAz p_domeEl } {

  upvar $p_domeAz ret_domeAz
  upvar $p_domeEl ret_domeEl
  global M_PI

  set DEG2RAD [expr {$M_PI/180.0}]
  set RAD2DEG [expr {180.0/$M_PI}]
 
  # The sign of the x axis as defined by Stward is opposite from what this code is assuming
  # so we are going to reverse the sign
  set tel_x [expr {-$tel_x}] 

  #define the sphere/dome center
  set cx 0.0 
  set cy 0.0 
  set cz 0.0 

  #Create a second point along the optical path that is outside of the sphere
  set vx [expr {3.0 * sin($tel_az*$DEG2RAD) * cos($tel_el*$DEG2RAD) + $tel_x}]
  set vy [expr {3.0 * cos($tel_az*$DEG2RAD) * cos($tel_el*$DEG2RAD) + $tel_y}]
  set vz [expr {3.0 * sin($tel_el*$DEG2RAD) + $tel_z}]

  set px $tel_x
  set py $tel_y
  set pz $tel_z

  set a [expr {$vx*$vx + $vy*$vy + $vz*$vz}]
  set b [expr {2.0 * ($px * $vx + $py * $vy + $pz * $vz - $vx * $cx - $vy * $cy - $vz * $cz)}]
  set c [expr {$px * $px - 2.0 * $px * $cx + $cx * $cx + $py * $py - 2.0 * $py * $cy + $cy * $cy + $pz * $pz - 2.0 * $pz * $cz + $cz * $cz - $dome_radius * $dome_radius}]

  set d [expr {$b * $b - 4 * $a * $c}]
  if {$d < 0} {
    set ret_domeAz $tel_az
    set ret_domeEl $tel_el
    return "No intersection found, why not?"
  }


  if {$d == 0} {
    set t1 [expr {( -$b - sqrt ( $d ) ) / ( 2.0 * $a )}]

    set ix [expr {$tel_x + $t1 * ($vx-$px)}]
    set iy [expr {$tel_y + $t1 * ($vy-$py)}]
    set iz [expr {$tel_z + $t1 * ($vz-$pz)}]
  } else {
    set t2 [expr {( -$b + sqrt( $d ) ) / ( 2.0 * $a )}]

    set ix [expr {$tel_x + $t2 * ($vx-$px)}]
    set iy [expr {$tel_y + $t2 * ($vy-$py)}]
    set iz [expr {$tel_z + $t2 * ($vz-$pz)}]
  }

  set ret_domeEl [expr {$RAD2DEG * asin($iz/$dome_radius)}]
  set ret_domeAz [expr {$RAD2DEG * atan2($ix/$dome_radius, $iy/$dome_radius)}]
  if {$ret_domeAz < 0} {set ret_domeAz [expr {$ret_domeAz + 360.0}]}

  return ""
}

# exposure time calculator for determining missing SNR exposure or mangitude value
########################################################################################

global filterExtinctCoeff magZeroPoint

set filterExtinctCoeff(U) 0.60
set filterExtinctCoeff(B) 0.40
set filterExtinctCoeff(V) 0.20
set filterExtinctCoeff(R) 0.10
set filterExtinctCoeff(I) 0.08
set filterExtinctCoeff(N) 0.20
set filterExtinctCoeff(W) 0.15

set magZeroPoint(U)  5.50e+05
set magZeroPoint(B)  3.91e+05
set magZeroPoint(V)  8.66e+05
set magZeroPoint(R)  1.10e+06
set magZeroPoint(I)  6.75e+05
set magZeroPoint(N)  4.32e+06
set magZeroPoint(W)  2.00e+06

########################################################################################

proc Mpc2ConfigExpcalc { mpccode } {
  set configArgNames [list filter primary radius qe readnoise pixelsize skybrightness airmass fwhm obstdiameter]
  set configArgValues ""

  if {$mpccode == "703"} {
    set configArgValues [list N 72.0 6.0 0.90 8.0 3.0 20.0 1.5 3.0 25.0]
  } elseif {$mpccode == "G96"} {
    set configArgValues [list N 152.0 6.0 0.90 8.0 1.5 20.0 1.5 3.0 48.0]
  } elseif {$mpccode == "I52"} {
    set configArgValues [list N 100.0 6.0 0.90 8.0 1.036 20.0 1.5 3.0 40.0]
  } elseif {$mpccode == "V06"} {
    set configArgValues [list N 154.0 6.0 0.90 8.0 0.572 20.0 1.5 3.0 40.0]
  } elseif {$mpccode == "E12"} {
    set configArgValues [list N 50.0 6.0 0.90 8.0 1.8 20.0 1.5 3.0 23.0]
  }

  return $configArgValues
}

########################################################################################

proc FractionInside { fwhm radius pixelsize } {
  # how many pieces do we sub-divide pixels into?
  set piece 20.0

  #rescale FWHM and aperture radius into pixels (instead of arcsec)
  set fwhm [expr {$fwhm / $pixelsize} ]
  set radius [expr {$radius / $pixelsize} ]

  set max_pix_rad 30

  # check to make sure user isn't exceeding our built-in limits
  if {$radius >= $max_pix_rad} {
    puts "Warning: radius exceeds limit of $max_pix_rad\n");
    set radius 30
  }
#puts "DEBUG FractionInside fwhm $fwhm radius $radius pixelsize $pixelsize"

  # these values control the placement of the star on the pixel grid:
  #   (0,0) to make the star centered on a junction of four pixels
  #   (0.5, 0.5) to make star centered on one pixel
  set psf_center_x 0.5
  set psf_center_y 0.5

  set sigma2 [expr {$fwhm /  2.35} ]
  set sigma2 [expr {$sigma2 * $sigma2}]
  set radius2 [expr {$radius * $radius}]
  set bit [expr {1.0 / $piece}]

  set rad_sum 0.0
  set all_sum 0.0

  for {set i [expr {0 - $max_pix_rad}]} {$i < $max_pix_rad} {incr i} {
    for {set j [expr {0 - $max_pix_rad}]} {$j < $max_pix_rad} {incr j} {
      # now, how much light falls into pixel (i, j)?
      set pix_sum 0.0
      for {set k 0} {$k < $piece} {incr k} {
        set x [expr {($i - $psf_center_x) + ($k + 0.5)*$bit}]
        set fx [expr {exp(-($x*$x)/(2.0*$sigma2)) }]
        for {set l 0} {$l < $piece} {incr l} {
          set y [expr {($j - $psf_center_y) + (1 + 0.5) * $bit }]
          set fy [expr { exp(-($y*$y)/(2.0*$sigma2)) }]
          set inten [expr {$fx * $fy}]
          set this_bit [expr {$inten * $bit * $bit}]
          set pix_sum [expr {$pix_sum + $this_bit}]

          set rad2 [expr {$x*$x + $y*$y}]
          if {$rad2 <= $radius2} {
            set rad_sum [expr {$rad_sum + $this_bit}]
          }
        }
      }
      set all_sum [expr {$all_sum + $pix_sum}]
    }
  }

  set ratio [expr {$rad_sum / $all_sum}]

# puts "DEBUG rad_sum $rad_sum all_sum $all_sum ratio $ratio"

  return $ratio
}

########################################################################################

proc MagCorrected { config magnitude extinction } {
  lassign $config filter primary radius qe readnoise pixelsize skybrightness airmass fwhm obstdiameter

  return [expr {$magnitude + $extinction * $airmass}]
}

########################################################################################

proc EffectiveArea { config } {

  global M_PI filterExtinctCoeff magZeroPoint
  lassign $config filter primary radius qe readnoise pixelsize skybrightness airmass fwhm obstdiameter

  set primaryArea [expr {$M_PI * ($primary/2.0) * ($primary/2.0)}]
  if {$obstdiameter > 0} {
    set obstArea [expr {$M_PI * ($obstdiameter/2.0)  * ($obstdiameter/2.0)}]
    return [expr {$primaryArea - $obstArea}]
  }
  return $primaryArea
}


########################################################################################
proc SkyElectronsPerSecond { config extinction nphotons npix fraction} {
  lassign $config filter primary radius qe readnoise pixelsize skybrightness airmass fwhm obstdiameter

  set S [expr {pow(10.0, -0.4 * $skybrightness) * $nphotons * [EffectiveArea $config] * $qe * ($pixelsize * $pixelsize) }]
  return $S
}

########################################################################################
proc StarElectronsPerSecond { config magnitude extinction nphotons npix fraction} {

  lassign $config filter primary radius qe readnoise pixelsize skybrightness airmass fwhm obstdiameter

  set magC [MagCorrected $config $magnitude $extinction] 
  set Nstar [expr {(pow(10.0, -0.4 * $magC) * $nphotons) * [EffectiveArea $config] * $qe * $fraction }]
  return $Nstar
}

########################################################################################

proc SNR_Magnitude2Exposure { snr magnitude {config "I52"} } {

  global M_PI filterExtinctCoeff magZeroPoint

  if {[llength $config] == 1} {
    set newConfig [Mpc2ConfigExpcalc $config]
    if {[llength $newConfig] == 10} {
      set config $newConfig
    } else {
      puts "[dict get [info frame 0] file]:[dict get [info frame 0] line]: Unrecognized MPC code/config '$config' length [llength $config], null return"
      return ""
    }
  }
  if {[llength $config] != 10} {
    puts "[dict get [info frame 0] file]:[dict get [info frame 0] line]: Invalide telescope configuration '$config' length [llength $config], null return"
    return ""
  }

  lassign $config filter primary radius qe readnoise pixelsize skybrightness airmass fwhm obstdiameter

  set extinction $filterExtinctCoeff($filter)
  set nphotons $magZeroPoint($filter)
  set npix [expr { ($M_PI * $radius * $radius)/($pixelsize * $pixelsize) }]
  set fraction [ FractionInside $fwhm $radius $pixelsize ]

  set S [SkyElectronsPerSecond $config $extinction $nphotons $npix $fraction]
  set p $npix
  set R $readnoise
  set Nstar  [StarElectronsPerSecond $config $magnitude $extinction $nphotons $npix $fraction]
  
  set A [expr {4.0 * $Nstar * $Nstar * $p * $R * $R}]
  set B [expr {$snr * $snr * $snr * $snr * (-$Nstar - $p * $S) *  (-$Nstar - $p * $S)}]
  set C [expr {$Nstar * $snr * $snr}]
  set D [expr {$p * $S * $snr * $snr}]
  set E [expr {2.0 * $Nstar * $Nstar}]
  set t [expr {(sqrt($A+$B) + $C + $D) / $E}]

  return $t
}

########################################################################################

proc SNR_Exposure2Magnitude { snr exposure {config "I52"} } {

  global M_PI filterExtinctCoeff magZeroPoint

  if {[llength $config] == 1} {
    set newConfig [Mpc2ConfigExpcalc $config]
    if {[llength $newConfig] == 10} {
      set config $newConfig
    } else {
      puts "[dict get [info frame 0] file]:[dict get [info frame 0] line]: Unrecognized MPC code/config '$config' length [llength $config], null return"
      return ""
    }
  }
  if {[llength $config] != 10} {
    puts "[dict get [info frame 0] file]:[dict get [info frame 0] line]: Invalide telescope configuration '$config' length [llength $config], null return"
    return ""
  }

  lassign $config filter primary radius qe readnoise pixelsize skybrightness airmass fwhm obstdiameter

  set t $exposure
  set extinction $filterExtinctCoeff($filter)
  set nphotons $magZeroPoint($filter)
  set npix [expr { ($M_PI * $radius * $radius)/($pixelsize * $pixelsize) }]
  set fraction [ FractionInside $fwhm $radius $pixelsize ]

  set S [SkyElectronsPerSecond $config $extinction $nphotons $npix $fraction]
  set p $npix
  set R $readnoise
  
  set A [expr {$t * $snr * $snr}]
  set B [expr {$t * $snr}]
  set C [expr {4.0 * $p * $R * $R}]
  set D [expr {4.0 * $p * $S * $t}]
  set E [expr {$snr * $snr}]
  set F [expr {2.0 * $t * $t}]
  set Nstar [expr {(($A - $B * -sqrt($C + $D + $E) ) / $F) / [EffectiveArea $config] / $qe / $fraction}]
  set magRaw [expr {log10($Nstar / $nphotons) / -0.4}]
  set mag [expr {$magRaw - ($extinction * $airmass)}]

  return $mag
}

########################################################################################

proc Exposure_Magnitude2SNR { exposure magnitude {config "I52"} } {

  global M_PI filterExtinctCoeff magZeroPoint

  if {[llength $config] == 1} {
    set newConfig [Mpc2ConfigExpcalc $config]
    if {[llength $newConfig] == 10} {
      set config $newConfig
    } else {
      puts "[dict get [info frame 0] file]:[dict get [info frame 0] line]: Unrecognized MPC code/config '$config' length [llength $config], null return"
      return ""
    }
  }
  if {[llength $config] != 10} {
    puts "[dict get [info frame 0] file]:[dict get [info frame 0] line]: Invalide telescope configuration '$config' length [llength $config], null return"
    return ""
  }

  lassign $config filter primary radius qe readnoise pixelsize skybrightness airmass fwhm obstdiameter

  set extinction $filterExtinctCoeff($filter)
  set nphotons $magZeroPoint($filter)
  set fraction [ FractionInside $fwhm $radius $pixelsize ]
  set npix [expr { ($M_PI * $radius * $radius)/($pixelsize * $pixelsize) }]
  set p $npix
  set t $exposure
  set Nstar  [StarElectronsPerSecond $config $magnitude $extinction $nphotons $npix $fraction]
  set S [SkyElectronsPerSecond $config $extinction $nphotons $npix $fraction]
  set R $readnoise

  set signal [expr {$Nstar * $t}]
  set noise [expr {sqrt( ($Nstar * $t) + ($S * $p * $t) + ($p * $R * $R) ) }]
  set signal_to_noise [expr {$signal / $noise}]
  set signal_to_noise [format {%.3f} $signal_to_noise]

  return [list $signal $noise $signal_to_noise]
}

########################################################################################
