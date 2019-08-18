# initializes the variable HorizonLIst with the
# set of 25 azimuth, altitude pairs, starting azimuth
# 0 degrees (North) and increasing to the East. 

# IMPORTANT! IMPORTANT! IMPORTANT!
# Note that there is a value at the end for 360 degrees
# that should have the same altitude value as the entry
# for 0 degrees. This is mandantory, and helps simplify the
# interpolation algorithm a bit.
#
# This list if for the Catalina Schmidt telescope based
# on meaurements taken 12/9/02 by E. Beshore and R. Hill.
# Some of these might be able to go a bit lower. Consider
# them a bit conservative - but be aware that the path that
# the telescope takes to some of these points may result
# in the telescope going below some of these values.

# BE VERY CAREFUL AND BASE NEW ESTIMATES ON KNOWLEDGE
# OF THE TELESCOPES MOTION, ITS PHYSICAL CONFIGURATION,
# AND THE CONFIGURATION OF THE OBSERVING FLOOR

# 080204 Added SSS limits EJC
# 031505 Revised SSS Limits ECB

global LOCATION

if {$LOCATION == "703"} {

    set HorizonList [list \
			 {0 45.0} \
			 {15 37.0} \
			 {30 30.0} \
			 {45 25.0} \
			 {60 19.9} \
			 {75 19.9} \
			 {90 19.9} \
			 {105 19.9} \
			 {120 19.9} \
			 {135 19.9} \
			 {150 19.9} \
			 {165 19.9} \
			 {180 19.9} \
			 {195 19.9} \
			 {210 19.9} \
			 {225 19.9} \
			 {240 19.9} \
			 {255 19.9} \
			 {270 19.9} \
			 {285 19.9} \
			 {300 19.9} \
			 {315 25.0} \
			 {330 30.0} \
			 {345 37.0} \
			 {360 45.0}]

} elseif {$LOCATION == "G96" || $LOCATION == "V06"} {

    set HorizonList [list \
			 {0 45.0} \
			 {15 30.0} \
			 {30 20.0} \
			 {45 20.0} \
			 {60 20.0} \
			 {75 20.0} \
			 {90 20.0} \
			 {105 20.0} \
			 {120 20.0} \
			 {135 20.0} \
			 {150 20.0} \
			 {165 20.0} \
			 {180 20.0} \
			 {195 20.0} \
			 {210 20.0} \
			 {225 20.0} \
			 {240 20.0} \
			 {255 20.0} \
			 {270 20.0} \
			 {285 20.0} \
			 {300 20.0} \
			 {315 20.0} \
			 {330 20.0} \
			 {345 30.0} \
			 {360 45.0}]

} elseif {$LOCATION == "E12"} {

    set HorizonList [list \
			 {0 10.0} \
			 {15 10.0} \
			 {30 10.0} \
			 {45 6.0} \
			 {60 6.0} \
			 {75 6.0} \
			 {90 6.0} \
			 {105 6.0} \
			 {120 6.0} \
			 {135 15.0} \
			 {150 20.0} \
			 {165 35.0} \
			 {180 35.0} \
			 {195 37.0} \
			 {210 34.0} \
			 {225 16.0} \
			 {240 14.0} \
			 {255 14.0} \
			 {270 13.0} \
			 {285 12.0} \
			 {300 10.0} \
			 {315 10.0} \
			 {330 15.0} \
			 {345 10.0} \
			 {360 10.0}]

} elseif {$LOCATION == "I52" || $LOCATION == "21-IN"} {

    set HorizonList [list \
			 {0 45.0} \
			 {15 45.0} \
			 {30 45.0} \
			 {45 45.0} \
			 {60 35.0} \
			 {75 25.0} \
			 {90 20.0} \
			 {105 20.0} \
			 {120 20.0} \
			 {135 20.0} \
			 {150 30.0} \
			 {165 25.0} \
			 {180 25.0} \
			 {195 20.0} \
			 {210 20.0} \
			 {225 20.0} \
			 {240 20.0} \
			 {255 20.0} \
			 {270 20.0} \
			 {285 20.0} \
			 {300 25.0} \
			 {315 30.0} \
			 {330 35.0} \
			 {345 40.0} \
			 {360 45.0}]

}

