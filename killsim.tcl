#!/usr/bin/tclsh

# kill all TCS simulators we can find:
set count 0
set maxcount 10
while {1 && $count < $maxcount} {
    set cmdline "ps -fea | grep tcs-sim.tcl | grep -v grep"
    catch { eval exec $cmdline } result

    set pid [ lindex $result 1 ]

    if {![ string is integer $pid ]} {
	break
    }

    set cmdline "kill -9 $pid"
    puts "killing TCS simulator $pid"
    catch { eval exec $cmdline } result
    incr count
}
