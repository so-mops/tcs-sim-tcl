proc CenterFocusWin { winid {winnum 0} {wait 1} } {

    upvar $winid win
    update idletasks

    set sw [winfo screenwidth $win]
    set sh [winfo screenheight $win]

    set w [winfo width $win]
    set h [winfo height $win]

    set x [expr {$sw / 2 - $w / 2}]

    if {$winnum != 0} {
	set x [expr {$x + ($x/2)}]
    } else {
	set x [expr {$x - ($x/2)}]
    }

    set y [expr {$sh / 2 - $h / 2}]

    wm geometry $win +$x+$y
    wm protocol $win WM_DELETE_WINDOW { }
    wm protocol $win WM_MINIMIZE_WINDOW { }
    
    focus $win

    if {$wait} {

	tkwait window $win
    }
}

##################################################################################
#                                                                                #
# Protocol for Message Box                                                       #
# Call it as follows:                                                            #
#       MessBox title message boxtype:                                           #
# where:                                                                         #
#       "title" is a string that will be used in the dialog box window title     #
#       "message" is a string that contains the message to be displayed          #
#       "boxtype" describes the supported predefined sets of buttons. Allowed    #
#               values are ok, okcancel, retrycancel, yesno, yesnocancel,        #
#               skipabort, and retryskipabort                                    #
# None of the arguments are optional.                                            #
# MessBox returns a string that corresponds to the name of the button pressed.   #
# this program. (NOT FINISHED)                                                   #
#                                                                                #
# This program needs the proc CenterFocusWin first in order to operate properly. #
#                                                                                #
##################################################################################

proc MessBox { title messagetext type { winnum 0 } } {
   
    global gresult

    set color17 "#ffffff"
    set color22 "#000000"

    # determine if there are any MessBoxes present:
    set counter 1
    while {[ winfo exists .mb$counter ]} {
	incr counter
    }

    # name the toplevel MessBox accordingly (.mb1 if first, .mb2 if second, etc.)
    set mbName ".mb$counter"

    # first, create widgets
       
    # forced winnum to window 0

    set top [toplevel $mbName -class Dialog ]

    wm title $mbName $title
    frame $mbName.top -bd 1
    grid configure $mbName.top -row 0 -column 0
    frame $mbName.bot -bd 1
    grid configure $mbName.bot -row 1 -column 0

    # Message
   
    label $mbName.top.mess -text $messagetext -font mc -wraplength 300 -borderwidth 2

    grid config $mbName.top.mess -column 0 -row 0 -sticky nsew -padx 10 -pady 6

    switch -- $type {

	dialog {
	    set numbuts 0
	}

        ok {
            set numbuts 1
            set but0text "OK"
        }

        okcancel {
            set numbuts 2
            set but0text "OK"
            set but1text "CANCEL"
        }

        retrycancel {
            set numbuts 2
            set but0text "RETRY"
            set but1text "CANCEL"
        }

        yesno {
            set numbuts 2
            set but0text "YES"
            set but1text "NO"
        }

        yesnocancel {
            set numbuts 3
            set but0text "YES"
            set but1text "NO"
            set but2text "CANCEL"
        }

        retryskipabort {
            set numbuts 3
            set but0text "RETRY"
            set but1text "SKIP"
            set but2text "ABORT"
        }

        skipabort {
            set numbuts 2
            set but0text "SKIP"
            set but1text "ABORT"
        }

        default {
            return
        }
    }

    # Buttons

    if {$numbuts > 0} {

	button $mbName.bot.but0 \
	    -foreground $color17 -background darkblue -activebackground $color22 \
	    -activeforeground $color17 -text $but0text -font mc -borderwidth 2 \
	    -command "MessBut $but0text $mbName"
	
	grid config $mbName.bot.but0 -row 1 -column 0 -padx 10 -pady 6
	
	if {$numbuts > 1} {
	    button $mbName.bot.but1 \
		-foreground $color17 -background darkblue -activebackground $color22 \
		-activeforeground $color17 -text $but1text -font mc -borderwidth 2 \
		-command "MessBut $but1text $mbName"
	    
	    grid config $mbName.bot.but1 -row 1 -column 1 -padx 10 -pady 6
	    
	    if {$numbuts > 2 } {
		
		button $mbName.bot.but2 \
		    -foreground $color17 -background  darkblue -activebackground $color22 \
		    -activeforeground $color17 -text $but2text -font mc -borderwidth 2 \
		    -command "MessBut $but2text $mbName"
		
		grid config $mbName.bot.but2 -row 1 -column 2 -padx 10 -pady 6
		
	    }
	}
    }	

    if {$numbuts == 0} {

	CenterFocusWin top $winnum 0
	return $mbName

    } else {

	CenterFocusWin top $winnum
	return $gresult
    }
}

proc MessBut { Mess name } {

    global gresult
    set gresult $Mess
    destroy $name
}

font create mg -family Geneva -size 12 -weight normal
font create mc -family Courier -size 12 -weight normal
