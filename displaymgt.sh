#!/usr/bin/env bash

################################################################
#
#                   Simple Display Management
#    
#    Usage: ./displaymgt.sh             # extended mode
#           ./displaymgt.sh --mirror    # mirror mode
#
#    Licence: GNU GPL v3.0
#
#    Author: Xiaokui Shu
#    Email: xiaokui.shu@ibm.com
#
################################################################

# all connected displays
DISPCONN=($(xrandr -q | awk '$2 == "connected" {print $1}'))

# all disconnected displays
DISPDISCONN=($(xrandr -q | awk '$2 == "disconnected" {print $1}'))

# laptop display
DISPLP=${DISPCONN[0]}

# potential external display
DISPEXT=${DISPCONN[1]}

if [ -n "$DISPEXT" ]; then
    if [ $1 = "--mirror" ]; then
        xrandr --output $DISPLP --mode 1920x1080 --output $DISPEXT --mode 1920x1080 --same-as $DISPLP
    else
        xrandr --output $DISPLP --auto --primary --output $DISPEXT --auto --right-of $DISPLP
    fi
else
    DISPALL=("${DISPCONN[@]}" "${DISPDISCONN[@]}")
    xrandr $(printf " --output %s --auto" ${DISPALL[@]})
fi
