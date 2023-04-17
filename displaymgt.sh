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
    if [[ $1 = "--mirror" ]]; then

        # laptop resolution
        RESLP=$(xrandr -q | sed -n "/$DISPLP/{n;p;}" | awk '{print $1}')

        # external display resolution
        RESEXT=$(xrandr -q | sed -n "/$DISPEXT/{n;p;}" | awk '{print $1}')

        # find the smaller resolution and assign it to $RESF
        RESLPs=(${RESLP//x/ })
        RESEXTs=(${RESEXT//x/ })
        if [ "${RESLPs[0]}" -gt "${RESEXTs[0]}" ]; then
            RESF=$RESEXT
        else
            RESF=$RESLP
        fi

        # mirror the laptop display using the smaller resolution
        xrandr --output $DISPLP --mode $RESF --output $DISPEXT --mode $RESF --same-as $DISPLP
    else
        # extended mode
        xrandr --output $DISPLP --auto --primary --output $DISPEXT --auto --right-of $DISPLP
    fi
else
    # no external display: remove all display spaces by using "--auto"
    DISPALL=("${DISPCONN[@]}" "${DISPDISCONN[@]}")
    xrandr $(printf " --output %s --auto" ${DISPALL[@]})
fi
