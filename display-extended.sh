#!/usr/bin/env bash

if xrandr -q | grep -Fq eDP1
then
    LPMONITOR="eDP1"
else
    LPMONITOR="LVDS1"
fi

EXTMONITOR=$(xrandr -q | grep " connected" | awk '{print $1}' | grep -v $LPMONITOR)

if [ -n "$EXTMONITOR" ]; then
    xrandr --output $LPMONITOR --auto --primary --output $EXTMONITOR --auto --right-of $LPMONITOR
else
    xrandr --output $LPMONITOR --auto --output DP1 --auto --output DP2 --auto --output HDMI1 --auto --output HDMI2 --auto
fi
