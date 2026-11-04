#!/bin/sh
# volumectl.sh - control the ALSA Master volume via amixer
#
# usage: volumectl.sh +|-|m
#   +  unmute if muted, raise volume by 5% of the full range, print volume bar
#   -  unmute if muted, lower volume by 5% of the full range, print volume bar
#   m  toggle mute; print an empty (0 volume) bar when muted,
#      the volume bar when unmuted

set -u

program_name=${0##*/}
mixer_control=Master

usage() {
    echo "usage: $program_name +|-|m" >&2
    exit 2
}

print_volume_bar() {
    # Convert to decimal (including leading zeros) and cap at 100%.
    volume_percent=$(printf '%s\n' "$1" | awk '
        match($0, /\[[0-9]+%\]/) {
            percent = substr($0, RSTART + 1, RLENGTH - 3) + 0
            print (percent > 100 ? 100 : percent)
            exit
        }
    ')
    if [ -z "$volume_percent" ]; then
        echo "$program_name: cannot read volume of $mixer_control" >&2
        exit 1
    fi

    # Each symbol represents 5%; round to the nearest whole symbol.
    filled_symbols=$(( (volume_percent * 20 + 50) / 100 ))
    volume_bar=
    symbol_index=0
    while [ "$symbol_index" -lt 20 ]; do
        if [ "$symbol_index" -lt "$filled_symbols" ]; then
            volume_bar="$volume_bar▮"
        else
            volume_bar="$volume_bar▯"
        fi
        symbol_index=$((symbol_index + 1))
    done
    printf '%s' "$volume_bar"
}

[ $# -eq 1 ] || usage
case $1 in
    +|-|m) ;;
    *) usage ;;
esac

# amixer's output is parsed below; keep its labels and switch states stable.
LC_ALL=C
export LC_ALL

if ! command -v amixer >/dev/null 2>&1; then
    echo "$program_name: amixer not found (install alsa-utils)" >&2
    exit 1
fi

case $1 in
    # Unmute (no-op if already unmuted) and adjust in one amixer call.
    +) mixer_output=$(amixer sset "$mixer_control" unmute 5%+) || exit 1
       print_volume_bar "$mixer_output" ;;
    -) mixer_output=$(amixer sset "$mixer_control" unmute 5%-) || exit 1
       print_volume_bar "$mixer_output" ;;
    m) mixer_output=$(amixer sset "$mixer_control" toggle) || exit 1
       # Any [on] channel means unmuted; otherwise require an explicit [off].
       # A muted bar shows 0%, regardless of the stored volume.
       if printf '%s\n' "$mixer_output" | grep -q '\[on\]'; then
           print_volume_bar "$mixer_output"
       elif printf '%s\n' "$mixer_output" | grep -q '\[off\]'; then
           print_volume_bar '[0%]'
       else
           echo "$program_name: cannot read mute state of $mixer_control" >&2
           exit 1
       fi ;;
esac
