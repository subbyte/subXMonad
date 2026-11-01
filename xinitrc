# wire dmenu to local commands
export PATH="$HOME/.local/bin:$PATH"

# no screensavor, set cursor shape
/usr/bin/xset s off -dpms &
/usr/bin/xsetroot -cursor_name left_ptr &

# audio setup
/usr/bin/gentoo-pipewire-launcher restart &

# notification daemon
/usr/bin/dunst &

# pipewire-aware xmonad session
exec /usr/bin/dbus-launch --exit-with-session /usr/bin/xmonad
