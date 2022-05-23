#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Auto start programs & commands
if [ -f /usr/bin/setxkbmap ]; then
    setxkbmap -layout se
fi

if [ -f /usr/bin/feh  ]; then
    feh --bg-scale ~/Pictures/mountain.jpg &
fi

if [ -f /usr/local/bin/ckb-next ]; then
    ckb-next &
fi

if [ -f /usr/bin/protonmail-bridge ]; then
    protonmail-bridge &
fi

if [ -f /usr/bin/protonvpn ]; then
    protonvpn &
fi

if [ -f /usr/bin/xrandr ]; then
    xrandr --output DP-4 --auto --left-of DVI-D-0
    xrandr --output DP-4 --mode 1920x1080 --rate 144.00 --primary
    xrandr --output HDMI-0 --left-of DP-4
fi

if [ -f /usr/bin/compton ]; then
    compton -f --daemon
fi


if [ -f /usr/bin/i3 ]; then
    exec i3
fi
