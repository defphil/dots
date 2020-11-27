#!/bin/bash

# Load resources
xrdb -merge .Xresources

# Set the background color<
xsetroot -solid midnightblue

# Fire up apps
/usr/bin/xmodmap /home/phil/.Xmodmap &
xset r rate 200 50 &
picom -b --config ~/.config/picom.conf &
exec xmonad
