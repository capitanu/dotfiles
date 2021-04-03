#!/bin/sh
sleep 1
xrandr -s 1920x1080
sleep 0.5
xrandr --output DP-1 --auto --left-of eDP-1
sleep 2
feh --bg-scale /home/calin/.config/starwars.jpg
