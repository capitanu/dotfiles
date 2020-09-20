#!/bin/bash
TOGGLE=$HOME/.config/scripts/.toggle

if [ ! -e $TOGGLE ]; then
    touch $TOGGLE
    xmodmap /home/calin/.Xmodmap
else
    rm $TOGGLE
    xmodmap /home/calin/.Xmodmapse
fi
