#!/bin/bash

if pgrep xcompmgr &>/dev/null; then
    echo "Turning xcompmgr OFF"
    pkill xcompmgr &
else
    echo "Turning xcompmgr ON"
    xcompmgr -c -l0 -t0 -r0 -o.00 &
fi

exit 0
