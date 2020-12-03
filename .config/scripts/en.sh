#!/bin/sh

PWD=$(pwd)
FZF=$(fzf)
FULLPATH="${PWD}${FZF:1}"
emacsclient -e "(open-buffer-with \"${FULLPATH}\")"
#alias en="emacsclient -e \"(open-buffer-with \$(fzf))\""
