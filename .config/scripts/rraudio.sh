#!/bin/sh
pulseaudio --kill;
jack_control start;
jack_control exit;
pulseaudio --start
