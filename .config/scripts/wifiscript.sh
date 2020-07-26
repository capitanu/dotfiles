#!/bin/sh
systemctl start wpa_supplicant
sleep 1
nmcli device wifi connect Darth_Vader_5G password vcccdcoc2010
