#!/bin/sh
GOOGLE_DNS="$(grep -i "8.8.8.8" /etc/resolv.conf)"
TO_REMOVE_GOOGLE_DNS=false
tmpfile=$(mktemp)

if [ -z "$GOOGLE_DNS" ]
then
    echo "Google DNS will be added as your DNS"
    TO_REMOVE_GOOGLE_DNS=true
    sudo echo "nameserver 8.8.8.8" >> /etc/resolv.conf
else
    TO_REMOVE_GOOGLE_DNS=false
    echo "Google DNS is currently active in your /etc/resolv.conf file"
fi

if [ "$TO_REMOVE_GOOGLE_DNS" = true ]
then
    sudo sed '/8.8.8.8/d' /etc/resolv.conf > ${tmpfile}
    cat ${tmpfile} > /etc/resolv.conf
    rm -f ${tmpfile}
    echo "Removed Google DNS from your config file"
else
    echo "Did not remove Google DNS, as it was there before"
fi

