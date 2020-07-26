#!/bin/sh

export GDK_DPI_SCALE=0.5;
exec kdenlive;
echo $GDK_DPI_SCALE;
