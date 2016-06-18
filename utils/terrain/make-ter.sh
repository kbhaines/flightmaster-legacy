#!/bin/bash
#
# top-level file to build FM terrain datasets from the global file
#

GLOBAL=/cygdrive/h/world-terrain

WORKDIR=/cygdrive/f/

echo "Using $GLOBAL, output to $WORKDIR"

#
# build the binary
#

gcc make-fm-terrain.c -o make-fm-terrain

#./make-fm-terrain 0 -6 -38 45 $GLOBAL FlightMaster7-Terrain.fma
#zip safr-terrain.zip FlightMaster7-Terrain.fma

#./make-fm-terrain 30 -90 -65 -33 $GLOBAL FlightMaster7-Terrain.fma
#zip sa-terrain.zip FlightMaster7-Terrain.fma

#./make-fm-terrain 75 -170 20 -50 $GLOBAL FlightMaster7-Terrain.fma
#zip na-terrain.zip FlightMaster7-Terrain.fma

#./make-fm-terrain 75 -30 -35 40 $GLOBAL FlightMaster7-Terrain.fma
#zip eu-terrain.zip FlightMaster7-Terrain.fma

#./make-fm-terrain -10 110 -55 180 $GLOBAL FlightMaster7-Terrain.fma
./make-fm-terrain 30 90 -55 180 $GLOBAL $WORKDIR/FlightMaster7-Terrain.fma
zip -j $WORKDIR/ausnz-terrain.zip $WORKDIR/FlightMaster7-Terrain.fma

