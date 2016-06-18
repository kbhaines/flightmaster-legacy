#!/bin/sh
#
# Helper script for building FlightMaster
#

#
# Debugging is turned on for modules by specifying the module
# name in the file 'debug-modules', and passing -d to this script
#

DEBUG=no
if [ "$1" == "-d" ];then

	[ -f debug-modules ] && DEBUGLIST=`cat debug-modules`
	DEBUG=yes

	shift 1

fi

RUN="no"
if [ "$1" == "-r" ];then

	RUN="yes"
	shift 1

fi

ARGS=""
if [ "$DEBUGLIST" ];then

	ARGS="$ARGS DBG=$DEBUGLIST"

fi

if [ "$1" == "-s" ];then	

	ARGS="splint"

fi

ARGS="$ARGS $*"

if makepp $ARGS;then

	if [ "$RUN" == "yes" ];then

		if [ "$DEBUG" == "yes" ];then

			./sim.sh -d

		else

			./sim.sh

		fi

	fi

fi
