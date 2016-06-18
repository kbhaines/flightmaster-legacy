#!/bin/sh
#

while [ 1 ];do
	echo -n "Ready:"
	read s
	if [ "$s" == "r" ] ;then 

		./FlightMaster.exe r:/jep/eu-wps.dat r:/jep/eu-asp.dat r:/wtdata/t EGVN 200 com4 +RTS -s

	else

		echo "Compiling"
		ghc --make -package GLUT -O2 FlightMaster.hs 2>&1 | tee errors.txt
		echo "Done."
	fi
done
