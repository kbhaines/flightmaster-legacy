#!/bin/sh
#

SIMPATH_BASE=/cygdrive/c/Palm

usage() {

	cat <<-EOF

Usage: $0 [-q] [-d] [-t <type>] [-c <ssf>] [-f <FlightMaster.prc>]

where:

	-d = debug sim
	-t = type of simulator (default=Generic)
	-c = Config file (Snapshot File)
	-f = FlightMaster.prc file to load

	-q = Output path to simulator

EOF
exit

}

SIMCLASS=Release
SIMTYPE=Generic
SSF=default.ssf
QUERY=n
while getopts ":hdt:c:f:q" option;do

	case $option in

		d)	SIMCLASS=Debug
			;;

		t)  SIMTYPE=$OPTARG
			;;

		c)  SSF=$OPTARG
			;;

		f)  FMPRC=$OPTARG
			;;

		q)  QUERY=y
			;;

		h)  usage;;

		\?) usage;;
		*)	usage;;

	esac

done

SIMPATH=$SIMPATH_BASE/$SIMCLASS/$SIMTYPE

if [ ! -d $SIMPATH ];then

	echo "Error : Couldn't find $SIMPATH"
	exit 1

fi

if [ $QUERY == y ];then

	echo $SIMPATH
	exit 0

fi

if [ ! -z "$FMPRC" ];then

	[ -d $SIMPATH/AutoRun ] || mkdir $SIMPATH/AutoRun
	cp "$FMPRC" $SIMPATH/AutoRun

fi

cd $SIMPATH
exec ./PalmSim.exe -storagesnapshotfile:$SSF
