#!/bin/sh
#
# Utility to take the datafile from Crabb Computing (www.crabb.biz) and convert
# to FMA file
#
# Deletes non-A/D class airspace data from file, adds UK MATZ & danger areas &
# airways
#


if [ $# -ne 2 ];then

	echo "crabb2fma <crabb file> <airdb fma file>"
	exit 

fi

INFILE=$1
AIRDB=$2
TMP=/tmp/$$.fma

sed -n '1,/CLASS A AIRSPACE/d;/AC W/,$d;s/AC B/AC A/;p' $INFILE > $TMP

openair2fma.pl $TMP > $TMP.2

fma-filter.pl $AIRDB GWS 60:-15:48:3 >> $TMP.2

cat $TMP.2

rm $TMP $TMP.2
