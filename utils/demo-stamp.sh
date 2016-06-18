#!/bin/sh
#
# demo-stamp.sh
#
# Puts date stamp and reg-code prime values into FlightMaster demo file
#
# Build FlightMaster with 'makepp demo' to insert demo code
# into the executable
#

if [ $# -ne 3 ];then

	echo "Usage: demo-stamp.sh <file> <reg-code-prime> <days>"
	exit

fi

DATE_TAG="DATE"
REG_TAG="CODE"
ALERT_TAG="XXXMMM"

prc=$1
reg=$2
days=$3

if ! grep -q $DATE_TAG $prc || ! grep -q $REG_TAG $prc;then

	echo "Tag(s) missing from specified file"
	exit 1

fi

date=`date -d "$days days" +"%Y%m%d"`
date_str=`date -d "$days days" +"%e %b"`

#
# convert date and reg code from 32 bits to 4 bytes, so we can insert them in
# big-endian order into the PRC file
#

date0=$(( $date & 0xff ))
date1=$(( ($date & 0xff00) >> 8 ))
date2=$(( ($date & 0xff0000) >> 16 ))
date3=$(( ($date & 0xff000000) >> 24 ))

reg0=$(( $reg & 0xff ))
reg1=$(( ($reg & 0xff00) >> 8 ))
reg2=$(( ($reg & 0xff0000) >> 16 ))
reg3=$(( ($reg & 0xff000000) >> 24 ))

sed -i "s/$DATE_TAG/\d$date3\d$date2\d$date1\d$date0/g" $prc
sed -i "s/$REG_TAG/\d$reg3\d$reg2\d$reg1\d$reg0/g" $prc
sed -i "s/$ALERT_TAG/$date_str/g" $prc

