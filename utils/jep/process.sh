#!/bin/sh
#

EXTLOGIN="khaines@p4-7057.uk2net.com"
AREAS="EU NA AUSNZ"
UPLOADDIR="/home/sites/site12/jeppesen/"
ARCHIVE="$HOME/fm/jep/Archive"

export PATH=$PATH:.

if [ $# -ne 2 ];then

cat <<-EOF
Usage: processjep.sh <cycle> <zipfile>

1. Download the ZIP file from Jeppesen into $ARCHIVE

2. Run this process

3. Files are uploaded to website, and copied to $ARCHIVE

4. Verify that AIRAC cycle database has data for this cycle

EOF
exit 0

fi

CYCLE=$1
JEPFILE=$2

rm fmaster*.pc
unzip "$JEPFILE"

if [ ! -f fmaster.pc ];then

	echo "fmaster.pc not found!"
	exit 1

fi

. makedb.bat

#
# by now there should be a file for each area
#

for f in $AREAS;do

	fname=$f$CYCLE.zip

	if [ ! -f $fname ];then	

		echo "Error: $fname not found"
		exit 1

	fi

	#scp $fname $EXTLOGIN:$UPLOADDIR
	#ssh $EXTLOGIN md5sum $UPLOADDIR/$fname
	md5sum $fname
	mv $fname "$ARCHIVE"

done
