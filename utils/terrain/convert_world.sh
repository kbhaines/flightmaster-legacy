#!/bin/sh
#
# This script processes raw globe data into a complete flight-master
# terrain database. Globe data should be in gzip format in
# the src directory.
#
# The script requires binary components: convert_globe, resample_globe
# and stitch
# 

src=$1
dst=$2

if [ $# != 3 ];then

	echo "Usage: convert_world.sh <source directory> <destination>"
	echo "  where source dir contains NGDC files in gzip format"
	exit

fi

#
# resample produces the three resolutions of terrain database for
# a chunk of the map 90x90 degrees
#

resample() {

	top=$dst/$1
	bottom=$dst/$2
	result=$dst/$3

	echo "Resampling $top & $bottom to $result"

	if [ -f $result-r1 ];then 

		return;

	fi

	cat $top $bottom > $dst/tmp1
	rm $top $bottom
	./resample_globe 10800 10800 8192 8192 $dst/tmp1 ${result}-r1
	rm $dst/tmp1
	./resample_globe 8192 8192 4096 4096 ${result}-r1 ${result}-r2
	./resample_globe 4096 4096 2048 2048 ${result}-r2 ${result}-r4

}

#
# build the binary utilities
#
#

gcc convert_globe.c -o convert_globe
gcc resample_globe.c -o resample_globe
gcc stitch.c -o stitch

#
# convert each globe format file into FlightMaster format
#

for f in $src/*.gz;do
	
	n=$dst/`basename $f .gz`
	echo "Processing $f to $n"
	if [ ! -f $n ]; then

		gunzip -c $f > $dst/tmp
		./convert_globe $dst/tmp $n
		rm $dst/tmp

	fi

done

# northern hemisphere

resample a10g e10g ae
resample b10g f10g bf
resample c10g g10g cg
resample d10g h10g dh

# southern hemisphere

resample i10g m10g im
resample j10g n10g jn
resample k10g o10g ko
resample l10g p10g lp

# combine two hemispheres and stitch the pieces together

rowsize=8192
for r in r1 r2 r4;do

	echo "Processing resolution $r, rowsize = $rowsize"

	echo "Northern: " 
	./stitch $rowsize $dst/ae-$r $dst/bf-$r $dst/cg-$r $dst/dh-$r $dst/north

	echo "Southern: " 
	./stitch $rowsize $dst/im-$r $dst/jn-$r $dst/ko-$r $dst/lp-$r $dst/south

	cat $dst/north $dst/south > $dst/$r.raw
	rm $dst/north $dst/south
	rowsize=`expr $rowsize / 2`

done

# combine the pieces together

#cat $dst/r1 $dst/r2 $dst/r4 > $dst/world-terrain
#rm $dst/r[124]
