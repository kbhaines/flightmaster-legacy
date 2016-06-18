#!/bin/sh
#
#

f=$1

width=22
output=filter
rcpfile=filter-icons.rcp

if [ ! -f $f ];then

	echo "$f - not found"
	exit 1

fi


echo '#include "ResourceDefines.h"' > $rcpfile

#
# icon, airspace and airway filters
#

for i in `seq 0 11`;do

	x=`expr $i \* $width`

	off="$output$i-off.bmp"
	on="$output$i-on.bmp"
	onl="$output$i-onl.bmp"

	# filter = off
	convert $f +antialias +compress -crop ${width}x${width}+$x+0 -strokewidth 2 -draw "stroke red line 2 2 18 18" $off

	# filter = on
	convert $f +compress -crop ${width}x${width}+$x+0  $on

	# filter = on+label
	convert $f +compress -crop ${width}x${width}+$x+0 -stroke black -draw "fill #FFFF00 polygon 5 0 18 0 18 6 5 6 2 3" $onl

	cat <<-EOF >> $rcpfile
BITMAP ID (FilterIcon0off+$i) COMPRESS 
BEGIN
	BITMAP "$off" BPP 4 DENSITY 1
	BITMAP "$off" BPP 8 DENSITY 1 TRANSPARENT 0 255 0
END
BITMAP ID (FilterIcon0on+$i) COMPRESS 
BEGIN
	BITMAP "$on" BPP 4 DENSITY 1
	BITMAP "$on" BPP 8 DENSITY 1 TRANSPARENT 0 255 0
END
BITMAP ID (FilterIcon0onl+$i) COMPRESS 
BEGIN
	BITMAP "$onl" BPP 4 DENSITY 1
	BITMAP "$onl" BPP 8 DENSITY 1 TRANSPARENT 0 255 0
END
EOF


done

#
# altitude filters
#

for i in `seq 12 13`;do

	icon=`expr $i - 12`
	x=`expr $i \* $width`
	bmp="$output$i.bmp"
	convert $f +compress -crop ${width}x${width}+$x+0 $bmp

	cat <<-EOF >> $rcpfile
BITMAP ID (FilterAltIcon+$icon) COMPRESS 
BEGIN
	BITMAP "$bmp" BPP 4 DENSITY 1
	BITMAP "$bmp" BPP 8 DENSITY 1 TRANSPARENT 0 255 0
END
EOF

done

#
# airway width toggle
#

for i in `seq 14 15`;do

	icon=`expr $i - 13`
	x=`expr $i \* $width`
	bmp="$output$i.bmp"
	convert $f +compress -crop ${width}x${width}+$x+0 $bmp

	cat <<-EOF >> $rcpfile
BITMAP ID (FilterAirwayWidthIcon+$icon) COMPRESS 
BEGIN
	BITMAP "$bmp" BPP 4 DENSITY 1
	BITMAP "$bmp" BPP 8 DENSITY 1 TRANSPARENT 0 255 0
END
EOF

done
