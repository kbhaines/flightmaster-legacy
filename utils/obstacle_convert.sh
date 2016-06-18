#!/bin/sh
#
# Tool for converting TW's obstacle databases from CoPilot
# updates to FM6.02's creator and file type (obs0-9)
#

num=0
for f in *PDB;do 

	echo "Processing $f as obstacle DB $num"

	sed "s/Update Waypoint/Update Waypoin$num/;s/swpuGXBU/obs${num}BHMN/" $f> $f.tmp

	mv $f.tmp `basename $f .PDB`-2.pdb

	num=$(($num + 1))

done
