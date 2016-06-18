#!/bin/sh
#
# generates list of hard surfaces from input file, which must be in format:
#
# <surface name> <y|n>\n
#
# where y indicates a hard surface.
#

if [ $1 == "" ];then
	echo "Usage: hardsurfaces.sh <surfaces-file>"
	exit 0;
fi

grep -e "y$" $1 | cut -f 1 -d " " |sort -u | while read x;do echo \"$x\",;done
echo \"zz\"
