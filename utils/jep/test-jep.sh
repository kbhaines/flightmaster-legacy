#!/bin/sh
#
# Jeppesen.ssf settings are:
# - 40nm range on map #1
# - all waypoints, airspace and airways on
# - no labels
# - no registration code
# - preferences installed i.e. not fresh installation
#
# Waypoints:
#  EU - egbp egll egnt lfrc lima lggg lfml lfbp ellx
#  US - klax kjfk komn kphl cyyc cykz
#  AUS- waoo sy yscb ybbn nzaa

AREAS="EU NA AUSNZ"
#AREAS="NA"
ARCHIVE="$HOME/fm/jep/Archive"
TEST_UTILS=/cygdrive/y/edev
TEST_MANAGER=$TEST_UTILS/test-manager.sh
FMBINARY=~/fm/jep/FlightMaster.prc

export PATH=$PATH:.:$TEST_UTILS

if [ $# -ne 1 ];then

cat <<-EOF
Usage: test-jep.sh <cycle>

EOF
exit 0

fi

CYCLE=$1

if [ ! -x $TEST_MANAGER ];then

	echo "$TEST_MANAGER not found"
	exit

fi

set -x
for area in $AREAS;do

	navdata_test=test-$area.zip
	navdata=$ARCHIVE/$area$CYCLE.zip
	unzip -o "$navdata"
	test-manager.sh -p "CoPilot_Waypoint.pdb FlightMaster7-Airspace.pdb" -f "$FMBINARY" -l "-t Treo680 -c jeppesen.ssf" -x test-$area

done
