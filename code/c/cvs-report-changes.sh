#!/bin/sh
#
# cvs-report-changes.sh
#
# Reports a list of the changes made since the specified version of FlightMaster
#

if [ $# -ne 1 ];then
	
	echo "Usage: $0 <tag>"
	exit 0

fi

VERSION=$1
MODULE=palm/MiniNav

cvs log -Nr$VERSION:: `cvs diff -r$VERSION 2>/dev/null | grep "^Index: " | cut -f2 -d' '`
