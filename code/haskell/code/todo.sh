#!/bin/sh
#

if [ $# -eq 0 ];then

    echo "$0 <files>"
    exit

fi

grep -n "TODO:" $* | cut -d: -f 1,2,4

