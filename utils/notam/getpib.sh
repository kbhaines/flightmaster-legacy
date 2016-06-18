#!/bin/sh
#
#

NOTAMFILTER=/home/khaines/notam/notam2fma

cd /home/sites/site12/web/downloads/notam

wget -q -O - http://www.nats.co.uk/operational/pibs/pib3.shtml | $NOTAMFILTER
