#!/bin/sh
#

if [ $# -ne 2 ];then

	echo "Usage: loader.sh <count> <product>"
	exit

fi

echo "Input sql password:"
read -s pw

count=$1
product=$2

for x in `seq 1 $count`;do

	token=`dd if=/dev/urandom count=16 2>/dev/null | md5sum | cut -f 1 -d' '`

	mysql flightmaster -u khaines -p$pw -e "insert into registrations (token, product_id, attempts) values ('$token', '$product', 0)"

	echo "http://www.flight-master.com/recover.php?txn_id=$token"

done

echo

