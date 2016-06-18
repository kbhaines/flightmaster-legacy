#!/bin/sh

#
# the translation table file has tab-separated columns, one column
# per language, with English in column 1.
#
# Strings in the file to be processed should be in English, but
# with an '_' character. E.g. "_My String"
#


lang=$1
trans=$2
src=$3
dst=$4

echo "FlightMaster string translator - parameters: $*"

if [ $# -ne 4 ];then

	echo "Incorrect parameter count"
	exit

fi

# English - default, easy, just strip out the _ characters

if [ $lang = "English" ];then

	sed 's/"_/"/g' $src > $dst

	exit

fi


#
# choose column from the translation table
#

case $lang in

	French) column=2;;
	
	German) column=3;;

	Spanish) column=4;;

	*) echo "No language specified";exit 1;;

esac

#
# produce a sed translation script from the translation table file
#

cut -f 1,$column $trans | sed 's/\//\\\//g;s/^/_/;s/\(.*\)	\(.*\)/s\/"\1"\/"\2"\/g/g' > tmp$$.sed

# run the script on the input file

sed -f tmp$$.sed $src > $dst
rm tmp$$.sed

if grep \"_ $dst;then

	echo "Untranslated string(s) - See above"
	exit 1

fi
