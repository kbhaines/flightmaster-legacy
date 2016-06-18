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
dst=$lang.rcp

echo "FlightMaster RCP language translator, called with $*"

if [ $# -ne 2 ];then

	echo "Incorrect parameter count"
	exit

fi

echo "TRANSLATION \"$lang\"" > $dst
echo "BEGIN" >> $dst

if [ $lang = "English" ];then

	cut -f 1 $trans | sed 's/\(.*\)/"_\1" = "\1"/' >> $dst

	echo "END" >> $dst
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

cut -f 1,$column $trans | sed 's/\(.*\)	\(.*\)/"_\1" = "\2"/' >> $dst
echo "END" >> $dst
