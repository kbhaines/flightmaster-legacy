#!/bin/sh
#

SIM_SCREEN_SIZE=320x320
MAX_TEST_TIME=360
BINARY=/cygdrive/y/edev/FlightMaster.prc

usage() {

	cat<<-EOF

$0 [ -r | -x ] [ RECORD_OPTIONS | RUN_OPTIONS ] <test>

where <test> is the name of the ZIP file containing the test

-l <opts>	Launch options to send to launch-sim.sh script

-f <path>	FlightMaster.prc binary (default: $BINARY)

RECORD_OPTIONS:

-p <file> 
	Copy the specified PDB file(s) to AutoLoad (use quotes if more
	than one file is being copied)

-e	Edit the test script after recording (before creating test zip)

RUN_OPTIONS:

OTHER OPTIONS:

-a	<zipfile>	Accept results, removing failure-detected files such
				as diff, logfile etc.

EOF

exit

}

CheckBinary() {

	if ! grep -q TEST_FRAMEWORK "$BINARY";then

		return 1

	fi

	return 0

}

RunSimulator() {

	launch-sim.sh -f "$BINARY" $LAUNCH_OPTS &
	#launch-sim.sh $LAUNCH_OPTS &
	SIM_PID=$!

}

ProcessRawImageFiles () {

	# convert raw files in directory $1 to png files, and
	# remove the raw files

	targ_dir=$1

	for rawfile in $targ_dir/*.raw;do

		targ_name=`basename $rawfile .raw`.png
		convert -size $SIM_SCREEN_SIZE -depth 8 rgb:$rawfile $targ_dir/$targ_name
		[ $? -eq 0 ] && rm -f $rawfile

	done
	
}

LoadDatabases() {

	loaded_dbs=""
	src_dir=$1
	for db in $src_dir/*.pdb;do

		cp $db $SIM_DB_LOAD_DIR

	done

}

UnloadDatabases() {

	rm -f $SIM_DB_LOAD_DIR/*.pdb

}

AcceptResults() {

	resultsfile=$1

	if [ ! -f $resultsfile ];then

		echo "$resultsfile not found"
		exit 1

	fi

	zip -d $resultsfile \*-src.png diff-\*.png logfile.txt

}

RecordTest () {

	set -x
	# Record a test named $1 using simulator directory $2

	testname=$1
	results_dir=$2

	shift 2

	rm -f $results_dir/*.pdb $results_dir/*.txt $results_dir/*.png $results_dir/*.raw $results_dir/TESTCOMPLETE

	if [ ! -z "$PDB_OPTS" ];then

		cp $PDB_OPTS $results_dir

	fi

	LoadDatabases $results_dir

	RunSimulator
	wait

	UnloadDatabases

	mv $results_dir/testscript.txt.record $results_dir/testscript.txt
	ProcessRawImageFiles $results_dir

	if [ "$EDIT" == "yes" ];then

		vi $results_dir/testscript.txt
		rm $results_dir/testscript.txt~

	fi

	vi $results_dir/INFO.txt

	zip -j $testname.zip $results_dir/*

}

RunTest() {

	# Run test named in file $1 using simulator directory $2
	
	testname=$1
	run_dir=$2

	rm -f $run_dir/*.txt $run_dir/*.png $run_dir/*.raw $run_dir/*.pdb $run_dir/TESTCOMPLETE
	unzip -o -q -d $run_dir $testname.zip

	if [ ! -z "$PDB_OPTS" ];then

		cp $PDB_OPTS $run_dir

	fi

	cd $run_dir

	for image in $run_dir/*.png;do 
		
		mv $image $image-src.png

	done

	LoadDatabases $run_dir
	RunSimulator
	echo $SIM_PID

	echo -n "Waiting for test to complete..."
	count=0
	while [ ! -f TESTCOMPLETE -a $count -lt $MAX_TEST_TIME ]; do

		sleep 5;
		count=`expr $count + 5`

	done

	if [ -f TESTCOMPLETE ];then

		echo "completed."
		rm TESTCOMPLETE

		if [ -f FAILED-00.raw ];then

			PASSED=0

		else

			PASSED=1

		fi

	else 

		echo "timed out."
		PASSED=0
		echo "FAILED: Timed out"

	fi

	kill $SIM_PID
	UnloadDatabases

	ProcessRawImageFiles $run_dir

	# check each source image against the test run, and if differences
	# are found produce a comparison image (and fail the test)

	for srcf in *src.png;do

		result=`basename $srcf -src.png`
		if [ -f $result ];then

			if ! cmp $srcf $result;then

				compare $srcf $result diff-$result.png
				PASSED=0

			fi

		else

			echo "FAILED: $result is missing"
			PASSED=0

		fi

	done

	if [ $PASSED -eq 1 ];then

		result_dir=$TEST_HOME/passed
		echo "TEST PASSED"

	else

		result_dir=$TEST_HOME/failed
		echo "TEST FAILED"
		montage diff-* -geometry +2+2 diff-montage.png

	fi

	[ -d "$result_dir" ] || mkdir "$result_dir"

	zipname=$testname-`date +%Y%m%d-%H%M`.zip
	zip -q -j "$result_dir"/$zipname $run_dir/*

}

if [ $# -eq 0 ];then

	usage

fi

EDIT=no

shopt -s nullglob

while getopts ":rxp:el:f:a:" option;do

	case $option in

		f)	BINARY=$OPTARG;;
		r)	mode=record;;
		x)	mode=run;;
		p)  PDB_OPTS=$OPTARG;;
		l)  LAUNCH_OPTS=$OPTARG;;
		e)  EDIT=yes;;
		a)  AcceptResults $OPTARG
			exit
			;;
		:)  echo "Missing option to -$OPTARG"
			exit 1
			;;
		\?) usage;;
		*)	usage;;

	esac

done

if ! CheckBinary ;then

	echo "Test framework not compiled in, aborting"
	exit 1

fi

shift `expr $OPTIND - 1`
testfile=$1
TEST_HOME=`pwd`

if [ -z "$testfile" ];then

	echo "Must specify a test"
	exit 1

fi

SIM_BASE_DIR=`launch-sim.sh $LAUNCH_OPTS -q`
SIM_CARD_DIR=$SIM_BASE_DIR/Card1/FMTest
SIM_DB_LOAD_DIR=$SIM_BASE_DIR/AutoLoad
if [ -z "$SIM_BASE_DIR" ];then

	echo "Can't locate launch-sim.sh script in PATH"
	exit 1

fi
if [ ! -d $SIM_CARD_DIR ];then

	echo "No simulator test directory - $SIM_CARD_DIR";
	exit 1

fi


case $mode in

	"record")
		RecordTest $testfile $SIM_CARD_DIR
		;;

	"run")
		if [ ! -f $testfile.zip ];then

			echo "$testfile.zip not found"
			exit 1

		fi

		RunTest $testfile $SIM_CARD_DIR
		;;

	*)
		echo "Must specify a mode (-x or -r)"
		exit 1
esac

