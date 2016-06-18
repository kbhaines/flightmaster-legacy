#!/usr/bin/perl -w
#
# dafif processing script
# 
# accepts name of dafift.zip file as single parameter, and produces
# a single file, airdb.fma, containing FlightMaster airspace records
#
# airdb.fma is then ready for processing using the fma-filter.pl script
#

use Cwd;

sub EncodeAltitude {

	my $alt = $_[0];

	if ( $alt =~ m/FL[0-9]*/ ) {

		$alt="F".substr($alt,2);

	} elsif ($alt =~ m/\d+AMSL/) {

		$alt =~ s/AMSL//;
		$alt = "A".$alt;

	} elsif ($alt =~ m/\d+AGL/) {

		$alt =~ s/AGL//;
		$alt = "G".$alt;

	} elsif ( $alt eq "SFC" || $alt eq "SURFACE" || $alt eq "GND") {

		$alt="G0";

	} elsif ( $alt eq "UNLTD" || $alt eq "U" ) {

		$alt="F9999";

	} else {

		$alt="A".$alt;

	}

	return $alt;

}


sub DecodeAltitude {

	my $alt = $_[0];

	if ($alt =~ m/FL[0-9]*/ ) {

		$alt = substr($alt,2) * 100;

	}

	return $alt;

}



sub AirwayRecords {

	$airway= $_[0]."ats/ats.txt";
	open(AIRWAY,$airway) || die "Can't open $airway";

	$airwayIdent = "";
	$count = 0;
	$segCount = 0;

	$skip=<AIRWAY>;

	while (<AIRWAY>) {

		#
		# interesting DAFIF fields are:
		#
		#  0 		- airway ident
		#  7 		- level (L=Low, H=High, B=Both)
		#  11 		- waypoint 1 ident
		#  18/20 	- waypoint 1 lat/lon
		#
		#  23		- waypoint 2 ident
		#  30/32	- waypoint 2 lat/lon
		#
		#  37/38   	- high/low altitude limits
		#
		
		@line = split(/\t/);

		$ident = $line[0];
		$level = $line[7];

		$wpt1Ident = $line[11];
		$wpt1Lat   = $line[18];
		$wpt1Lon   = $line[20];
		
		$wpt2Ident = $line[23];
		$wpt2Lat   = $line[30];
		$wpt2Lon   = $line[32];

		$high     = $line[37];
		$low     = $line[38];

		$high = "FL999" if ($high eq "");
		$low = "0" if ($low eq "");

		if ($ident ne $airwayIdent) {

			#
			# start of a new airway, output the current airway
			#
			
			$lastLat = 0; $lastLon = 0;$lastLevel="Z";

			for ($j=0;$j<$segCount;$j++) {

				#
				# start a new airway record if the airway segment is
				# non-contiguous, or the level changes, or either of
				# the altitude limits change.
				#

				if ($startLat[$j] ne $lastLat || $startLon[$j] ne $lastLon || $level[$j] ne $lastLevel
					|| $lastLowAlt ne $lowAlt[$j] || $lastHighAlt ne $highAlt[$j] ) {

					if ($j > 0) {

						print OF "X\n";
						close OF;
						open(OF, ">$airwayIdent-seg$j");

					} else {

						open(OF, ">$airwayIdent-seg$j");

					}

					print OF "W$level[$j]\n$airwayIdent\n-\n50\n";
					$l = EncodeAltitude($lowAlt[$j]);
					$h = EncodeAltitude($highAlt[$j]);

					print OF "$l\n$h\n";
					print OF "L$startLat[$j] $startLon[$j]\n";

				}

				print OF "L$endLat[$j] $endLon[$j]\n";

				$lastLat = $endLat[$j];
				$lastLon = $endLon[$j];
				$lastLevel = $level[$j];
				$lastLowAlt = $lowAlt[$j];
				$lastHighAlt = $highAlt[$j];

			}
			if ($airwayIdent ne "" ) { 
			
				$count++;
				print OF "X\n";
				close (OF);


			}

			$segCount = 0;
			$airwayIdent = $ident;

		}

		#
		# check for duplicated leg; some airways are defined twice in the file;
		# once in reverse but with possibly different altitude limits
		#
		
		for ($j=0;$j<$segCount;$j++) {
		
			if ($idents[$j] eq $wpt2Ident.$wpt1Ident) { 

				last;

			}

		}

		if ($j != $segCount) {

			#
			# found a duplicate. check the altitude limits and adjust
			# if necessary. Then continue processing the next record
			#
			
			if (DecodeAltitude($low) < DecodeAltitude($lowAlt[$j])) {

				$lowAlt[$j] = $low;

				#print "Shifting low level of $airwayIdent\n";

			}

			if (DecodeAltitude($high) > DecodeAltitude($highAlt[$j])) {

				$highAlt[$j] = $high;
				#print "Shifting high level of $airwayIdent\n";

			}

			next;

		}

		#
		# store the leg
		#
		
		$lowAlt[$segCount] = $low;
		$highAlt[$segCount] = $high;
		$level[$segCount] = $level;
		$idents[$segCount] = $wpt1Ident.$wpt2Ident;
		$startLat[$segCount] = $wpt1Lat;
		$startLon[$segCount] = $wpt1Lon;
		$endLat[$segCount] = $wpt2Lat;
		$endLon[$segCount] = $wpt2Lon;

		$segCount++;

	}

}


sub SUASRecords {

	$parent = $_[0]."suas/suas_par.txt";
	$segment= $_[0]."suas/suas.txt";

	open(PAR,$parent) || die "Can't open $parent";
	open(SEG,$segment) || die "Can't open $segment";

	#
	# process SUAS parent records
	#
	# Fields of interest are:
	# 
	#  1 - Id
	#  3 - Type (A=Alert, D=Danger, M=MOA, P=Prohibited, R=Resticted, T=TRA, W=Warning)
	#  4 - Name
	#  10 - frequency
	#  13- upper alt
	#  14- lower alt
	#  15- Active time
	#  16- WX
	#

	$skip=<PAR>;

	while (<PAR>) {

		@line=split(/\t/);

		open (OF,">$line[0]");

		$type=$line[2];
		if ($type eq "") {

			$type="W";

		};

		print OF "S$type\n";
		print OF "$line[0]\n";
		print OF "$line[3] $line[14] $line[15]\n";
		print OF "-\n";
		if ($line[9]) { 
			@tmp = split(" ",$line[9]);
			printf OF "%3.2f\n",$tmp[0];
		} else {

			printf OF "0\n";

		}

		$upper=$line[12];
		$lower=$line[13];

		$lower=EncodeAltitude($lower);
		$upper=EncodeAltitude($upper);
		print OF "$lower\n$upper\n";

		close(OF);
	}

	close(PAR);

	#
	# process segment records
	#
	# fields of interest are:
	#
	#   1 - id
	#   7 - shape [HBG] = Line, C=Circle, [LR] = Left/Right arc
	#
	#   10,12- lat/lon of start
	#   14,16 - lat/lon of end
	#   18,20 - cente of arc/circle
	#   21    - radius of circle
	#  

	$id="";
	$segnum=0;
	$count=0;
	$skip=<SEG>;
	while (<SEG>) {

		@line=split(/\t/);
		
		#
		# check for start of new boundary
		# 

		if ($id ne $line[0]) {

			#
			# finished with open file (if opened)
			# 

			if ($count) {
				print OF "X\n";
				close(OF);
			}

			$count++;

			$id=$line[0];
			open(OF, ">>$id");

			$segnum = 0;

		}

		if ($line[6] =~ /[HBG]/) {

			print OF "L$line[9] $line[11]\n" if ($segnum == 0);
			print OF "L$line[13] $line[15]\n";

		} elsif ($line[6] eq "C") {

			print OF "C$line[17] $line[19] $line[20]\n";

		} elsif ($line[6] eq "L" || $line[6] eq "R") {

			print OF "A$line[6]$line[9] $line[11] $line[13] $line[15] $line[17] $line[19]\n";

		}

		$segnum++;

	}

	print OF "X\n";
	close(OF);

}


sub BoundaryRecords {

	$parent = $_[0]."bdry/bdry_par.txt";
	$segment= $_[0]."bdry/bdry.txt";

	open(PAR,$parent) || die "Can't open $parent";
	open(SEG,$segment) || die "Can't open $segment";

	#
	# process parent records
	#
	# Fields of interest are:
	# 
	#  1 - Id
	#  3 - name
	#  5 - description
	#  9 - frequency
	#  11- class (A-G)
	#  15- upper alt
	#  16- lower alt
	#

	$skip=<PAR>;

	while (<PAR>) {

		@line=split(/\t/);

		open (OF,">>$line[0]");

		$class=$line[10];

		if ($class eq "") {

			$class="O";

		} 

		print OF "$class\n";
		print OF "$line[2]\n";
		print OF "$line[4]\n";
		print OF "-\n";
		@tmp = split(" ",$line[8]);
		if ($tmp[0]) {
		
			printf OF "%3.2f\n",$tmp[0];

		} else {
			
			printf OF "0\n";

		}

		$lower=$line[15];
		$upper=$line[14];
		$lower=EncodeAltitude($lower);
		$upper=EncodeAltitude($upper);
		print OF "$lower\n$upper\n";

		close(OF);
	}

	close(PAR);

	#
	# process segment records
	#
	# fields of interest are:
	#
	#   1 - id
	#   6 - shape [HBG] = Line, C=Circle, [LR] = Left/Right arc
	#
	#   9,11 - lat/lon of start
	#   13,15 - lat/lon of end
	#   17,19 - cente of arc/circle
	#   20    - radius of circle
	#  

	$id="";
	$segnum=0;
	$count=0;

	$skip=<SEG>;
	while (<SEG>) {

		@line=split(/\t/);
		
		#
		# check for start of new boundary
		# 

		if ($id ne $line[0]) {

			#
			# finished with open file (if opened)
			# 

			if ($count) {
				print OF "X\n";
				close(OF);
			}

			$count++;

			$id=$line[0];
			open(OF, ">>$id");

			$segnum = 0;

		}

		if ($line[5] =~ /[HBG]/) {

			print OF "L$line[8] $line[10]\n" if ($segnum == 0);
			print OF "L$line[12] $line[14]\n";

		} elsif ($line[5] eq "C") {

			print OF "C$line[16] $line[18] $line[19]\n";

		} elsif ($line[5] eq "L" || $line[5] eq "R") {

			print OF "A$line[5]$line[8] $line[10] $line[12] $line[14] $line[16] $line[18]\n";

		}

		$segnum++;

	}

	print OF "X\n";
	close(OF);

}

if ( ! -f $ARGV[0] ) {

	die "Error: please supply name of DAFIF zip file";

}

$tmp="/tmp";

$zipout=`unzip -LL -d $tmp $ARGV[0] dafift/bdry/* dafift/ats/* dafift/suas/*`;

print $zipout;

$cwd=getcwd();
print $cwd;

mkdir "$tmp/dafift/db";
chdir "$tmp/dafift/db";

print "Boundaries...\n";
BoundaryRecords "$tmp/dafift/";

print "SUAS...\n";
SUASRecords "$tmp/dafift/";

print "Airways...\n";
AirwayRecords "$tmp/dafift/";

chdir "$cwd" or die "!!!";

$cwd=getcwd();

print "Joining database files in $cwd...\n";
open(OF,">airdb.fma");
@files=<$tmp/dafift/db/*>;
foreach $f (@files) {

	open(IF,$f);
	@lines = <IF>;
	close (IF);

	foreach (@lines) { print OF; }

}

close (OF);
print "Cleaning up...\n";
`rm -rf $tmp/dafift/`;
