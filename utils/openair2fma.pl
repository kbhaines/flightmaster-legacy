#!/usr/bin/perl -w
#
# openair2fma.pl
#
# Convert from the OpenAir format into FlightMaster Airspace format
#
# (c) 2007 Blackhawk Systems Limited
#
# Permission is granted to copy, modify and re-distribute this source code but
# this message and the copyright notice must be left intact.
#

sub dms2decimal {

	$dms = $_[0];

	if ( $dms =~ /(\d+):(\d+\.\d+)\s*([NSWE])/ ) {

		$val = $1+$2/60;
		$val = -$val if ($3 =~ /[SW]/);

	} elsif ( $dms =~ /(\d+):(\d+):(\d+)\s*([NSWE])/ ) {

		$val = $1+$2/60+$3/3600;
		$val = -$val if ($4 =~ /[SW]/);

	} else { 
	
		die ("Error in format");

	}

	return sprintf("%.4f",$val);

}


sub latlon2decimal {

	$_= $_[0];
 	my ($lat, $lon) = /(\d+:\d+[:.]\d+\s*[NS])\s+(\d+:\d+[:.]\d+\s*[WE])/;

	$latdec=dms2decimal($lat);
	$londec=dms2decimal($lon);

	return ($latdec, $londec);

}
	
if ( @ARGV != 1 ) {

	print "Usage: openair2fma.pl <file>\n";
	print "(Outputs to stdout)\n";

	exit;

}

$file=$ARGV[0];

if ( ! -f $file) {

	print "Error: $file not found\n";
	die;

}

open(FH, $file);

$count=0;
while (<FH>) {

	next if ( /^\*/ );

	if ( /^AC (\w*)/ ) {

		$arcdir = "R";

		$class=$1;
		print "X\n" if ($count >0);
		
		$class="SR" if ($class eq "R");
		$class="SD" if ($class eq "Q");
		$class="SP" if ($class eq "P");
		$class="D" if ($class eq "CTR" || $class eq "CTA");
		$class="O" if ($class eq "W" || $class eq "GP");

		print "$class\n";

	} elsif ( /^AN (.*)/ ) {

		print "$1\n-\n000.00\n";

	} elsif ( /^A[LH] (\w+)/ ) {

		$alt=$1;
		if ( $alt =~ /FL(\d+)/ ) {

			print "F$1\n";

		} elsif ( $alt =~ /(\d+)ALT/ ) {

			print "A$1\n";

		} elsif ( $alt eq "SFC" ) {

			print "G0\n";

		}

	} elsif ( /^V ([DXW])=(.*)/ ) {
	
		if ($1 eq "X") {

			($clat, $clon)  = latlon2decimal($2);

		} elsif ($1 eq "D") {

			$d=$2;
			$arcdir = "L" if ($d =~ /-/);
			$arcdir = "R" if ($d =~ /\+/);

		}

	} elsif ( /^DP (.*)/ ) {

		($lat,$lon) = latlon2decimal($1);
		print "L$lat $lon\n";

	} elsif ( /^DB (.*),\s*([^*]*)/ ) {

		$start = $1;$end = $2;

		($lat, $lon) = latlon2decimal($start);
		print "A$arcdir$lat $lon ";
		($lat, $lon) = latlon2decimal($end);
		print "$lat $lon $clat $clon\n";

	} elsif ( /^DC (.*)/ ) {

		print "C$clat $clon $1\n";

	}

	$count++;
}

print "X\n";
