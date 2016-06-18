#!/usr/bin/perl -w
#
# Processes a named .fma file looking for airspace records of a specified type
# and within the defined bounding rectangle(s)
#
# e.g. fma-filter.pl airdb.fma A-GSW 51:-2:50:-1
#
# would produce FMA file with class A-G, SUAS and Airway (W) records within 51N,2W:50N1W
#
# sends results to standard output
#

die "Parameters: <filename> <type> <lat1>:<lon1>:<lat2>:<lon2>" if ( @ARGV < 3 );

open(IF, $ARGV[0]) || die "File not found";
	
shift;
$type=$ARGV[0];

shift;
$coords = 0;
while (@ARGV) {

	($lat1[$coords], $lon1[$coords], $lat2[$coords], $lon2[$coords]) = split(":", $ARGV[0]);

	$coords ++;

	shift;
}

FILE: while (<IF>) {

	$count = 0;
	@lines = ();

	while ( $_ ne "X\n") {
	
		$lines[$count++] = $_;
		$_ = <IF>;
		
	}
	
	next FILE if ( $lines[0] !~ /[$type]/);

	foreach (@lines) {

		if ( m/^[LCA][RL]?(-?\d*\.\d*) (-?\d*\.\d*)/) {

			$lat = $1;
			$lon = $2;

			for ($j=0; $j<$coords; $j++) {

				if ($lat <= $lat1[$j] && $lat >= $lat2[$j] && $lon >= $lon1[$j] && $lon <= $lon2[$j]) {

					foreach (@lines) { print; }
					print "X\n";
					next FILE;

				}

			}

		}
	
	}

}
