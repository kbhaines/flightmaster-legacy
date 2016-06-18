#!/usr/bin/perl -w
#
# script to convert UK CAA Obstacle database to CSV for CoPilot
# waypoint generator
#
#
# Example of lines from source file:
# 
# (M/S = Multiple/Single)
#
# Desig.   			Type M/S Lat/Lon    	AMSL AGL Lit
# ======================================================
#
# Norwick (EGPM2169) RTM S 604900N 0004654W 415 323 No
# Sullum Voe (EGPM2096) FLR S 602740N 0011522W 488 336 Yes
# Spur Ness (150B706) TURB S *591136N 0024141W 358 328 No
#
# There are a couple of ways to obtain this from the CAA source
# document. Easiest is to use Acrobat to select all the text from the
# source, paste it into VI and then:
#
# :g!/\(Yes\|No\)$/d
#
# to remove the non-obstacle lines from the data
#
# Another way would be a pdf2text convertor, but I haven't tried
# this yet.
#


%type = (

	BRDG => "Bridges (including Suspension)",
	BLDG => "Buildings - General",
	CHCH => "Cathedral/Church (spire/steeple)",
	CHIM => "Smoke Stack or Chimney",
	COOL => "Cooling Tower",
	CRN => "Cranes",
	FLR => "Gas Flare",
	MINE => "Mining Structures",
	OIL => "Oil Refinery & Assoc Plant",
	PLT => "Misc Industrial Plant Buildings",
	PYL => "Powerline Pylons & Aerial Cable-ways",
	RTM => "Radio/TV Towers & Masts (antenna)",
	TURB => "Wind Farm/Turbine Blades",
	WASTE => "Waste Pile"

);

$count=0;

while (<>) {

	chomp;

	# remove () stuff, add a comma separator for next phase

	s/ \(.*\) /,/;

	# move type & single/multiple flags to end
	
	s/(.*),([A-Z]+) ([A-Z]) (.*)(Yes|No)$/$1,$4,$3:::$5,$2/;

	# split Lat & Lon, and altitude information
	
	s/(\d\d)(\d\d)(\d\d)(N|S) /$1,$2,$3,$4,/;
	s/(\d\d\d)(\d\d)(\d\d)(W|E) (\d+) (\d+) /$1,$2,$3,$4,$5,$6/;

	# single/multi and light status
	s/S:::/Type: OBSTACLE:::/;
	s/M:::/Type: OBSTACLES:::/;
	s/:::Yes/-L/;
	s/:::No//;

	@o = split(",");

	printf ("Z%03d,",$count);
	for ($j = 0;$j < 9; $j++) {
		print "$o[$j],";
	}

	print "4,W,",$o[9]-$o[10],",\"$o[11]\n\nAlt: $o[9]\n\n$type{$o[12]}\n\"\n";

	$count++;
}

