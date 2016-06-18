#!/usr/bin/perl -w
#
# Bit of perl to generate a list of
# sin and cosine values
#
# 0 - 45 degrees mapped to 0-63
#
#

$numElements=256;
$PI=3.1415926535897932384626433832795;
$angle = 0;
$step  = ($PI/4) / $numElements;
$one   = 32768*4;

$sins = "";
$coss = "";

for ($j=0; $j <= $numElements; $j++) {

	$s = $one * sin($angle);
	$sins .= int($s+0.5).", ";
	$coss .= int($one*cos($angle)).", ";
	$angle += $step;
	
}

print "#define SINTABSIZE ($numElements*8)\n";
print "static const Int32 sine[SINTABSIZE/8+1] = { $sins };\nstatic const Int32 cosine[SINTABSIZE/8+1] = { $coss };\n";
