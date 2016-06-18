./fma-filter.pl airdb.fma A-GSW 75:-170:22.5:-50 > na.fma
./fma-filter.pl airdb.fma A-GSW 73:-20:35:40 > eu.fma
./fma-filter.pl airdb.fma A-GSW -10:110:-60:180 > ausnz.fma
./fma-filter.pl airdb.fma A-GS 90:-180:-90:180 > world.fma

echo "Use Zire 72 emulator to create PDB"
echo "Press a key when ready"

read

EMPATH=/cygdrive/c/Program\ Files/Zire\ 72\ Simulator/Card1/

for x in na eu world ;do 
	
	cp $x.fma $EMPATH/PALM/FM/airspace.fma
	echo "Make DB in FM, then save to card from Launcher and press a key"
	read

	cp $EMPATH/PALM/Launcher/FlightMaster7-Airspace.pdb $xFlightMaster7-Airspace.pdb

done
