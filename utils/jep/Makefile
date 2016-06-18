#

CFLAGS=-mno-cygwin

all: airspace waypoint

PDBManager.o: PDBManager.c PDBManager.h

airspace: airspace.o PDBManager.o PDBManager.h
	$(CC) $(CFLAGS) -o airspace airspace.o PDBManager.o

waypoint: waypoint.o PDBManager.o PDBManager.h
	$(CC) $(CFLAGS) -o waypoint waypoint.o PDBManager.o

deploy: all
	-rm fmjep.zip
	zip -j fmjep.zip deploy/*
	zip -j fmjep.zip airspace.exe waypoint.exe arinc-fm.pl jeppesen.psml

clean:
	-rm airspace.exe airspace.o waypoint.exe waypoint.o PDBManager.o
