#GHCOPTS=-fwarn-name-shadowing -O2 -funbox-strict-fields
GHCOPTS=-threaded -cpp -O2 -funbox-strict-fields
GHCPROFILE=-threaded -cpp -O2 -funbox-strict-fields -prof -auto-all

COPTS=-O2

fm: tags
	ghc --make -package GLUT $(GHCOPTS) FlightMaster.hs

# Windows release code requires re-linking to LibGMP, and the distribution
# must contain the contents of the gmp directory
winfm:
	ghc --make -package GLUT -Lgmp $(GHCOPTS) FlightMaster.hs

profile:
	ghc --make -package GLUT $(GHCPROFILE) FlightMaster.hs

reader:
	ghc -threaded --make -O2 GPSReader.hs

all: fm reader windows

windows: wincomm.exe

wincomm.exe:
	gcc $(COPTS) -o wincomm.exe windows/wincomm.c

arinc:
	ghc --make $(GHCOPTS) arinc-gen.hs

run: fm
	./FlightMaster.exe -t r:/wtdata/ -g 17 -i EGDL +RTS -s 

harness: tags
	ghc --make -package GLUT $(GHCOPTS) FMHarness.hs

clean:
	rm *.o *.hi

# rules to allow 'make File.hs' to do module compilation
.PHONY: *.hs
*.hs:
	ghc $(GHCOPTS) -c $@

.PHONY: tags
tags:
	hasktags *.hs

.PHONY: docs
docs: Window.hs Navigation.hs MessageProcessor.hs
	haddock -h $^
