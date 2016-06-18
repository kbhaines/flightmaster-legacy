#
# Makefile for FlightMaster.
#
# Compatible with makepp, not GNU make, some features will need to be changed
# to work with GNU make again:
#
# - change repository statements (to VPATH)
#   
# - .PHONY targets not necessary
#   
# - will need to add explicit dependency checking for c files, makepp takes
#   care of this auto-magically (yippee!!). 
#
#   NOTE::Makesubs.pm (part of makepp) needed patching to add m68k-palmos-gcc
#   as a recognised c compiler.
#   

#PACE=1
#LOG=MapManager
#DBG=TimerForm
#TEST_FRAMEWORK=1

CC=/usr/bin/m68k-palmos-gcc -Werror-implicit-function-declaration
PILRC=/usr/bin/pilrc
TOUCH=touch
SRCDIR=src
INCLUDE= -I src/ -I extsrc/ -I ./

DEBUGCFLAGS= $(INCLUDE) -g -O2 -DDEBUG_BUILD -mdebug-labels
#RELEASECFLAGS= -O2 -Winline -Wmissing-declarations -Wall
RELEASECFLAGS= -O2 -Winline -Wmissing-declarations

CFLAGS = $(INCLUDE) $(RELEASECFLAGS)

#
# makepp's equivalent of VPATH directive
#
repository $(SRCDIR) extsrc/

RCPFILE=Resources.rcp

SOURCES=Main.c\
	Platform.c\
	Logging.c\
	Utils.c\
	MainForm.c\
	MapForm.c\
	GPSForm.c\
	FlightPlanForm.c\
	DiversionForm.c\
	MapDialog.c\
	PreferencesDialog.c\
	MorePreferencesDialog.c\
	SelectCopilotFlightDialog.c\
	KeypadDialog.c\
	AlphaPadDialog.c\
	CommandDialog.c\
	WPInfoDialog.c\
	TextDialog.c\
	SimulationDialog.c\
	MessageDialog.c\
	VNAVDialog.c\
	HeadingIndicator.c\
	Gps.c\
	MathLib.c\
	CpInterface.c\
	Upgrades.c\
	DiversionMgr.c\
	AvCalcs.c\
	AIGauge.c\
	Polygon.c\
	CollapseUtils.c\
	AsDatabase.c\
	FlightPlan.c\
	FlightDatabase.c\
	OBSManager.c\
	MapIcon.c\
	MapAirspace.c\
	MapManager.c\
	Colours.c\
	WDManager.c\
	NavManager.c\
	AlarmManager.c\
	Instruments.c\
	TrackLog.c\
	MonitorDialog.c\
	TerrainType.c\
	TimerForm.c\
	SaveFlightDialog.c\
	FMLangStrings.c\
	FMPreferences.c\
	TapPlan.c\
	PanManager.c\
	GarminGPS.c\
	FlightMaster-sections.o

OBJS=$(SOURCES:%.c=%.o)

FILTERICONS=filter0-off.bmp filter0-on.bmp filter0-onl.bmp

.PHONY: all
all: prc

.PHONY: test
test: TEST_FRAMEWORK=1
test: all

.PHONY: prc
prc:     FlightMaster.prc
	@if grep -q TEST_FRAMEWORK $<;then echo "**** WARNING: BUILT WITH TEST FRAMEWORK ****";fi

ifdef PRF
DEBUGCFLAGS += -DPROFILE
RELEASECFLAGS += -DPROFILE
endif

ifdef TEST_FRAMEWORK
Platform.o:DEBUGCFLAGS += -DTEST_FRAMEWORK
Platform.o:RELEASECFLAGS += -DTEST_FRAMEWORK
endif

ifdef PACE
TerrainType.o:DEBUGCFLAGS += -DTARG_PACE
TerrainType.o:RELEASECFLAGS += -DTARG_PACE
endif

#
# rule to build modules with debugging; specify
# 'make DBG=<modname list>'  (don't add .o or .c to modname!!)
#
$(DBG:%=%.o): CFLAGS := $(DEBUGCFLAGS)

#
# linker also needs debugging flags
#
ifdef DBG
LINKFLAGS=$(DEBUGCFLAGS)
else
LINKFLAGS=$(RELEASECFLAGS)
endif

ifdef AP
CFLAGS += -DAEROPALM
RESFLAGS += -D AEROPALM
endif

#LINKFLAGS += -Wl,--print-map

#
# rule to build modules with trace; specify
# 'make TRACE="<modname list>"' (no .o or .c on modname!!)
# 
$(TRACE:%=%.o): CFLAGS += -DTRACETOOLS

#
# rule to build modules with logging; specify
# 'make LOG="<modname list>"' (no .o or .c on modname!!)
# 

LOG1=$(LOG)
ifdef LOG
LOG1 += Platform

# dummy target to force Platform.o to be rebuilt, so that
# the build-timestamp is updated

dummy := $(shell rm -f Platform.o)

endif

$(LOG1:%=%.o): CFLAGS += -DLOGGING

#
# rule to build unit test modules; specify
# 'make UT="<modname list>"' (no .o or .c on modname!!)
#

UT1=$(UT)
ifdef UT
UT1 += Main
endif
$(UT1:%=%.o): CFLAGS += -DUNITTEST

FlightMaster-sections.o: FlightMaster-sections.s
	$(CC) $(CFLAGS) -c FlightMaster-sections.s -o FlightMaster-sections.o

ifdef AP
DEFFILE=AeroPalm.def
else
DEFFILE=FlightMaster.def
endif

%.o: src/%.c
	$(CC) $(CFLAGS) -c $(input) -o $(output)

#
# target for generating language variations
#

LANGUAGE=English
ifdef FMLANG
LANGUAGE=$(FMLANG)
endif

FMLangStrings.c: translations.tab FMStrings.c
	build_fmstrings.sh $(LANGUAGE) translations.tab src/FMStrings.c FMLangStrings.c

FMLangStrings.o:
	$(CC) $(CFLAGS) -c FMLangStrings.c -o $(output)
	
ResourceLookupTable.c: src/ResourceDefines.h
	./build-test-defs.sh

FlightMaster-sections.s FlightMaster-sections.ld: $(DEFFILE)
	/usr/bin/m68k-palmos-multigen --base FlightMaster-sections  $(DEFFILE)

FlightMaster: $(OBJS) FlightMaster-sections.ld
	$(CC) $(LINKFLAGS) -o $@ $(OBJS) FlightMaster-sections.ld -lPalmOSGlue #-Wl,-Map=t1.map,-cref

FlightMaster.prc: FlightMaster bin.stamp $(DEFFILE)
	build-prc $(DEFFILE) FlightMaster -o FlightMaster.prc *.bin

TerrainCore.bin: src/TerrainCore.c
	arm-palmos-gcc -nostartfiles -nostdlib -fPIC -ffixed-r9 -o TerrainCore.o $<
	arm-palmos-objcopy -O binary TerrainCore.o TerrainCore.bin

$(LANGUAGE).rcp: translations.tab
	build_language_rcp.sh $(LANGUAGE) translations.tab

bin.stamp: $(RCPFILE) ResourceDefines.h TerrainCore.bin $(LANGUAGE).rcp
	$(PILRC) -L $(LANGUAGE) $(RESFLAGS) -I src/ $<
	$(TOUCH) bin.stamp

.PHONY: clean

splint:
	# -CC option to suppress comment deletion
	$(CC) -E -C $(INCLUDE) src/NavManager.c > t1.c
	splint.exe -booltype Boolean -type +charindex -boolops -retvalother -predboolothers -predboolint -nullret -retvalbool t1.c

clean:
	rm -f $(OBJS) FlightMaster FlightMaster.prc *.bin bin.stamp
	rm -f FlightMaster-sections.*
	#rm -f filter*.bmp

check:
	@echo "*******************************************************"
	@echo "****** Verify that version numbers are correct!! ******"
	@echo "******                                           ******"
	@echo "******       (Then press return to build)        ******"
	@echo "*******************************************************"
	@grep "APPLICATIONICONNAME" $(SRCDIR)/$(RCPFILE)
	@grep "TITLE \"About " $(SRCDIR)/$(RCPFILE)
	@/bin/sh -c read

release: check clean FlightMaster.prc
	# build resources & Application
	#( rm *.bin; $(PILRC) $(RESFLAGS) -I $(SRCDIR) $(SRCDIR)/$(RCPFILE))
	#build-prc FlightMaster.def FlightMaster -o FlightMaster.prc *.bin
	./release FlightMaster $(SRCDIR)/$(RCPFILE)

	
############## TESTS/UTILITIES ####################################

LCC=/usr/bin/gcc

regcodegen: RegCodeHarness.c RegistrationCode.c
	$(LCC) -o $(output) $<
