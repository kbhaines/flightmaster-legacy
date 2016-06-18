#
# Makefile for sdstub/start program.
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


CC = m68k-palmos-gcc 
DEBUGCFLAGS= -g -O2 -DDEBUG_BUILD -mdebug-labels
RELEASECFLAGS= -O2 -Winline

SRCDIR=.

INCLUDE= -I ../../src/ -I ../../../modules/

CFLAGS = $(INCLUDE) $(RELEASECFLAGS)

#
# makepp's equivalent of VPATH directive
#
repository $(SRCDIR) ../modules ../../src

#RCPFILE=Resources.rcp

SOURCES=Main.c
	

OBJS=$(SOURCES:%.c=%.o)

.PHONY: prc
prc:     start.prc

#
# rule to build modules with debugging; specify
# 'make DBG=<modname list>'  (don't add .o or .c to modname!!)
#
$(DBG:%=%.o): CFLAGS := $(INCLUDE) $(DEBUGCFLAGS)

#CFLAGS += -DAEROPALM
#RESFLAGS += -DAEROPALM

#
# linker also needs debugging flags
#
ifdef DBG
LINKFLAGS=$(DEBUGCFLAGS)
else
LINKFLAGS=$(RELEASECFLAGS)
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
LOG1 += Main

# dummy target to force Main.o to be rebuilt, so that
# the build-timestamp is updated

dummy := $(shell rm Main.o)

endif

$(LOG1:%=%.o): CFLAGS += -DLOGGING

DEFFILE=sdstub.def

start: $(OBJS) 
	$(CC) $(LINKFLAGS) -o $@ $(OBJS)

bin.stamp: Resources.rcp
	pilrc $(RESFLAGS) $<
	touch bin.stamp

start.prc: start sdstub.def $(DEFFILE) bin.stamp
	build-prc $(DEFFILE) start -o start.prc *.bin

# 
# target for updating resources after resource files change
#
#bin.stamp: $(RCPFILE) ResourceDefines.h filter-bin.stamp
	#pilrc $(RESFLAGS) -I src/ $<
	#touch bin.stamp


.PHONY: clean

clean:
	rm -f $(OBJS) start start.prc *.bin bin.stamp filter-bin.stamp
