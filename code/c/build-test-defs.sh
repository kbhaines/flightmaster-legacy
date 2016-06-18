#!/bin/sh

awk '
BEGIN { print "struct { const char *token; UInt16 resID; } \
	resourceLookupTable[] = {" }
/#define/ {print "{\""$2"\",", $3,"}," }
END { print "{ NULL, 0} };" }\
' src/ResourceDefines.h > ResourceLookupTable.c
