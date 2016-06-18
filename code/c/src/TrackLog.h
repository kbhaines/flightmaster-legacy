/*
 * TrackLog.h
 *
 * (c) Blackhawk Systems 2005
 *
 * Track logging will log GPS position data to internal memory using
 * records of TLInternalRecordType, and to a file on the SD card if
 * possible
 *
 */

#ifndef TRACKLOG_H_INCLUDED
#define TRACKLOG_H_INCLUDED

#include "Platform.h"
#include "Gps.h"

typedef struct {

	Int32 lat;
	Int32 lon;

} TLInternalRecordType;

/*
 * TLRefType is a simple type that points into the tracklog for retrieval
 * purposes; if two TLRefType variables are equal then they are pointing to the
 * same TL record
 *
 */

typedef Int16 TLRefType;

/*
 * TLOpen
 *
 * open internal & external (if possible) track log for reading/writing
 *
 */

extern void TLOpen(void) ADT2_SECTION;

/*
 * TLClose
 *
 * Well, duh!
 *
 */

extern void TLClose(void) ADT2_SECTION;

/*
 * TLAddPoint
 *
 * adds a point to the track log
 *
 */

extern void TLAddPoint(GPSPosnData *gps) ADT2_SECTION;

/*
 * TLClear
 *
 * Clears the track log
 *
 * The external track log file is emptied and re-initialised with headers
 *
 *
 */

extern void TLClear(void) ADT2_SECTION;

/*
 * TLInitGetPoint
 *
 * Initialises log retrieval.
 *
 * Returns a reference to pass to TLGetPoint.
 *
 */

extern TLRefType TLInitGetPoint(void) ADT2_SECTION;

/*
 * TLGetPoint
 *
 * Get the next point from the *internal* track log. 
 *
 * When result->lat = 0 & result->lon = 0 this indicates a break-point in the
 * log, i.e. a point where the track-log is broken in two and one shouldn't
 * draw a line between the two points.
 *
 * Retrieval runs in reverse, the last point stored is recovered *first*
 *
 * *ref is updated by a call to the function
 *
 * Returns false if last point has already been retrieved; in this case result
 * is invalid.
 *
 */

extern Boolean TLGetPoint(TLRefType *ref, TLInternalRecordType *result) ADT2_SECTION;

/*
 * TLGetForwardPoint
 *
 * Get the next point from the internal track log, but advance the
 * reference *forwards*
 *
 * *ref is updated by calling this function
 *
 * Returns false if there are no more points to recover
 * 
 */

extern Boolean TLGetForwardPoint(TLRefType *ref, TLInternalRecordType *result) ADT2_SECTION;

/*
 * TLGetNumNewEntries
 *
 * Returns number of new entries added since *ref
 *
 */

extern Int16 TLGetNumNewEntries(TLRefType ref) ADT2_SECTION;

/*
 * TLWriteBreak
 *
 * Writes a tracklog break point into the log
 *
 */

extern void TLWriteBreak() ADT2_SECTION;

#endif
