/*
 * Instruments.h
 *
 * (c)2005 Blackhawk Systems
 *
 */

#ifndef INSTRUMENTS_H_INCLUDED
#define INSTRUMENTS_H_INCLUDED

#include "Platform.h"
#include "Constants.h"

typedef struct VSITypeStruct *VSIType;

typedef struct GSITypeStruct *GSIType;

typedef struct SatConstTypeStruct *SatConstType;

/*
 * VSINew
 *
 * Create a new instance of a VSI
 *
 */

extern VSIType VSINew(Coord x, Coord y, Coord width, Coord height, 
		Boolean leftHanded, Int32 maxDeflection) ADT_SECTION;

extern void VSIFree(VSIType vsi) ADT_SECTION;

/*
 * VSIDraw
 *
 * Draw the VSI at the specified location
 *
 */

extern void VSIDraw(VSIType vsi, Int32 vs, Int32 requiredVS) ADT_SECTION;

/*
 * GSINew
 *
 * Create a new instance of a Glideslope Indicator (GSI)
 *
 */

extern GSIType GSINew(Coord x, Coord y, Coord width, Coord height, Int32 maxDeflection, Int32 tickInterval) ADT_SECTION;

extern void GSIFree(GSIType gsi) ADT_SECTION;

/*
 * GSIDraw
 *
 * Draw the GSI at the specified location
 *
 */

extern void GSIDraw(GSIType vsi, Int32 altError) ADT_SECTION;

/*
 * SatConstNew
 *
 * Creates a new instance of a satellite constellation
 *
 */

extern SatConstType SatConstNew(GPSType *gps, Coord x, Coord y, Coord w, Coord h) ADT_SECTION;
extern void SatConstFree(SatConstType sc) ADT_SECTION;

extern void SatConstFree(SatConstType sat) ADT_SECTION;

extern void SatConstFree(SatConstType sat) ADT_SECTION;

/*
 * SatConstDraw
 *
 * Draws the specified satellite constellation
 *
 */

extern void SatConstDraw(SatConstType sc) ADT_SECTION;

/*
 * FlightDirectorDraw
 *
 * Draws a flight director centred at the specified location. Space is
 * the offset from that centre location to draw. This allows the FD to 
 * be displayed left or right of an existing display e.g a heading value:
 *
 *         126 >>> --- FD (right indication)
 *			|
 *			Heading
 */

extern void FlightDirectorDraw(Coord centreX, Coord centreY, Coord space) ADT_SECTION;

#endif
