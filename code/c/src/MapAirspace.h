/*
 * MapAirspace.h
 *
 */

#ifndef MAP_AIRSPACE_H_INCLUDED
#define MAP_AIRSPACE_H_INCLUDED

#include "Platform.h"
#include "AsDatabase.h"

#define MAX_ALT_CHARS 10
#define MAX_AIRWAY_CHARS 6

typedef struct {

	AirspaceIDType id;

	AirspaceClassType type;

	PFScreenRectType bounds;

	/*
	 * holds null-terminated strings, for labelling
	 *
	 */

	char  lowerAltString[MAX_ALT_CHARS];
	char  upperAltString[MAX_ALT_CHARS];

	char  airwayIdent[MAX_AIRWAY_CHARS];

	/*
	 * for display-filtering purposes, based on altitude limits
	 * in source AirspaceType record
	 *
	 * (in feet)
	 *
	 */

	Int32 lowerAlt;
	Int32 upperAlt;

	Int16 numLines;
	LineType *lines;

	/*
	 * clipCount controls whether we draw the airspace or not.
	 *
	 * If it's zero, we try to draw the airspace. If not, we decrement 
	 * clipCount and wait
	 *
	 */

	Int8 clipCount;
	
	Boolean visible;

	PointType labelHint;

} MapAirspaceType;


typedef struct {

	AirspaceIDType id;
	
	MapAirspaceType mapAirspace;

	const AirspaceType *ptr;

} MapAirspaceSelectedType;

/*******************************************************************************
 *
 * functions
 *
 */

/*
 * function : MapAirspaceInit
 *
 * 
 */

extern void MapAirspaceInit(Int32 latReference, Int32 lonReference,
		Int32 latScaling, Int32 lonScaling,
		Coord clipLimit) ADT_SECTION;

/*
 * function : MapAirspaceCreate
 *
 * Creates a new MapAirspaceType, returns true if some of the bounds are within
 * clipping region.
 *
 */

extern Boolean MapAirspaceCreate(const AirspaceType *a, MapAirspaceType *mas, AirspaceIDType id, 
		UInt16 altUnits) ADT_SECTION;

/*
 * function : MapAirspaceDraw
 *
 * Draws the airspace. Uses transformation function supplied.
 *
 * Returns false if the the airspace was clipped because it is below
 * the bottom of the screen
 *
 */

extern Boolean MapAirspaceDraw(MapAirspaceType *mas, Boolean border, Boolean thick, Boolean highlight, Boolean label, 
		Boolean (*tf)(Coord, Coord, Coord*, Coord*)) ADT_SECTION;

#endif
