/*
 * MapIcon.h
 *
 * ADT for a map icon
 *
 */

#ifndef MAPICON_H_INCLUDED
#define MAPICON_H_INCLUDED

#include "Platform.h"
#include "GlobalTypes.h"
#include "CpInterface.h"

/*
 * MapIconType
 *
 */

#define MAX_ICON_CHARS 10

typedef struct {

	/*
	 * the icon's x & y offsets from the reference position
	 *
	 */

	Coord x;
	Coord y;

	/*
	 * type of the waypoint. If this is wpRouteMark then it's an icon
	 * associated with a route waypoint. In this case 'id' is the waypoint
	 * number from the flight plan.
	 * 
	 *
	 */

	WaypointClassType type;

	/*
	 * ID of the source waypoint
	 * 
	 * Can also be the waypoint number in a flight plan, if the icon is
	 * being used to represent a route icon.
	 *
	 */

	WaypointIDType id; 

	/*
	 * which type of icon to draw
	 *
	 */

	Int16 icon;

	/*
	 * if icon is an obstacle, the altitude of it is stored here (feet)
	 *
	 */

	Int32 obstacleAltitude;
	
	/*
	 * label to accompany the icon
	 *
	 * Max is 6 chars, as it is extracted from the index
	 * which stores a maximum of 5 characters. Besides,
	 * longer than this starts to look messy on the screen
	 *
	 */

	char label[6];

} MapIconType;

/*
 * selected icons have extra information 
 *
 */

typedef struct {
	
	Int16  magVar;
	UInt16 numRunways;
	CpRunwayInfoType *runways;

} ExtraAirfieldInfoType;

typedef struct {

	char *freq;
	UInt16 freqLen;

} ExtraNavaidInfoType;

typedef union {

	ExtraAirfieldInfoType airfield;
	ExtraNavaidInfoType   navaid;

} ExtraIconInfoType;

typedef struct {

	WaypointIDType    id;
	MapIconType       icon;
	ExtraIconInfoType info;

	Int32 pixelScale;

	double lat, lon;
	const Waypoint   *ptr;

} MapIconSelectedType;


/*****************************************************************************
 *
 * public functions
 * 
 */

/*
 * function : MapIconInit
 *
 */
	
extern void MapIconInit(UInt16 iconsBitmapId, UInt16 maskBitmapId) MAPICON_SECTION;

/*
 * function : MapIconSetScale
 *
 * Sets up the scaling divisor for the map icons, used for
 * drawing runways and ATZs
 *
 * scale = pixels / NM * 256
 *
 */

extern void MapIconSetScale(Int32 scale, Coord atz) MAPICON_SECTION;

/*
 * function : MapIconNew
 *
 * Creates a new MapIconType in *icon using the given waypoint information
 * as the source.
 *
 */

extern void MapIconNew(MapIconType *icon, Coord x, Coord y, const ShortWaypointType *wp) MAPICON_SECTION;


/*
 * function : MapIconDraw
 *
 * Draws the icon at the given location, with or without a label
 *
 */

extern void MapIconDraw(const IconWindowsType *iconSet, MapIconType *icon, Boolean label, Coord x, Coord y, Int16 rotation) MAPICON_SECTION;

/*
 * function : MapIconDrawSelected
 *
 * Draws the selected Icon at the specified position. rotation is used for runway
 * orientation
 *
 */

extern void MapIconDrawSelected(const IconWindowsType *iconSet, MapIconType *icon, Coord ax, Coord ay, Coord rotation) MAPICON_SECTION;

#endif
