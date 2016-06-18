/*
 * MapManager
 *
 * ADT2 for a map
 *
 */

#ifndef MAPMANAGER_H_INCLUDED
#define MAPMANAGER_H_INCLUDED

#include "Platform.h"
#include "WDManager.h"
#include "AsDatabase.h"
#include "FlightPlan.h"

typedef struct MapTypeStruct *MapType;

typedef struct MapSelectionStruct *MapSelection;

typedef enum { msNone, msWaypoint, msRouteWaypoint, msAirspace, msFreePoint } MapSelectionType;


/*
 * NMUnitValue is the value of one NM in mapping units
 *
 * This is used to map from lat/lon to map coordinates as follows:
 *
 * UV (Unit Value)
 * w  (screen width in pixels)
 * s  (scale, e.g. 10nm, 20nm etc)
 *
 * SD = s * UV
 *      ------
 *        w
 *
 * SD is the "scaling divisor", which becomes the scale parameter to MapInit
 *
 */

#define NMUnitValue 198841

/*
 * pass lat=lon=MAPORIGIN to MapDraw to draw the map centred
 * on it's origin
 *
 * pass lat=lon=MAPLAST to MapDraw to draw the map at the same
 * place as last time
 *
 */

#define MAPORIGIN 0x7fffffff
#define MAPLAST   0x7ffffffe

/*****************************************************************************
 *
 * public functions
 * 
 */

/*
 * MapInit
 *
 * Initialise a new map instance.
 *
 * Lat & lon are the centre coordinates for the map. Scaling determines the
 * area that will be covered in the window, whose dimensions are in pixels
 * given in pxWidth & pxHeight
 *
 * See NMUnitValue, above, for details on how to calculate an appropriate value for
 * scaling.
 *
 * Specify waypoint and airspace types that should be included in the map.
 *
 * Returns a new instance of the map.
 *
 */
	
extern MapType MapInit(Int32 lat, Int32 lon, Int32 scaling, Coord pxWidth, Coord pxHeight,
		Boolean showRoute, Boolean showTrackLog,
		WaypointClassType wpTypes, WaypointClassType wpLabels,
		AirspaceClassType airspaceTypes, AirspaceClassType airspaceLabels,
		FlightPlanType flightPlan, Boolean previewMode,
		TerrainPaletteType terrainPalette) ADT2_SECTION;

/*
 * MapFree
 *
 * Frees memory associated with the map
 *
 */

extern void MapFree(MapType map) ADT2_SECTION;

/*
 * MapBuild
 *
 * After a map is created, it must be built using this function. Mapping an area
 * is a CPU intensive operation, this function is designed to run in time-sliced fashion
 * such that repeated calls will produce a finished map.
 *
 * Returns true when the map is finished (further calls will have no effect).
 *
 */

extern Boolean MapBuild(MapType map, UInt32 maxTicks) ADT2_SECTION;

/*
 * MapDraw
 *
 * Draws the map into the currently active screen space, whose limits are 0,0 ->
 * xlimit, ylimit (pixels), and clipped at clipLimit extra pixels beyond the
 * edge.
 *
 * lat & lon allow the map centre to be translated from it's reference position
 * (which was given in the call to MapInit). If lat & lon equal the values
 * given in MapInit then the map isn't translated.
 *
 * Rotation is given in integer-degrees where +32767 = +180 degrees
 *
 * upper & lower altitude filters are given in feet, and used to filter
 * out some of the airspaces
 *
 * upper2 & lower2 allow emphasising of airspace affecting the aircraft
 *
 * ObstacleThreshold controls the altitude above which obstacles must
 * be to draw a warning around them.
 *
 * Returns false if the map didn't draw within the allotted time
 *
 */

extern Boolean MapDraw(MapType map, Int32 maxTicks,
		Int32 lat, Int32 lon, Int16 rotation,
		Int32 acLat, Int32 acLon,
		Int32 terrainAltWarning,
		Coord xOrigin, Coord yOrigin,
		Coord xlimit, Coord ylimit, Coord clipLimit,
		Int32 lowerAltFilter, Int32 upperAltFilter,
		Int32 lowerFilter2, Int32 upperFilter2,
		Int32 obstacleThreshold) ADT2_SECTION;

/*
 * MapSelectByCoord
 *
 * Select a map element close to or at the specified location, given in screen
 * coordinates. The selection will depend upon the map as drawn in the previous
 * call to MapDraw.
 *
 * startFrom can be an existing selection, in which case the search of the map starts
 * from that item (allows multiple taps of the same position to cycle around possible
 * selections)
 * 
 * Returns NULL if selection not found there. If not NULL, caller must free the memory
 * 
 */

extern MapSelection MapSelectByCoord(MapType map, Coord x, Coord y, Coord threshold, MapSelection startFrom) ADT2_SECTION;

/*
 * MapSelectionSetWaypoint
 *
 * Select the specified waypoint, creating new selection and
 * returning pointer to it (caller must free) 
 *
 */

extern MapSelection MapSelectionNewWaypoint(WaypointIDType wpID) ADT2_SECTION;

/*
 * MapSelectionNewRouteWaypoint
 * 
 * Select the specified route waypoint, creating new selection and
 * returning pointer to it (caller must free)
 * 
 */

extern MapSelection MapSelectionNewRouteWaypoint(FlightPlanType fp, Int16 waypointNum) ADT2_SECTION;

/*
 * MapSelectionNewAirspace
 * 
 * Select the specified airspace, creating new selection and
 * returning pointer to it (caller must free)
 * 
 */

extern MapSelection MapSelectionNewAirspace(AirspaceIDType asID, Int32 lat, Int32 lon) ADT2_SECTION;

/*
 * MapSelectionNewFreePoint
 * 
 * Create a selection point which is not related to a waypoint
 * nor airspace etc.
 * 
 */

extern MapSelection MapSelectionNewFreePoint(const WorldCoords *wc) ADT2_SECTION;

/*
 * MapSelectionFree
 *
 * Dispose of the map selection, set selection to NULL
 * 
 */

extern void MapSelectionFree(MapSelection *sel) ADT2_SECTION;

/*
 * MapSelectionGetType
 * 
 */

extern MapSelectionType MapSelectionGetType(MapSelection sel) ADT2_SECTION;

#define MapSelectionIsWaypoint(sel) (MapSelectionGetType(sel) == msFreePoint || MapSelectionGetType(sel) == msWaypoint || MapSelectionGetType(sel) == msRouteWaypoint)
#define MapSelectionIsAirspace(sel) (MapSelectionGetType(sel) == msAirspace)

/*
 * MapSelectionGetCoords
 * 
 * Returns pointer to selections coordinates.
 * 
 */

extern const WorldCoords *MapSelectionGetCoords(MapSelection sel) ADT2_SECTION;

/*
 * MapSelectionGetWaypointID
 * 
 */

extern WaypointIDType MapSelectionGetWaypointID(MapSelection sel) ADT2_SECTION;

/*
 * MapSelectionGetRouteWaypoint
 * 
 */

extern Int16 MapSelectionGetRouteWaypoint(MapSelection sel) ADT2_SECTION;

/*
 * MapSelectionGetIdent
 * 
 */

extern char *MapSelectionGetIdent(MapSelection sel) ADT2_SECTION;

/*
 * MapSelectionGetMagVar
 * 
 */

extern float MapSelectionGetMagVar(MapSelection sel) ADT2_SECTION;

/*
 * MapSelectionGetAirspaceID
 * 
 */

extern AirspaceIDType MapSelectionGetAirspaceID(MapSelection sel) ADT2_SECTION;

/*
 * MapSelectionDraw
 * 
 */

extern void MapSelectionDraw(MapType map, MapSelection sel) ADT2_SECTION;


/*
 * MapGetScreenCoords
 *
 * Calculates the screen coordinates of the specified lat/lon. Use to
 * overlay extra data on top of a just-drawn map
 *
 * Uses the translation data from the last drawn map to work out the
 * coordinates.
 * 
 * Returns false if the coordinates would be clipped from the display by
 * the amount specified in the call to MapInit
 *
 */

extern Boolean MapGetScreenCoords(MapType map, Int32 lat, Int32 lon, /*@out@*/ Coord *x, /*@out@*/ Coord *y) ADT2_SECTION;

/*
 * MapGetLatLon
 *
 * Calculates the latitude/longitude of the specified screen coordinate
 *
 * Uses the translation data from the last drawn map to work out the
 * coordinates.
 *
 */

extern void MapGetLatLon(MapType map, Coord x, Coord y, Int32 *lat, Int32 *lon) ADT2_SECTION;

/*
 * MapUpdatePlan
 *
 * Tells the map that the flight plan has been changed
 *
 */

extern void MapUpdatePlan (MapType map, FlightPlanType plan) ADT2_SECTION;

#endif
