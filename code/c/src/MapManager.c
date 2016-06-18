/*
 * MapManager.c
 *
 */

#include "Platform.h"
#include "Utils.h"
#include "MapManager.h"
#include "FlightPlan.h"
#include "OBSManager.h"
#include "WDManager.h"
#include "AsDatabase.h"
#include "MapAirspace.h"
#include "MapIcon.h"
#include "Modules.h"
#include "MathLib.h"
#include "TrackLog.h"
#include "TerrainType.h"
#include "AvCalcs.h"

#include "FMPreferences.h"
#include "NavManager.h"

#define ModuleID MapManagerModuleID

//#define SHOW_TIMERS 1

#ifdef SHOW_TIMERS
#define TIMERSTART(s) { UInt32 loggingstart = PFGetTicks();char TIMER[32]; char *TIMNAME=s;
#define TIMERSTOP(y) StrPrintF(TIMER,"%s %ld", TIMNAME, (PFGetTicks()-loggingstart));FntSetFont(stdFont);PFDrawChars(TIMER, StrLen(TIMER), 0,y); }
#else
#define TIMERSTART(s)
#define TIMERSTOP(y)
#endif

/*******************************************************************************
 *
 * global variables
 * 
 */

extern const AppColourPrefsType AppColourPrefs;
extern const IconWindowsType IconWindows;
extern const IconWindowsType SmallIconWindows;
extern const DisplayUnitsType DisplayUnits;
extern const UserConversionType UC;

extern const WDMHandle WPDataset;
extern const FMDataset FMDS;
extern const FMPreferencesType Preferences;

extern TerrainType Terrain;

/*****************************************************************************
 * module variables
 *
 */

struct MapSelectionStruct {
	
	MapSelectionType type;
	
	WorldCoords position;
	float magVar;		// radians, +ve = east

	struct {

		char ident[11];

		/*
		 * id can be wpNotFound if the selection is a route icon
		 *
		 */

		WaypointIDType id;

		/*
		 * routeWaypoint is -1 if the selection is not a waypoint of the plan. Otherwise, this
		 * can be 0..N (where N is number of waypoints in the plan-1). 
		 * 
		 * id can also be set which indicates a route waypoint with a 
		 * corresponding icon on the map.
		 *
		 */

		Int16 routeWaypoint;
			
		
	} waypoint;

	struct {

		AirspaceIDType id;
//		AirspaceType *ptr;

	} airspace;

};



#define MAX_TRACKLINES 400

typedef enum { 
	
	srcRouteWaypoint, 	/* from the route waypoints */
	srcWaypoints,		/* from waypoint database */
	srcAirspace,		/* from airspace DB */
	srcTFRs,			/* from TFRs DB */
	srcTrackLog

} MapSourceType;


#define MAX_MAPICONS 400
#define MAX_AIRSPACES 350
#define MAX_MAP_ELEMENTS (MAX_MAPICONS+MAX_AIRSPACES)

struct MapTypeStruct {

	Boolean previewMode;

	Boolean showTrackLog;
	Boolean showRoute;

	Boolean complete;	// set true when the map is finished being built

	/*
	 * reference latitude and longitude of this map - elements in the
	 * map are stored relative to this point
	 *
	 */

	Int32 latRef;
	Int32 lonRef;

	/*
	 * bounding box, northwest and southeast corners 
	 *
	 */

	Int16 lat1, lon1;
	Int16 lat2, lon2;

	/*
	 * convert from Int32-based lat/lon to pixels by dividing by these 
	 * values
	 *
	 */

	Int32 latScaling, lonScaling;

	Coord fiveNMPixels;		// how many pixels represent 5nm
	Coord clipLimitX;
	Coord clipLimitY;

	/*
	 * if true, then the map should be drawn with smaller elements
	 *
	 */

	Boolean drawReduced;

	double sinlat;
	double coslat;
	
	WaypointClassType wpTypes, wpLabels;
	AirspaceClassType airspaceTypes, airspaceLabels;

	/*
	 * rotation of the map, in degrees, 0 = north
	 *
	 */

	Int16 rotation;

	/*
	 * numIcons is the total number of icons in the map 
	 * icon is where the icon data is actually stored
	 *
	 */

	UInt16 numIcons;
	MapIconType *icons;

	/*
	 * stores clipped line segments for each part of the route, and the
	 * icons for the route. Only icons for route waypoints that are
	 * within the clipping region are stored.
	 *
	 */

	Int16 numRouteLines;
	LineType *routeLines;

	Int16 numRouteIcons;
	MapIconType *routeIcons;
	
	/*
	 * airspaces are stored in 'airspace' in whatever order we encounter them
	 * as the map is built. However the index is sorted by descending area of
	 * the airspace (determined by it's bounding box), and is used during the
	 * drawing process. This makes sure we draw larger airspaces first.
	 * 
	 */

	Int16  numAirspaces;
	MapAirspaceType *airspace;
	Int16  *airspaceIndex;

	/*
	 * the map is constructed in phases, this points to the
	 * database source currently being used
	 *
	 */

	MapSourceType currentSource;

	/*
	 * used internally by the constructor processes AddAirspace
	 *
	 */

	AirspaceIDType *airIDs;

	/*
	 * last drawn position parameters
	 *
	 */

	Int32 lastDrawLat, lastDrawLon;
	Coord lastXOrigin, lastYOrigin;
	Int16 lastRotation;

	FlightPlanType flightPlan;		// reference to flight plan to show

	/*
	 * track log
	 *
	 * Stores a set of lines representing the track log
	 *
	 */

	Int16 numTrackLines;
	LineType *trackLog;	// collection of LineType
	TLRefType tlRef;		// reference to track log, for retrieval
	TLRefType tlFwdRef;		// allows forward log retrieval

	/*
	 * terrain
	 *
	 */

	TerrainType terrain;

};

/*
 * Waypoint hash
 *
 * This is a tightly packed hash representing the waypoint IDs of those
 * waypoints which are used in the flight plan. The hash is 512 bytes,
 * which allows representation of 4096 values.
 *
 * Use the lower 12 bits of the waypoint ID to index into the hash. If the bit
 * is set then it means that there is a *possibility* that the waypoint is used
 * in the flight plan - further investigation is required.
 *
 */

#define NUM_HASH_ELEMENTS 4096
#define HASH_SIZE ( NUM_HASH_ELEMENTS / 8 )
static UInt8 waypointHash[HASH_SIZE];

/*
 * translation matrix is used by the map code to translate from map coordinates
 * into screen coordinates, via a rotation if rotation != 0
 *
 */

struct {

	/*
	 * map translation, usually relative to the aircraft's
	 * coordinates
	 *
	 */
	
	Coord mapx, mapy;

	/*
	 * rotation in degrees, and sine/cosine returned by GetSinCos function
	 *
	 */
	
	Int16 rotation;
	Int32 sine, cosine;

	/*
	 * screen translation, often the aircraft's position
	 * on the screen but can also just be the centre of the screen
	 *
	 */

	Coord screenx, screeny;

} transMatrix;

/*
 * used for screen clipping
 *
 */

static Coord clipLeft, clipRight, clipTop, clipBottom;

/*****************************************************************************
 *
 * private functions
 *
 */

static void AddRoute(MapType map) ADT2_SECTION;
static void AddProxIcons(MapType map) ADT2_SECTION;
static Boolean AddAirspace(MapType map, UInt32 maxTicks) ADT2_SECTION;
static Boolean AddTrackSegment(MapType map, LineType *wl, 
		Coord x1, Coord y1, Coord x2, Coord y2) ADT2_SECTION;
static void DrawIcons(MapType map, MapIconType *icons, Int16 numIcons, Coord ax, Coord ay, 
		Int32 obstacleThreshold) ADT2_SECTION;

static Boolean AddTrackLog(MapType map, UInt32 maxTicks) ADT2_SECTION;

static void SetupWaypointHash(void) ADT2_SECTION;
/*
 * macros for manipulating the hash; each takes a waypoint ID as a parameter
 *
 */

#define SET_HASH(id) waypointHash[((id) & 0x0FFF) / 8] |= ( 1 << ((id) & 7) )
#define HASH_VALUE(id) ( waypointHash[((id) & 0x0FFF) / 8] & ( 1<< ((id) & 7) ) )

static Boolean ApplyTransform(Coord xi, Coord yi, Coord *xr, Coord *yr) ADT2_SECTION;
static void ReverseTransform(Coord xi, Coord yi, Coord *xr, Coord *yr) ADT2_SECTION;

/*
 * function : DrawIcons
 *
 */

static void DrawIcons(MapType map, MapIconType *icons, Int16 numIcons, Coord ax, Coord ay, Int32 obstacleThreshold) {

	MapIconType *icon;
	Int16 j;

	const Coord warningWidth = 18;

	PFSetTextColour(AppColourPrefs.label);

	for (j=0; j<numIcons; j++) {

		Coord x,y;
		
		icon = &icons[j];

		if (ApplyTransform(icon->x, icon->y, &x, &y)) {

			if ((icon->type & wpAnyObstacle ) 
				&& icon->obstacleAltitude > obstacleThreshold
				&& x > ax - map->fiveNMPixels && x < ax + map->fiveNMPixels
				&& y > ay - map->fiveNMPixels && y < ay + map->fiveNMPixels) {

				PFScreenRectType r;
				Coord ix = x - warningWidth;
				Coord iy = y - warningWidth;

				PFScreenRectangleSetRel(&r, ix, iy, warningWidth * 2, warningWidth * 2);

				PFSetForeColour(AppColourPrefs.warning);
				PFPaintRectangleFrame(&r, boldRoundFrame);
				
			}

			if (map->drawReduced) {

				MapIconDraw(&SmallIconWindows, icon, (map->wpLabels & icon->type) ? true:false,x , y, transMatrix.rotation);

			} else {

				MapIconDraw(&IconWindows, icon, (map->wpLabels & icon->type) ? true:false,x , y, transMatrix.rotation);
					
			}
			
		}

	}

}


/*
 * function : AddRoute
 *
 * Constructs the current route into the specified map.
 *
 * This function can be called after the route has been updated, and
 * it will safely modify the map.
 *
 */

static void AddRoute(MapType map) {

	static Int32  lastx, lasty;
	Int32  lx, ly;
	Int16  numWaypoints;
	MapIconType *icon;
	Int16 j;

	LOGENTRY;
	
	/*
	 * check if map has route already, if so we will be making a new one
	 *
	 */

	if (map->routeLines) PFMemFree(map->routeLines);
	if (map->routeIcons) PFMemFree(map->routeIcons);
	map->routeIcons = NULL;
	map->numRouteIcons = 0;
	map->numRouteLines = 0;

	if (!map->showRoute || FpIsBlank(map->flightPlan)) return;

	/*
	 * if OBS is active we'll use the OBS waypoints
	 *
	 */

	if (OBSActive()) {

		numWaypoints = 3;
		
	} else {

		numWaypoints = map->previewMode ? 
			FpGetNumLegs(map->flightPlan) + 1 : 
			(FpGetNumLegs(map->flightPlan) - FpGetCurrentLeg(map->flightPlan)) + 1;

	}

	map->routeLines = PFMalloc(numWaypoints*sizeof(LineType)); 
	ModErrThrowIf(!map->routeLines);

	map->routeIcons = PFMalloc(numWaypoints*sizeof(MapIconType));
	ModErrThrowIf(!map->routeIcons);

	icon = (map->routeIcons);

	for (j=0; j < numWaypoints; j++) {

		const FlightPlanLegWaypointType *wp = NULL;

		Int32 x, y;
		Int16 routeWaypointNum;

		if (OBSActive()) {

			/*
			 * get 'waypoints' from OBS settings
			 *
			 */

			switch(j) {

			case 0: wp = OBSGetWaypoint(0);
					break;

			case 1: wp = FpGetCurrentWaypoint(map->flightPlan);
					break;

			case 2: wp = OBSGetWaypoint(1);
					break;

			default: wp = NULL;
					 ModErrThrowIf(1);
					 break;

			}

		} else {

			/*
			 * use the flight-plan waypoints
			 *
			 */

			routeWaypointNum = map->previewMode ? j : j + FpGetCurrentLeg(map->flightPlan);

			wp = FpGetWaypoint(map->flightPlan, routeWaypointNum);

		}

		ModErrThrowIf(!wp);

		y = (wp->lat - map->latRef) / map->latScaling;
		x = (wp->lon - map->lonRef) / map->lonScaling;

		/*
		 * if the icon is beyond the edge of the map, then use great
		 * circle range & bearing from lat & lon reference point to
		 * determine icon's position.
		 *
		 * We won't actually add this icon, but we do need to add a
		 * clipped line for it.
		 *
		 */

		if (x > map->clipLimitX || x < -map->clipLimitX
			|| y > map->clipLimitY || y < -map->clipLimitY) {

			double range;
			Int32  intRange;
			double crs;
			double wplat = INT32_TO_RAD(wp->lat);
			double wplon = INT32_TO_RAD(wp->lon);
			double lat = INT32_TO_RAD(map->latRef);
			double lon = INT32_TO_RAD(map->lonRef);


#ifdef LOGLEVEL2
			LOGTAG("Rng/Brg calc");
			LOGSTR(wp->ident);

			LOGINT32(map->sinlat*1000);
			LOGINT32(map->coslat*1000);
			LOGINT32(sinlat2*1000);
			LOGINT32(coslat2*1000);
			LOGINT32(lat*1000);
			LOGINT32(lon*1000);

#endif
#ifdef FASTER_RANGE_CHECK
			range=acos(map->sinlat*sinlat2+map->coslat*coslat2*
					cos(lon-wplon));

			if (lon == wplon) {

				if (lat > wplat)
					crs = PI;
				else
					crs = 0;

			} else {

				crs = acos((sinlat2-map->sinlat*cos(range))/(sin(range)*map->coslat));

				if (! (sin(wplon-lon)<0))
					crs = 2*PI - crs;

			}
#endif
			
			crs = AvCalcGreatCircleCourse(lat, lon, wplat, wplon, &range);

			/*
			 * calculate range in pixels
			 *
			 */

			intRange = RAD_TO_INT32(range) / map->latScaling;

			x = (Int32) ((double)intRange * sin(crs));
			y = (Int32) ((double)intRange * cos(crs));
			
		} else {

			/*
			 * Add an icon for the route element
			 *
			 */

			ShortWaypointType swp;

			swp.type = wpRouteMark;
			swp.wpId = j + FpGetCurrentLeg(map->flightPlan);
			StrNCopy(swp.extra, wp->ident,5);
			MapIconNew(&icon[map->numRouteIcons], (Coord)x, (Coord)y, &swp);
			map->numRouteIcons++;

		}

		lx = lastx; ly = lasty;
		lastx = x; lasty = y;

		/*
		 * only add route lines after waypoint 0
		 *
		 */

		if (j > 0) {
			
			if (ClipLine32(&lx, &ly, &x, &y, -map->clipLimitX, -map->clipLimitY,
					map->clipLimitX, map->clipLimitY )) {
			
				map->routeLines[map->numRouteLines].x1 = (Coord)lx;
				map->routeLines[map->numRouteLines].y1 = (Coord)ly;
				map->routeLines[map->numRouteLines].x2 = (Coord)x;
				map->routeLines[map->numRouteLines].y2 = (Coord)y;
				map->numRouteLines++;

			} 

		}

	}

	LOGEXIT;
	
}

/*
 * function : AddProxIcons
 *
 * Adds proximity icons to the map space
 *
 */

static void AddProxIcons(MapType map) {

	Int16 w;
	MapIconType *icon;
	ShortWaypointType *wpList;

	wpList = PFMalloc(sizeof(ShortWaypointType)*MAX_MAPICONS);
	ModErrThrowIf(!wpList);

	WDMScanArea(WPDataset, FMDS.all, map->lat1, map->lon1, 
		map->lat2, map->lon2, map->wpTypes,
		wpList, &w, MAX_MAPICONS);

	if (!w) {

		PFMemFree(wpList);
		return;

	}

	map->icons = PFMalloc(sizeof(MapIconType)*w);
	ModErrThrowIf(!map->icons);

	icon = map->icons;

	while (w--) {

		ShortWaypointType *shortWp = & (wpList[w]);
		Int32 x,y;

		LOGINT16(w);

		x = (shortWp->lon - map->lonRef) / map->lonScaling;
		y = (shortWp->lat - map->latRef) / map->latScaling;

		MapIconNew(&icon[map->numIcons++], x,y, shortWp);

	}

	PFMemFree(wpList);

	return;

}

/*
 * function : AddAirspace
 *
 * Adds airspace to the map space, governed by maxTicks
 *
 * Initialise the function by calling with maxTicks = 0
 *
 * Returns true if finished all airspace records
 *
 */

static Boolean AddAirspace(MapType map, UInt32 maxTicks) {

	static Int16 w;
	static Int16 airspaceArea[MAX_AIRSPACES];

	UInt32 now = PFGetTicks();

	LOGENTRY;

	if (maxTicks == 0) {
		
		/*
		 * initialise the scanning
		 *
		 */

		map->airIDs = PFMalloc(sizeof(AirspaceIDType)*MAX_AIRSPACES);
		ModErrThrowIf(!map->airIDs);

		TIMERSTART("Air scan");
		w = AsSearchDatabase(map->lat1,map->lon1, map->lat2, map->lon2,
				map->airspaceTypes, map->airIDs, MAX_AIRSPACES);
		TIMERSTOP(40);

		if (w) {

			map->airspace = PFMalloc(sizeof(MapAirspaceType)*w);
			map->airspaceIndex = PFMalloc(sizeof(AirspaceIDType)*w);
			ModErrThrowIf(!map->airspace || !map->airspaceIndex);

		} else {

			map->airspace = NULL;
			map->airspaceIndex = NULL;

		}
			
		MapAirspaceInit(map->latRef, map->lonRef,
				map->latScaling, map->lonScaling, MAX(map->clipLimitX, map->clipLimitY));
				

		LOGEXIT;

		return false;

	}

	/*
	 * airspaces are sorted by area, largest first, so that the drawing
	 * routine will draw those first.
	 *
	 * note the importance of w-- AFTER checking the timer; the other way
	 * around causes icons & airspace to disappear!
	 *
	 */

	TIMERSTART("AddAirspace");

	while (!PFTimerHasExpired(now,maxTicks) && w--) {

		AirspaceType *as;
		MapAirspaceType *mas = &map->airspace[map->numAirspaces];

		as = AsGetAirspace(map->airIDs[w]);

		LOGINT16(map->airIDs[w]);
		LOGINT16(w);

		if (MapAirspaceCreate(as, mas, map->airIDs[w], DisplayUnits.altitude)) {

			// TODO - is airspace area sorting needed?
			
			Int16 area = (Int16) (PFGetRectangleWidth(mas->bounds)/8) * 
					(PFGetRectangleHeight(mas->bounds) / 8);

			if (map->drawReduced && area < 2) { 
				
				if (mas->lines) PFMemFree(mas->lines);
				DBRecordFree((void*)as);

				continue;

			}

			if (map->numAirspaces) {

				/*
				 * insert the new entry into the index, according to
				 * the area of the bounding box of the airspace
				 *
				 */
				
				Int16 j,k;
				for (j=0;j<map->numAirspaces;j++)
					
					if (area > airspaceArea[j]) break;

				for (k=map->numAirspaces;k>j;k--)  {
					
					airspaceArea[k] = airspaceArea[k-1];
					map->airspaceIndex[k] = map->airspaceIndex[k-1];

				}

				airspaceArea[j] = area;
				map->airspaceIndex[j] = map->numAirspaces;

			} else {

				map->airspaceIndex[0] = 0;
				airspaceArea[0] = area;

			}

			map->numAirspaces++;
			LOGINT16(map->numAirspaces);

		}

		DBRecordFree((void*)as);

	}

	TIMERSTOP(20);

	if (w<0) {

		PFMemFree(map->airIDs);
		map->airIDs = NULL;
		LOGEXIT;
		return true;

	}

	LOGEXIT;
	return false;

}

/*
 * AddTrackSegment
 *
 * Adds a single line segment to the tracklog of the specified map
 *
 * Returns false if the line was clipped or not longer than 2 pixels (and was dropped)
 *
 *
 */

Boolean AddTrackSegment(MapType map, LineType *wl, Coord x1, Coord y1, Coord x2, Coord y2) {

	static Coord lastXdiff, lastYdiff;
	Coord xdiff, ydiff;

	const Coord threshold = 6;

	/*
	 * store the line only if the clipping check succeeds and the line is
	 * greater than 3 or 6 (highdensity) pixels in length
	 *
	 */

	xdiff = x2-x1;
	ydiff = y2-y1;

	if ( (abs(xdiff) > threshold || abs(ydiff) > threshold) &&
			ClipLine16(&x1, &y1, &x2, &y2, -map->clipLimitX, -map->clipLimitY, map->clipLimitX, map->clipLimitY)) {

		/*
		 * optimisation: we can just make do with moving the endpoint of the previous
		 * line if the gradient is identical, or nearly so
		 *
		 */

		if (map->numTrackLines && lastXdiff == xdiff && lastYdiff == ydiff &&
				x1 == wl[map->numTrackLines-1].x2 && y1 == wl[map->numTrackLines-1].y2) {

			wl[map->numTrackLines-1].x2 = x2;
			wl[map->numTrackLines-1].y2 = y2;

		} else {
			
			wl[map->numTrackLines].x1 = x1;
			wl[map->numTrackLines].y1 = y1;

			wl[map->numTrackLines].x2 = x2;
			wl[map->numTrackLines].y2 = y2;

			map->numTrackLines++;

			lastXdiff = xdiff;
			lastYdiff = ydiff;

		} 

		return true;

	}

	return false;

}

/*
 * function : AddTrackLog
 *
 * Adds points from the tracklog to the specified map
 *
 */

static Boolean AddTrackLog(MapType map, UInt32 maxTicks) {

	Boolean done = false;
	static TLInternalRecordType tlr;

	UInt32 now = PFGetTicks();
	LineType *wl;

	LOGENTRY;

	if (maxTicks == 0) {

		/*
		 * initialising
		 *
		 */

		map->trackLog = PFMalloc(sizeof(LineType)*MAX_TRACKLINES);

		ModErrThrowIf(!map->trackLog);

		map->tlRef = TLInitGetPoint();
		map->tlFwdRef = map->tlRef;

		tlr.lat=0;
		tlr.lon=0;

		LOGEXIT;
		return false;

	} 

	wl = (LineType*) (map->trackLog);

	do {

		static Coord x1,y1;
		Coord x2,y2;

		if (tlr.lat == 0 && tlr.lon == 0) {

			Boolean morePoints = true;

			/*
			 * find the first point, or the start point in a new segment
			 *
			 */

			LOGTAG("Start segment");
			while (tlr.lat == 0 && tlr.lon == 0 && morePoints)
				morePoints = TLGetPoint(&map->tlRef, &tlr);
			
			if (!morePoints) {

				LOGTAG("EOL");
				done = true;
				break;

			}

			x1 = (tlr.lon - map->lonRef) / map->lonScaling;
			y1 = (tlr.lat - map->latRef) / map->latScaling;

		}

		if (TLGetPoint(&map->tlRef, &tlr)) {
			
			/*
			 * if this is a break-record (lat=lon=0) then continue on,
			 * allowing the code above to start a new line segment
			 *
			 */

			if (!tlr.lat && !tlr.lon) continue;

			x2 = (tlr.lon - map->lonRef) / map->lonScaling;
			y2 = (tlr.lat - map->latRef) / map->latScaling;

			if (AddTrackSegment(map, wl, x1, y1, x2, y2)) {

				x1 = x2;
				y1 = y2;

				done = map->numTrackLines == MAX_TRACKLINES;
				
			}

		} else { 

			done = true;
			break;

		}
		
	} while (!done && !PFTimerHasExpired(now, maxTicks));

	/*
	 * if finished then shrink the memory we allocated to the actual amount
	 * used, plus a little extra to allow for dynamic updates to occur
	 *
	 */

	if (done) {

		LOGTAG("Finished TL");

		PFMallocResize(map->trackLog, sizeof(LineType) * (map->numTrackLines+40));

		map->tlRef = map->tlFwdRef;	// tlRef becomes anchor for forward retrieval

		LOGINT16(map->numTrackLines);
		LOGEXIT;
		return true;
		
	}

	LOGINT16(map->numTrackLines);
	LOGEXIT;
	return false;

}

/*
 * function : SetupWaypointHash
 *
 * Initialises the waypoint hash from the values in the flight plan.
 *
 */

static void SetupWaypointHash(void) {

	/*
	 * empty the hash 
	 *
	 */

	PFMemSet(waypointHash, HASH_SIZE, 0);

}

/*
 * function : ApplyTransform
 *
 * Applies the transformation matrix, transMatrix, to the specified coordinate
 * (which is in the map system) and returns the resulting screen coordinate
 *
 * Returns false if the resulting coordinate is outside clipping bounds
 *
 */

static Boolean ApplyTransform(Coord xi, Coord yi, Coord *xr, Coord *yr) {
	
	Int32 xs = (Int32)xi;
	Int32 ys = (Int32)yi;
	Int32 x,y;

	xs += (Int32)transMatrix.mapx;
	ys += (Int32)transMatrix.mapy;

	if (transMatrix.rotation != 0) {

		//xs *= 8;
		//ys *= 8;

		x = xs * transMatrix.cosine + ys * transMatrix.sine;
		y = ys * transMatrix.cosine - xs * transMatrix.sine;

		//x += x < 0 ? -(COSINEZERO/2) : (COSINEZERO/2);
		//y += y < 0 ? -(COSINEZERO/2) : (COSINEZERO/2);

		x /= COSINEZERO;
		y /= COSINEZERO;

	} else {

		x = xs;
		y = ys;

	}

	x += (Int32)transMatrix.screenx;
	y = (Int32)transMatrix.screeny - y;

	*xr = (Coord)x;
	*yr = (Coord)y;

	if (x < clipLeft || x > clipRight || y < clipTop || y > clipBottom) return false;

	return true;
	
}

/*
 * function : ReverseTransform
 *
 * Applies the transformation matrix, transMatrix, in reverse to the specified
 * *screen* coordinate and returns the resulting map coordinate
 *
 */

static void ReverseTransform(Coord xi, Coord yi, Coord *xr, Coord *yr) {
	
	Int32 xs = (Int32)xi;
	Int32 ys = (Int32)yi;

	xs -= (Int32)transMatrix.screenx;
	ys = (Int32)transMatrix.screeny - ys;

	if (transMatrix.rotation != 0) {

		Int32 sn,cs;
		Int32 x,y;
		Int16 rot = -transMatrix.rotation;
		
		WRAPMAX(rot,360);

		GetSinCos(rot, &sn, &cs);

		x = xs * cs + ys * sn;
		y = ys * cs - xs * sn;

		x += x < 0 ? -(COSINEZERO/2) : (COSINEZERO/2);
		y += y < 0 ? -(COSINEZERO/2) : (COSINEZERO/2);

		xs = x / COSINEZERO;
		ys = y / COSINEZERO;

	}

	xs -= (Int32)transMatrix.mapx;
	ys -= (Int32)transMatrix.mapy;

	*xr = (Coord)xs;
	*yr = (Coord)ys;

}

/*******************************************************************************
 *
 * public functions
 *
 */

//PUB:

/*
 * MapInit
 *
 */
	
MapType MapInit(Int32 lat, Int32 lon, Int32 scaling, Coord pxWidth, Coord pxHeight,
		Boolean showRoute, Boolean showTrackLog,
		WaypointClassType wpTypes, WaypointClassType wpLabels,
		AirspaceClassType airspaceTypes, AirspaceClassType airspaceLabels,
		FlightPlanType flightPlan, Boolean previewMode,
		TerrainPaletteType terrainPalette)  {

	Int32 lat1, lon1, lat2, lon2;
	Coord xRange = pxWidth / 2;
	Coord yRange = pxHeight / 2;
	Int32 lonScaling;
	MapType map;

	LOGENTRY;

	map = PFMalloc(sizeof(struct MapTypeStruct));
	ModErrThrowIf(!map);

	map->complete = false;

	map->flightPlan = flightPlan;
	map->previewMode = previewMode;

	map->showRoute = showRoute;
	map->showTrackLog = showTrackLog;
	
	map->trackLog = NULL;
	map->numTrackLines = 0;

	map->fiveNMPixels = RAD_TO_INT32(NM_TO_RAD(5.0)) / scaling;

	if (!previewMode) {

		xRange += map->fiveNMPixels;
		yRange += map->fiveNMPixels;

	}

	/*
	 * clipping limits for determining what to include & draw on
	 * the map. 11/8 = 1.414 = 1/cos(45)
	 *
	 * NB at 1nm scale in hi-res: 5nm = 1600px + 480x = 2080px
	 *
	 * 32000/2080 = 15, e.g. max multiplier is 15, so I settled
	 * for 11/8 as the closest I can get in 16 bits
	 *
	 */

	map->clipLimitX = (11*xRange)/8;
	map->clipLimitY = (11*yRange)/8;

	LOGINT16(map->fiveNMPixels);
	LOGINT16(map->clipLimitX);
	LOGINT16(map->clipLimitY);
	
	/*
	 *
	 * Calculate the bounding limits of the map
	 *
	 * longitude scaling is dependant on cosine of the latitude
	 *
	 */

	lonScaling = (Int32) ( round( (double)scaling/ cos(INT32_TO_RAD(lat))) );

	lat1 = (lat + (Int32)yRange * scaling) / 65536;
	lon1 = (lon - (Int32)xRange * lonScaling) / 65536;
	if (lon1 < -32767) lon1 += 65536;

	lat2 = (lat - (Int32)yRange * scaling) / 65536;
	lon2 = (lon + (Int32)xRange * lonScaling) / 65536;
	if (lon2 > 32768) lon2 -= 65536;

	LOGINT32(lat);LOGINT32(lon);
	LOGINT16(lat1);LOGINT16(lon1);
	LOGINT16(lat2);LOGINT16(lon2);

	map->latRef = lat;
	map->lonRef = lon;
	map->lat1   = (Int16)lat1;
	map->lon1   = (Int16)lon1;
	map->lat2   = (Int16)lat2;
	map->lon2   = (Int16)lon2;

	map->wpTypes = wpTypes;
	map->wpLabels = wpLabels;
	map->airspaceTypes = airspaceTypes;
	map->airspaceLabels = airspaceLabels;

	map->numAirspaces = 0;

	map->latScaling= scaling;
	map->lonScaling= lonScaling;

	/*
	 * map reduction occurs when 1nm < 3 map units (pixels)
	 *
	 */

#ifdef PROFILE
	map->drawReduced = false;
#else
	map->drawReduced = ( RAD_TO_INT32(NM_TO_RAD(1.0)) / scaling < 6);
#endif

	map->icons = NULL;
	map->numIcons = 0;

	map->routeIcons = NULL;
	map->numRouteIcons = 0;

	map->routeLines = NULL;
	map->numRouteLines = 0;

	map->airspace = NULL;
	map->airspaceIndex = NULL;

	map->airIDs = NULL;

	LOGLINE;

	map->sinlat   = sin(INT32_TO_RAD(lat));
	map->coslat   = cos(INT32_TO_RAD(lat));

	map->currentSource = srcAirspace;

	SetupWaypointHash();

	MapIconSetScale(scaling, RAD_TO_INT32(NM_TO_RAD((float)Preferences.atzRadius/8))/scaling);

	TIMERSTART("Route/Icons");
	AddProxIcons(map);
	AddRoute(map);
	TIMERSTOP(50);

	AddAirspace(map, 0);

	/*
	 * terrain
	 *
	 */

	map->terrain = Terrain;

	if (map->terrain) TerrainSetPalette(map->terrain, terrainPalette, FILEROOT APPNAME"-Palette.fma", Preferences.nightMode);

	LOGEXIT;

	return map;

}


/*
 * MapFree
 *
 */

void MapFree(MapType this)  {

	Int16 j;

	LOGENTRY;

	if (this->routeLines) PFMemFree(this->routeLines);
	if (this->icons) PFMemFree(this->icons);
	if (this->routeIcons) PFMemFree(this->routeIcons);

	if (this->airspace) {
		
		for (j=0;j<this->numAirspaces;j++) 
			PFMemFree(this->airspace[j].lines);

		PFMemFree(this->airspace);
		PFMemFree(this->airspaceIndex);

	}

	if (this->airIDs) PFMemFree(this->airIDs);
	if (this->trackLog) PFMemFree(this->trackLog);

	PFMemFree(this);

	LOGEXIT;

}


/*
 * MapBuild
 *
 */

Boolean MapBuild(MapType this, UInt32 maxTicks)  {

	Boolean finished = false;
	UInt32 now;
	UInt32 subTicks = (UInt32)maxTicks;

	LOGENTRY;

	/*
	 * loop until we've run out of time or until we've finished
	 * the map
	 *
	 */

	now = PFGetTicks();

 	do {

		if (this->currentSource == srcAirspace) {

			/*
			 * airspace
			 *
			 */

			LOGTAG("Add airspace");

			if (AddAirspace(this, subTicks)) {

				if (this->showTrackLog) {
					
					AddTrackLog(this, 0);
					this->currentSource = srcTrackLog;

				} else {

					finished = true;

				}

			}

		} else {

			if (AddTrackLog(this, subTicks)) {

				finished = true;

			}

		}
		
		LOGINT16(this->numIcons);
		LOGINT16(this->numAirspaces);

		subTicks = PFTimerTicksLeft(now, maxTicks);

	} while (!finished && subTicks);

	this->complete = finished;

	LOGEXIT;

	return finished;

}


/*
 * MapDraw
 *
 */

Boolean MapDraw(MapType this,  Int32 maxTicks,
		Int32 lat, Int32 lon, Int16 rotation,
		Int32 acLat, Int32 acLon, 
		Int32 terrainAltWarning,
		Coord xOrigin, Coord yOrigin,
		Coord xlimit, Coord ylimit,
		Coord clipLimit,
		Int32 lowerAltFilter,
		Int32 upperAltFilter,
		Int32 lowerFilter2, Int32 upperFilter2,
		Int32 obstacleThreshold) {

	Int16         j;

	UInt32 now;

	Boolean tooLong = false;

	Int32 lat1, lon1, lat2, lon2;
	Int32 lat3, lon3, lat4, lon4;

	Coord ax, ay;	// onscreen location of aircraft

	Boolean drawThick;	// true if zoomed in enough to draw thick lines
	
	LOGENTRY;

	LOGINT16(this->numIcons);
	LOGINT16(this->numAirspaces);

	TIMERSTART("MapDraw");

	if (lat == MAPORIGIN && lon == MAPORIGIN) {

		lat = this->latRef;
		lon = this->lonRef;

	} else if (lat == MAPLAST && lon == MAPLAST) {

		lat = this->lastDrawLat;
		lon = this->lastDrawLon;
		xOrigin = this->lastXOrigin;
		yOrigin = this->lastYOrigin;
		rotation = this->lastRotation;

	}

	this->lastDrawLat = lat;
	this->lastDrawLon = lon;
	this->lastXOrigin = xOrigin;
	this->lastYOrigin = yOrigin;
	this->lastRotation = rotation;

	/*
	 * setup the translation matrix
	 *
	 */

	/*
	 * translate the map to the specified origin
	 *
	 */

	transMatrix.mapx = (this->lonRef - lon)/ this->lonScaling;
	transMatrix.mapy = (this->latRef - lat) / this->latScaling;

	/*
	 * rotation, if in track-up mode
	 *
	 */

	transMatrix.rotation = rotation;

	if (rotation) {

		transMatrix.sine = sin(DEG_TO_RAD(transMatrix.rotation))*COSINEZERO;
		transMatrix.cosine = cos(DEG_TO_RAD(transMatrix.rotation))*COSINEZERO;

	}

	/*
	 * screen origin
	 *
	 */

	transMatrix.screenx = xOrigin;
	transMatrix.screeny = yOrigin;

	/*
	 * screen clipping limits
	 *
	 */

	clipLeft = -clipLimit;
	clipRight = xlimit + clipLimit;
	clipTop = -clipLimit;
	clipBottom = ylimit + clipLimit;

	/*
	 * Draw the terrain 
	 *
	 * Don't start the timer until after, because the terrain manager may load
	 * data from SD card and this often causes the airspace/waypoints to
	 * disappear momentarily.
	 * 
	 */

	LOGLINE;
	
	if (this->terrain) {
			
			/*
			 * The Terrain module will draw *directly* onto the screen, so get pointer
			 * to the screen and it's width/height
			 *
			 */

			UInt16 scrWidth, scrHeight, rowWidth;
			UInt8 *screen = (UInt8*)PFGetScreenAddressAndDimensions(&scrWidth, &scrHeight, &rowWidth);

			LOGINT16(scrWidth); LOGINT16(scrHeight);LOGINT16(rowWidth);

			MapGetLatLon(this, 0,0, &lat1, &lon1);
			MapGetLatLon(this, scrWidth-1,0, &lat2, &lon2);
			MapGetLatLon(this, scrWidth-1,scrHeight-1, &lat3, &lon3);
			MapGetLatLon(this, 0,scrHeight-1, &lat4, &lon4);
			
			LOGINT32(lat1);LOGINT32(lon1);
			LOGINT32(lat2);LOGINT32(lon2);

			//TerrainRenderNorthUp(map->terrain, lat1, -lon1, lat3, -lon3, (void*)screen,
					//scrWidth, scrHeight);

			TIMERSTART("Terrain");
			TerrainSetAlertAlt(this->terrain, terrainAltWarning);
			TerrainRender(this->terrain, lat1, lon1, lat2, lon2, 
							lat3, lon3, lat4, lon4, (void*)screen, scrWidth, scrHeight, rowWidth);
			
			TIMERSTOP(100);
			
	}

	now = PFGetTicks();
	LOGLINE;
	PFDrawStatePush();

	/*
	 * draw airspace
	 *
	 * Notice that airspace is drawn in order according the size of the
	 * airspace, so we use airspaceIndex as the index into the airspace data.
	 *
	 */

	PFSetDrawMode(blockMode);

	drawThick = (this->fiveNMPixels > 21);

	TIMERSTART("MapAirspace");
#ifdef PROFILE
	for (j=0;j<this->numAirspaces;j++) {
#else
	for (j=0;j<this->numAirspaces && !PFTimerHasExpired(now,maxTicks);j++) {
#endif

		Boolean aboveBottom;
		Boolean border;
		MapAirspaceType *mas = &this->airspace[this->airspaceIndex[j]];

		/*
		 * filter by altitude
		 *
		 */


		if (mas->clipCount || mas->lowerAlt > upperAltFilter || mas->upperAlt < lowerAltFilter) {

			LOGSTR(mas->upperAltString);
			LOGINT32(mas->lowerAlt);
			LOGINT32(mas->upperAlt);
			LOGINT32(lowerAltFilter);
			LOGINT32(upperAltFilter);

			mas->visible = false;

			continue;

		}

		border = (Boolean)((mas->lowerAlt <= upperFilter2) && (mas->upperAlt >= lowerFilter2));

		aboveBottom = MapAirspaceDraw(mas, border, drawThick, false, (this->airspaceLabels & mas->type) ? true : false,
				&ApplyTransform);

		if (!aboveBottom && transMatrix.rotation == 0) {

			mas->clipCount = 127;

		}

	}
	
	TIMERSTOP(160);

	/*
	 * track log
	 *
	 */

	if (!this->previewMode && this->showTrackLog && this->complete && TLGetNumNewEntries(this->tlFwdRef) > 1) {

		TLInternalRecordType tlr1;
		TLInternalRecordType tlr2;
		Coord x1, y1, x2, y2;
		TLRefType anchor = this->tlRef;
		TLRefType end;
		LineType *wl = (LineType*) (this->trackLog);
		
		/*
		 * add additional segments to the end of the track log, representing
		 * the course we've travelled since the map was finished being built
		 *
		 * tlRef is used as the anchor point, we add another segment between it
		 * and tlFwdRef when AddTrackSegment tells us that the segment between
		 * those two points is long enough; then the anchor becomes the current
		 * end point and the process starts again
		 *
		 */

		LOGINT16(this->tlRef);LOGINT16(this->tlFwdRef);

		if (this->tlFwdRef == this->tlRef)
			TLGetForwardPoint(&this->tlFwdRef, &tlr1);

		end = this->tlFwdRef;
		tlr1.lat = 0; tlr1.lon = 0;

		while (TLGetForwardPoint(&anchor, &tlr1) && tlr1.lat == 0 && tlr1.lon ==0);

		if (TLGetForwardPoint(&this->tlFwdRef, &tlr2)) {

			x1 = (tlr1.lon - this->lonRef) / this->lonScaling;
			y1 = (tlr1.lat - this->latRef) / this->latScaling;

			x2 = (tlr2.lon - this->lonRef) / this->lonScaling;
			y2 = (tlr2.lat - this->latRef) / this->latScaling;

			if (AddTrackSegment(this, wl, x1, y1, x2, y2)) {
				
				this->tlRef = end;

			}

		}

		LOGINT16(this->tlRef);LOGINT16(this->tlFwdRef);

	}

	if (this->numTrackLines) {

		LineType *wl = (LineType*)(this->trackLog);
		LineType *dl = PFMalloc(PFMallocSize(this->trackLog));

		/*
		 * add a new track point, after the previously stored one
		 *
		 */

		LOGTAG("Track");

		for (j=0;j<this->numTrackLines;j++) {

			ApplyTransform(wl[j].x1, wl[j].y1, &dl[j].x1, &dl[j].y1);
			ApplyTransform(wl[j].x2, wl[j].y2, &dl[j].x2, &dl[j].y2);

		}

		PFDrawThickLines(dl, this->numTrackLines, AppColourPrefs.ground, 0,0 );

		PFMemFree(dl);

	}

	MapGetScreenCoords(this, acLat, acLon, &ax, &ay);

	LOGTAG("Icons");
	LOGTIMESTART;
	
	if (this->routeIcons) {

		DrawIcons(this, this->routeIcons, this->numRouteIcons, ax, ay, obstacleThreshold);

	}

	if (this->icons) {

		DrawIcons(this, this->icons, this->numIcons, ax, ay, obstacleThreshold);

	}

	LOGTIMESTOP;
	
	/*
	 * draw the route 
	 *
	 */

	if (this->numRouteLines) {

		LineType *route = PFMalloc(sizeof(LineType)*this->numRouteLines);

		for (j=0;j<this->numRouteLines;j++) {

			LOGLINE;
				
			ApplyTransform(this->routeLines[j].x1, this->routeLines[j].y1, &route[j].x1, &route[j].y1);
			ApplyTransform(this->routeLines[j].x2, this->routeLines[j].y2, &route[j].x2, &route[j].y2);

		}

		PFDrawThickLines(route, 1, AppColourPrefs.route, AppColourPrefs.route, this->previewMode ? 1:2);
		PFDrawThickLines(&route[1], this->numRouteLines-1, AppColourPrefs.route, AppColourPrefs.route, 1);
		PFMemFree(route);

	}


	LOGLINE;

	PFDrawStatePop();


	/*
	 * draw the selected element
	 *
	 */

	PFSetTextColour(AppColourPrefs.label);

#ifdef OLD_SELECTION_CODE
	if (MapSelectionIsSet(this)) {
		
		if (this->selectedDisplayItem > -1) {

			switch (this->selectedType) {
	
			case msWaypoint:
			case msRouteWaypoint: {
					MapIconType *icons = this->selectedType == msWaypoint ? this->icons : this->routeIcons;
					MapIconType *icon = &icons[this->selectedDisplayItem];
					Coord x,y;
	
					if (ApplyTransform(icon->x, icon->y, &x, &y)) {
	
						MapIconDrawSelected(icon, x, y, transMatrix.rotation);
	
					}
	
				}
				break;
	
			case msAirspace:
				MapAirspaceDraw(&this->airspace[this->selectedDisplayItem], true, drawThick, true, true, &ApplyTransform);
				break;
	
			default:
				ModErrThrowIf(1);
				break;
	

			}
			
		}

	}
#endif
	
	LOGLINE;

#ifdef SAFETYALT
	
	if (safetyAltitude > 0) {

		char warning[30];
		Int32 alt = (Int32)(UC.altitudeConv * (float)safetyAltitude);

		alt = ((alt + 50) / 50) * 50;

		StrPrintF(warning, "CLIMB TO %ld%s", alt, UC.altitudeUnits);

		PFSetTextColour(AppColourPrefs.warning);
		FntSetFont(boldFont);
		DrawAlignedChars(warning, ALIGNCENTRE, lscreen.xcentre, lscreen.ledHeight);

	}
#endif

	TIMERSTOP(130);

	tooLong = PFTimerHasExpired(now, maxTicks);

	/*
	 * reduce detail next time around
	 *
	 */

#ifndef PROFILE
	//if (tooLong) map->drawReduced = true;
#endif

	LOGEXIT;
	return !tooLong;

}

/*
 * MapSelectByCoord
 *
 */

MapSelection MapSelectByCoord(MapType this, const Coord x, const Coord y, Coord threshold, MapSelection currentSelection) {

	Coord mapx, mapy;
	MapIconType *icons = NULL, *routeIcons = NULL;
	MapAirspaceType *airspace;

	MapSelectionType *type;
	Int16 *idx;
	
	Int16 numItems,j;
	Int16 selector, stop;

	MapSelection newSelection = NULL;

	LOGENTRY;

	numItems = this->numAirspaces + this->numIcons + this->numRouteIcons;

	if (!numItems) {
		
		LOGEXIT;
		return NULL;

	}

	type = PFMalloc(sizeof(type)*numItems);
	idx = PFMalloc(sizeof(idx)*numItems);

	/*
	 * the sequence of construction below is important, as it affects the
	 * calculations in following switch statement
	 *
	 * NB Important for route icons to be first, as selecting a route Icon which
	 * has a waypoint in WPDataset causes the icon to be selected.
	 * 
	 */

	numItems = 0;
	for (j=0;j<this->numRouteIcons;j++) {
		type[numItems] = msRouteWaypoint;
		idx[numItems++] = j;
	}
	
	for (j=0;j<this->numIcons;j++) {
		type[numItems] = msWaypoint;
		idx[numItems++] = j;
	}
	
	for (j=0;j < this->numAirspaces;j++) {
		type[numItems] = msAirspace;
		idx[numItems++] = j;
	}

	/*
	 * if no selection, start from the beginning of the search otherwise
	 * start from the next element after startFrom.
	 *
	 */

	selector = 0;
	if (currentSelection) {
	
		Int16 j;
		
		switch (currentSelection->type) {
		case msAirspace:
			for (j=0;j<this->numAirspaces;j++)
				if (this->airspace[j].id == currentSelection->airspace.id){
					selector = j +  this->numIcons + this->numRouteIcons + 1;
					break;
				}
			break;
			
		case msWaypoint:
			for (j=0;j<this->numIcons;j++) 
				if (this->icons[j].id == currentSelection->waypoint.id) {
					selector = this->numRouteIcons + j + 1;
					break;
				}
			break;
			
		case msRouteWaypoint:
			selector = currentSelection->waypoint.id + 1;
			break;
			
		default:
			break;
			
		}
	
	}
	
	if (selector == numItems) selector = 0;

	stop = selector;

	LOGTIMESTART;

	ReverseTransform(x, y, &mapx, &mapy);

	LOGINT16(x);
	LOGINT16(y);
	LOGINT16(mapx);
	LOGINT16(mapy);
	
	if (this->icons) icons = this->icons;
	if (this->routeIcons) routeIcons = this->routeIcons;

	do {

		const MapIconType *icon;
		Coord lx,ly;

		LOGINT16(j);
		switch (type[selector]) {

		case msWaypoint:
		case msRouteWaypoint:
			icon = type[selector] == msWaypoint ? &icons[idx[selector]] : &routeIcons[idx[selector]];

			lx = icon->x - mapx;
			if (lx < 0) lx=-lx;

			if ( lx <= threshold ) {
			//if ( icon->id && x <= threshold ) {

				ly = icon->y - mapy;
				if (ly<0) ly=-ly;

				if ( ly <= threshold ) {

					if (type[selector] == msWaypoint) {

						newSelection = MapSelectionNewWaypoint(icon->id);
						
					} else if (type[selector] == msRouteWaypoint) {

						WaypointIDType wpID = NavGetIDOfPlanWaypoint(icon->id);

						if (wpID != wpNotFound) {

							Int16 j;
							
							for (j=0; j<this->numIcons;j++)
								if (this->icons[j].id == wpID) break;
							
							if (j == this->numIcons) wpID = wpNotFound;
							
						} 

						if (wpID == wpNotFound ){

							newSelection = MapSelectionNewRouteWaypoint(this->flightPlan, icon->id);

						} else {
							
							newSelection = MapSelectionNewWaypoint(wpID);
							
						}

					}

				}

			}

			break;

		case msAirspace:

			airspace = &this->airspace[idx[selector]];

			if (airspace->visible && PFScreenPointInRectangle(mapx,mapy, &airspace->bounds)) {

				Int16          k;
				LineType   *ln;
				Int32          distance;
				Int32          abx, aby, acx, acy, bcx, bcy;

				/*
				 * A = start of line, B=end of line, C=Tap Point
				 *
				 */

				LOGTAG("Vectors");
				for (k=0;k<airspace->numLines;k++) {

					ln = &airspace->lines[k];

					abx = (Int32) (ln->x2 - ln->x1);
					aby = (Int32) (ln->y2 - ln->y1);

					acx = (Int32) (mapx - ln->x1);
					acy = (Int32) (mapy - ln->y1);

					bcx = (Int32) (mapx - ln->x2);
					bcy = (Int32) (mapy - ln->y2);

					/*
					 * use dot product to determine if
					 * mapx,mapy is somewhere along the
					 * line AB. If it isn't then don't
					 * bother testing it
					 *
					 */

					if ( (abx == 0 && aby == 0) || (abx*bcx + aby*bcy) > 0 || (-abx*acx + -aby*acy) > 0) {
						
						LOGTAG("discounted");
						continue;

					}
					
					/*
					 * cross-product gives range from point to line
					 *
					 */

					distance = (abx*acy - aby*acx) /  (1+IntSqrt( abx*abx + aby*aby ));
					if (distance < 0) distance = -distance;
					if (distance < threshold+4) { 
						Int32 lat,lon;

						LOGINT16(ln->x1); LOGINT16(ln->y1);
						LOGINT16(ln->x2); LOGINT16(ln->y2);
						
						LOGINT32(abx); LOGINT32(aby);
						LOGINT32(acx); LOGINT32(acy);
						LOGINT32(IntSqrt((abx*abx+aby*aby)));

						MapGetLatLon(this, x, y, &lat, &lon);
						newSelection = MapSelectionNewAirspace(airspace->id, lat,lon);

						break;

					}

				}

			}
			break;

		default:

			ModErrThrowIf(1);
		
		}

		selector++;

		/*
		 * wrap-around
		 *
		 */

		if (selector == numItems) selector = 0;

	} while (!newSelection && selector != stop);

	LOGTIMESTOP;
	
	PFMemFree(idx);
	PFMemFree(type);

	if (!newSelection) {
		
		WorldCoords wc;
		
		MapGetLatLon(this, x, y, &wc.lat, &wc.lon);
		newSelection = MapSelectionNewFreePoint(&wc);
		
	}
	LOGEXIT;
	return newSelection;

}

/*
 * MapSelectByReference
 *
 */

MapSelection MapSelectionNewWaypoint(WaypointIDType wpID)  {

	Int32 lat, lon;
	Waypoint *wpPtr = WDMGetWaypoint(WPDataset, wpID);
	struct MapSelectionStruct *newSelection = PFMalloc(sizeof(struct MapSelectionStruct));
	
	ModErrThrowIf(!wpPtr);
	ModErrThrowIf(!newSelection)
	
	newSelection->type = msWaypoint;
	newSelection->waypoint.id = wpID;
	newSelection->magVar = wpPtr->magVar;
	
	StrNCopy(newSelection->waypoint.ident, wpPtr->ident, sizeof(newSelection->waypoint.ident));
	
	lat = RAD_TO_INT32(wpPtr->latitude); 
	lon = RAD_TO_INT32(wpPtr->longitude);
	
	SetWorldCoords(newSelection->position,lat,lon);

	newSelection->waypoint.routeWaypoint = -1;

	StrNCopy(newSelection->waypoint.ident, wpPtr->ident, sizeof(newSelection->waypoint.ident));
	
	PFMemFree(wpPtr);
	
	return newSelection;
	
}

/*
 * MapSelectionNewRouteWaypoint
 * 
 * Select the specified route waypoint, creating new selection and
 * returning pointer to it (caller must free)
 * 
 */

MapSelection MapSelectionNewRouteWaypoint(FlightPlanType fp, Int16 routeWaypoint) {

	struct MapSelectionStruct *newSelection = PFMalloc(sizeof(struct MapSelectionStruct));

	const FlightPlanLegWaypointType *fpl = FpGetWaypoint(fp, routeWaypoint);
	ModErrThrowIf(!newSelection)

	newSelection->type = msRouteWaypoint;
	newSelection->magVar = fpl->magVar;
	newSelection->waypoint.id = wpNotFound;
	newSelection->waypoint.routeWaypoint = routeWaypoint;
	StrNCopy(newSelection->waypoint.ident, fpl->ident, sizeof(newSelection->waypoint.ident));
	
	SetWorldCoords(newSelection->position, fpl->lat, fpl->lon);
	
	return newSelection;
	
}

/*
 * MapSelectionNewAirspace
 * 
 * Select the specified airspace, creating new selection and
 * returning pointer to it (caller must free)
 * 
 */

MapSelection MapSelectionNewAirspace(AirspaceIDType asID, Int32 lat, Int32 lon) {

	struct MapSelectionStruct *newSelection;

	const WorldCoords wc = { lat, lon };
	
	newSelection = MapSelectionNewFreePoint(&wc);
	
	StrNCopy(newSelection->waypoint.ident,"Bdry",sizeof(newSelection->waypoint.ident));
	newSelection->type = msAirspace;
	newSelection->airspace.id = asID;
	
	return newSelection;
	
}

/*
 * MapSelectionNewFreePoint
 * 
 */

MapSelection MapSelectionNewFreePoint(const WorldCoords *wc) {
	
	struct MapSelectionStruct *newSelection = PFMalloc(sizeof(struct MapSelectionStruct));

	ModErrThrowIf(!newSelection)

	SetWorldCoords(newSelection->position, wc->lat, wc->lon);

	newSelection->magVar = DEG_TO_RAD(AvCalcMagVarn(INT32_TO_RAD(wc->lat), INT32_TO_RAD(wc->lon), 0.0, 2008));
	
	StrCopy(newSelection->waypoint.ident,"wp");
	newSelection->type = msFreePoint;
	return newSelection;
	
}

/*
 * MapSelectionFree
 *
 */

void MapSelectionFree(MapSelection *this) {

	if (*this) PFMemFree(*this);
	
	*this = NULL;
	
}

/*
 * MapSelectionGetType
 * 
 */

MapSelectionType MapSelectionGetType(MapSelection this) {
	
	if (!this) return msNone;
	
	return this->type;
	
}

/*
 * MapSelectionGetCoords
 * 
 * Returns pointer to map's current selection coordinates.
 * 
 */

const WorldCoords *MapSelectionGetCoords(MapSelection this) {

	ModErrThrowIf(!this);
	
	return &(this->position);
	
}


/*
 * MapSelectionGetWaypointID
 * 
 */

WaypointIDType MapSelectionGetWaypointID(MapSelection this) {
	
	if (!this || (this->type != msWaypoint && this->type != msRouteWaypoint)) return wpNotFound;
	
	return this->waypoint.id;
	
}

/*
 * MapSelectionGetRouteWaypoint
 * 
 */

Int16 MapSelectionGetRouteWaypoint(MapSelection this) {

	if (!this || this->type != msRouteWaypoint) return -1;
	
	return this->waypoint.routeWaypoint;
	
}

/*
 * MapSelectionGetIdent
 * 
 */

char *MapSelectionGetIdent(MapSelection this) {
	
	ModErrThrowIf((!this));  // || (sel->type != msRouteWaypoint && sel->type != msWaypoint && sel->type != msFreePoint));
	
	return this->waypoint.ident;
	
}

/*
 * MapSelectionGetMagVar
 * 
 */

float MapSelectionGetMagVar(MapSelection this) {
	
	ModErrThrowIf(!this);
	return this->magVar;
	
}

/*
 * MapSelectionGetAirspaceID
 * 
 */

AirspaceIDType MapSelectionGetAirspaceID(MapSelection this) {

	if (!this || this->type != msAirspace) return asNotFound;
	
	return this->airspace.id;
	
}

/*
 * MapSelectionDraw
 * 
 */

void MapSelectionDraw(MapType map, MapSelection this) {

	Int16 j;
	
	if (!this) return;
	
	if (this->type == msAirspace) {
		
		AirspaceIDType selectionASID = MapSelectionGetAirspaceID(this);
		
		for (j=0;j<map->numAirspaces;j++) {
			if (map->airspace[j].id == selectionASID) {
				
				MapAirspaceDraw(&map->airspace[j], true, true, true, true, ApplyTransform);
				break;
				
			}
				
		}
		
	} else if (this->type == msWaypoint) {
	
		WaypointIDType selectionWPID = MapSelectionGetWaypointID(this);
		
		for (j=0;j<map->numIcons;j++) {
			
			if (map->icons[j].id == selectionWPID) {
				Coord x, y;

				ApplyTransform(map->icons[j].x, map->icons[j].y, &x, &y);
				MapIconDrawSelected(&IconWindows, &map->icons[j],x,y, transMatrix.rotation);
				return;
				
			}
			
		}
			
	} else if (this->type == msRouteWaypoint) {
		
		const Int16 selectedRouteWP = MapSelectionGetRouteWaypoint(this);
		Coord x,y;
		Int16 j;
		
		for (j=0; j<map->numRouteIcons;j++)
			if (map->routeIcons[j].id == selectedRouteWP) break;
		
		if (j < map->numRouteIcons) {
			
			ApplyTransform(map->routeIcons[j].x, map->routeIcons[j].y, &x, &y);
			MapIconDrawSelected(&IconWindows, &map->routeIcons[j], x,y, transMatrix.rotation);
			
		}
		
	}
	
	if (this->type == msFreePoint || this->type == msAirspace) {
		
		PFScreenRectType r;
		Coord ix,iy;
		
		MapGetScreenCoords(map, this->position.lat, this->position.lon, &ix, &iy);

		ix = ix - 6;
		iy = iy - 6;

		PFScreenRectangleSetRel(&r, ix, iy, 12,12);
		PFSetForeColour(AppColourPrefs.black);
		PFPaintRectangleFrame(&r, boldRoundFrame);

	}

}

/*
 * MapGetScreenCoords
 *
 */

Boolean MapGetScreenCoords(MapType this, Int32 lat, Int32 lon, Coord *x, Coord *y)  {

	Coord mapx, mapy;

	mapx = (lon - this->lonRef) / this->lonScaling;
	mapy = (lat - this->latRef) / this->latScaling;

	return ApplyTransform(mapx, mapy, x, y);

}

/*
 * MapGetLatLon
 *
 */

void MapGetLatLon(MapType this, Coord x, Coord y, Int32 *lat, Int32 *lon) {

	Coord mapx, mapy;

	ReverseTransform(x, y, &mapx, &mapy);

	*lon = this->lonRef + mapx * this->lonScaling;
	*lat = this->latRef + mapy * this->latScaling;

}

/*
 * MapUpdatePlan
 *
 */

void MapUpdatePlan (MapType this, FlightPlanType plan) {

	this->flightPlan = plan;

	AddRoute(this);

}

