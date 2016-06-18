/*
 * GlobalTypes.h
 *
 * (c) 2002 Blackhawk Systems Ltd.
 */
#ifndef GLOBAL_TYPES_H_INCLUDED
#define GLOBAL_TYPES_H_INCLUDED

#include "Platform.h"
#include "Gps.h"

#include "AsDatabase.h"
#include "WDManager.h"
#include "TerrainType.h"

#define MAXIDCHARS	10

#define PIRADIANS ((Int32)2147483647)

typedef struct {
	
	Int32 lat;
	Int32 lon;
	
} WorldCoords;

#define SetWorldCoords(w,swclat,swclon) (w).lat = swclat;(w).lon = swclon

/* 
 * Preferences 
 *
 */

#define NM_UNITS          0
#define MI_UNITS          1
#define KM_UNITS          2
#define CP_UNITS          3

#define FEET_UNITS        0
#define METRE_UNITS       1

typedef enum { surfEither, surfHard, surfGrass } RunwaySurfaceType;

typedef enum { fddOff = 0, fddBasic, fddFull } FlightDataDisplayType;

#define aptFilter (wpAirfield | wpLargeAirfield)
#define vorFilter wpVOR 
#define ndbFilter wpNDB

/*
 * structure for grouping the databases together in various ways...
 *
 */

typedef struct {

	WDMDatabaseIDType cpSystem;
	WDMDatabaseIDType cpUser;
	WDMDatabaseIDType obs[10];

	WDMDatabaseIDType noObstacles; 	// cpSystem | cpUser
	WDMDatabaseIDType obstacles;	// cpUser | obs[0..9]
	WDMDatabaseIDType all;

} FMDataset;

typedef enum { trackLogOff = 0, trackLog1s, trackLog5s, trackLog10s } TrackLogSettingType;

/*
 * Colour preferences
 *
 */

typedef struct {

	IndexedColorType route;
	IndexedColorType track;
	IndexedColorType atz;
	IndexedColorType label;

	IndexedColorType sky;
	IndexedColorType ground;
	IndexedColorType aaLines;
	IndexedColorType warning;
	IndexedColorType pointer;

	IndexedColorType satConstOuter;
	IndexedColorType satConstInner;

	IndexedColorType red;
	IndexedColorType yellow;
	IndexedColorType green;

	IndexedColorType black;
	IndexedColorType white;

	IndexedColorType vsiClimb;
	IndexedColorType vsiDescend;

	IndexedColorType statusBar;
	IndexedColorType statusBarLight;
	
	IndexedColorType background;
	IndexedColorType dataDisplay;

	IndexedColorType airspace[7];
	IndexedColorType airspaceOutline[8];
	IndexedColorType airway;
	IndexedColorType suas;
	IndexedColorType classOther;

	IndexedColorType airfieldIcon;
	IndexedColorType otherIcon;

} AppColourPrefsType;


/*
 * icon information
 *
 */

typedef struct {

	WinHandle aircraft;
	WinHandle icons;
	WinHandle iconMasks;

	Coord aircraftDim;
	Coord iconDim;

} IconWindowsType;

/*
 * display units allow user to configure what units his readings are displayed
 * in. The values are taken from CoPilot, for compatibility purposes
 */

typedef struct {
	UInt16 distance;	/* 0=nm, 1=mi, 2=km */
	UInt16 altitude;	/* 0=feet, 1=metres */
	UInt16 speed;	/* 0=kts, 1=mph, 2=kph */
} DisplayUnitsType;

/*
 * this structure holds the current set of conversions and labels
 *
 */

typedef struct {

	/*
	 * conversion factors, from internal system to user-specified units
	 *
	 */

	float altitudeConv;
	float speedConv;
	float distanceConv;

	float minorUnits;	// multiply to convert from NM to feet, or KM to m

	/*
	 * labels for user-specified units
	 *
	 */

	const char *altitudeUnits;
	const char *speedUnits;
	const char *distanceUnits;

	const char *heading;

} UserConversionType;

typedef enum {timeETE, timeETA, timeLocal, timeUTC, timeETV, timeETT } TimeDisplayType;

/* 
 * some error codes
 */
#define ERR_CANT_INIT_FORM 		0
#define ERR_INVALID_FORM_LOAD_EVENT	1
#define ERR_MEM_ALLOC_ERROR		2
#define ERR_MATHLIB_CLOSE_ERROR 	3
#define ERR_MATHLIB_NOT_FOUND		4

/*
 * some conversion factors
 */
#define PI 		     	  3.1415926535897932384626433832795
#define MI_PER_NM         1.1508
#define KM_PER_NM         1.852
#define METRES_PER_FOOT   0.3048
#define FEET_PER_METRE    (1/METRES_PER_FOOT)
#define FEET_PER_NM		  6076.11549
#define NM_PER_RADIAN     ((180*60)/PI)
#define KM_PER_RADIAN     (NM_PER_RADIAN*KM_PER_NM)
#define MI_PER_RADIAN     (NM_PER_RADIAN*MI_PER_NM)
#define DEG_PER_RAD       (180/PI)
#define RAD_PER_DEG       (PI/180)

#define FEET_TO_NM(x)	  ((x)*(1/6076.11549))
#define NM_TO_FEET(x)	  ((x)*6076.11549)

#define RAD_TO_DEG(x)	  (DEG_PER_RAD*(x))
#define DEG_TO_RAD(x)	  (RAD_PER_DEG*(x))

#define NM_TO_RAD(x)	  ((x)/NM_PER_RADIAN)
#define RAD_TO_NM(x)	  (NM_PER_RADIAN*(x))

#define RAD_TO_INT16(x)   (Int16) ( (x) * 32767 / PI)
#define RAD_TO_INT32(x)   (Int32) ( (x) * 2147483647 / PI )
#define RAD_TO_WORLD(w, lat, lon) ((w).lat = RAD_TO_INT32(lat);(w).lon = RAD_TO_INT32(lon))

#define DEG_TO_INT16(x)   (Int16) ( (x) * 32767 / 180)
#define DEG_TO_INT32(x)   (Int32) ( (x) * 2147483647 / 180 )

#define INT32_TO_RAD(x)   (double) ( (double)(x) * PI / 2147483647 )



/*
 * 1nm in Int32 format
 *
 */
#define INT32_1NM 198841

/*
 * text manipulation macros
 *
 */

/*
 * skip white-space; small kludge as we assume any character code < "!" is
 * white-space. This takes care of newlines, spaces and tabs.
 *
 */

#define SKIP_FIELD(p) while((p)[0]>' ')(p)++;while ((p)[0]<'!') (p)++

#define SKIP_WHITESPACE(p) for (; (p)[0] != 0 && (p)[0] < '!'; (p)++); if ((p)[0] == 0) (p) = NULL

#define SKIP_TO_WHITESPACE(p) for (; (p)[0] != 0 && (p)[0] > ' '; (p)++); if ((p)[0] == 0) (p) = NULL

#define SKIP_LINE(p) for (; (p)[0] != 0 && (p)[0] != '\n'; (p)++); if ((p)[0] != 0) (p)++; else (p) = NULL


/*
 * Function key mappings
 *
 */

typedef enum { noMapping, mapToMenuID, mapToControlID } KeymappingType;

typedef struct {

	WChar  			keyID;
	KeymappingType 	mapTo; 
	UInt16 			itemID;	// can be menu or control ID (according to 'mapTo')

} FunctionKeymapType;

/***************************************************************************
 *
 * Application defined events
 *
 */

typedef enum {


	/*
	 * course override notification event
	 *
	 */

	evtCrsOverride = (firstUserEvent + 2),

	/*
	 * Diversion activate/cancel events
	 *
	 * Activate/Reactivate are context sensitive, so the
	 * receiving form needs to take action to set up the diversion
	 * itself according to what the user is doing (e.g. on the map, the
	 * user may have selected a waypoint).
	 *
	 * ReActivate is sent when a divert has been activated, and a request
	 * for a new divert is received.
	 *
	 * In emergencies, the diversion is already set up and all
	 * the form needs to do is to respond and redraw itself.
	 * 
	 */

	evtDivertCancel,
	evtDivertActivate,
	evtDivertEmergency,
	evtDivertReActivate,

	/*
	 * Waypoint update
	 *
	 */
	evtNextWaypoint ,
	evtPrevWaypoint ,

	/*
	 * Waypoint Information Request
	 *
	 */
	evtWaypointInfoReq ,

	/*
	 * Map configuration request
	 *
	 */
	
	evtMapConfigReq ,

	/*
	 * map edit request
	 *
	 */

	evtMapEditReq,

	/*
	 * GPS messages
	 *
	 */

	evtGPSSync,
	evtGPSSyncLost,
	evtGPSPositionUpdate,
	evtGPSFix,
	evtGPSFixLost,
	evtGPSAltFix,
	evtGPSAltFixLost,
	evtGPSSatUpdate,	// 24595

	/*
	 * Map config update
	 *
	 */

	evtMapConfigUpdate,

	/*
	 * New waypoint, sent when user adds a new waypoint using the WP
	 * command button
	 *
	 */

	evtNewWPAdded,

	/*
	 * sent when alpha-input has been received from the alpha-input dialog
	 *
	 */

	evtAlphaInput,

	/*
	 * sent when keypad has finished
	 *
	 */

	evtKeypadInput,

	/*
	 * flight plan modified
	 *
	 */

	evtFlightPlanLoaded,

	/*
	 * Screen has been redrawn
	 *
	 */

	evtScreenRedrawn,

	/*
	 * end of user events
	 *
	 */

	evtLastUser

} appDefinedEvents;


/***************************************************************************/

/*
 * diversion classification
 */
typedef enum { divNone=0, divActive, divEmergency } DiversionType;

/*
 * you never know  ;-)
 */
typedef long long          Int64;
typedef unsigned long long UInt64;

#define MIN(x,y) ( (x) < (y) ? (x):(y) )
#define MAX(x,y) ( (x) > (y) ? (x):(y) )
#define ABS(a) ( (a) < 0 ? -a : a)

#define WRAPMAX(v,max)  if ( (v) < 0 ) (v)+=(max); else if ( (v) >= (max) ) (v) -= (max)

#define VALIDGPSFIX (GPS.sat.fixType > 1)
#define DEG_MAGVAR_WP(var, rad, mv) do { var = (typeof(var))(RAD_TO_DEG(rad-(Preferences.useMagnetic ?mv:0.0)) + 0.5); WRAPMAX(var,360);} while(0);
#define DEG_MAGVAR_GPS(var, rad, gps) do { var = (typeof(var))(RAD_TO_DEG(rad) - (Preferences.useMagnetic ? (gps).magVarn:0.0) - 0.5); WRAPMAX(var,360);} while(0);


/****************************************************************************
 *
 * Unit Testing macros and datatypes
 *
 */

#define assert(c) if (!((c))) return __LINE__;

typedef enum {
	reportNumTests,
	executeTest
} TestActionType;
	

	
#endif
