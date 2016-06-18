/******************************************************************************
 *
 * CpInterface
 *
 * (c) Blackhawk Systems 2002.
 *
 * This module interfaces to the CoPilot databases, acting as a translation
 * layer.
 *
 */

#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif

#include "Platform.h"
#include <SysUtils.h>
#include "GlobalTypes.h"
#include "CpInterface.h"
#include "AvCalcs.h"
#include "MathLib.h"
#include "Utils.h"
#include "Modules.h"
#include "FlightPlan.h"
#include "WDManager.h"

/**************************************************************************
 *
 * global variables
 *
 */

extern WDMHandle WPDataset;
extern FMDataset FMDS;
extern char ErrExtra[];

/**************************************************************************
 *
 * module variables
 *
 */

//VAR:

/*
 * these constants define the creator ID and the wayoint types 
 * of the CoPilot databases (under control of Laurie Davis)
 *
 */

#define FlightDataType         'Flgt'
#define PrefsDataType          'Pref'
#define AppType                'appl'
#define RouteDataType	       'Rout'

/*
 * the database type for the index, used to look for proximity waypoints
 *
 */

#define IndexDatabaseType     'Indx'
#define CardIndexDatabaseType 'CInd'


#define MAX_FLIGHT_DESCRIPTION 30

#define ModuleID CoPilotModuleID

/*
 * database references for the CoPilot databases that are accessed
 *
 */


static DmOpenRef flightDb;

/*
 * system waypoint database might be on external card, with a links file.
 *
 * This memory is a cache which holds the links file
 *
 */

static UInt32      *linksFile = NULL;
static UInt32		numLinks  = 0;

/*
 * status of the CoPilot interface module. If true, then CpInitialise has been
 * successfully called and the CoPilot module can be used
 *
 */

static Boolean initialised = false;

/*
 * hardSurfaces
 *
 * each entry in this list defines a surface type that is considered
 * to be 'hard', as opposed to soft/grass surface types. 
 *
 */

static const UInt16 numHardSurfaces = 77;
static const char * const hardSurfaces[] = {
#include "RunwaySurfaces.inc"
};

/*
 * information about the last loaded CoPilot plan
 *
 */

static Int16 numSegments;
static Int16 loadedSegment;
static UInt32 planID;
static Boolean alternateAvailable;
static Boolean alternateLoaded;

static const char *CoPilotFlightDB = "CoPilot Flight";
static const char *CoPilotPreferencesDB = "CoPilot Prefs";

/*****************************************************************************
 *
 * CoPilot constants and data structures
 *
 * These structures are under control of Laurie Davis and the CoPilot program
 *
 */

#define airwaySize    12
#define costItems     7
#define maxLegs       40
#define maxSegments   (maxLegs+1)

typedef enum {noType, flightRecord=321, routeRecord, wbRecord, planRecord }
	CpRecordType;

typedef enum {system,user} CpWaypointType;

typedef enum {north, west, south, east, noIntersection }Quadrant;

typedef enum {airw, sid, star, approach} CpAirwayType;

typedef struct {
	UInt32 wb;
	UInt32 plan;
} SegmentTable;

static const UInt16 flightDBVersion = 7;

typedef struct {
	CpRecordType recordType;
	UInt32 aircraft;
	UInt32 pilot;
	UInt32 route;
	SegmentTable segment[maxSegments];
	UInt16 currentWbSegment;
	UInt16 currentPlanSegment;
	UInt32 seconds;
	double fuelCost;
	double cost[costItems];
	UInt32 costType[costItems];
	UInt32 acDistance;
	UInt32 acTime;
	UInt32 acTakeoffs;
	char description[1];
	char note[1];
	char costText[costItems][1];
}FlightStruct;

typedef struct {
	UInt32         uniqueID;
	CpWaypointType wpType;
}CpLegWaypoint ;

/* 
 * groundTime value that indicates start of an alternate leg
 *
 */

static const UInt16 alternateTime = (UInt16)(-1);

typedef struct {
	CpLegWaypoint waypoint[2];
	double     altitude;
	Int16      windDirection;
	double     windSpeed;
	double     pressure;
	double     temp;
	Char       airway[airwaySize+1];
	CpAirwayType airwayType:3;
	Boolean    newSegment:1;
	Boolean    refuel:1;
	UInt16     groundTime;
	double     intersection;
	Quadrant   quadrant;
}LegStruct ;


typedef struct {
	CpRecordType recordType;
	UInt16 numLegs;
	LegStruct leg[maxLegs];
}RouteStruct ;

typedef struct {
	UInt16 currentFlightIndex;          // index of current flight
	UInt32 defaultAircraft;             // uniqueId
	UInt32 defaultPilot;                // uniqueId
	Int32  defaultPlanType;             // {canadian, american, icao}
	UInt16 lastMagVar;                  // 0 = west, 1 = east
	UInt16 lastLongitude;               // 0 = west, 1 = east
	UInt16 lastLatitude;                // 0 = north, 1 = south
	UInt16 distanceUnits;               // {nm, mi, km}
	UInt16 altitudeUnits;               // {ft, m}
	UInt16 pressureUnits;               // {inHg, hPa}
	UInt16 weightUnits;                 // {lb, kg}
	UInt16 armUnits;                    // {in, cm, metre}
	UInt16 speedUnits;                  // {nm, mi, km}
	UInt16 climbRateUnits;              // {ftPm, ftPs, mPm, mPs}
	UInt16 fuelDensityUnits;            // {lbPgalUS, lbPgalImp, kgPL}
	UInt16 preferencesShown;            // true if prefs have been Oked
	Char   initialChar;                 // default initial ident char

	/* there's more than the fields shown here, but we're not interested in
	 * them (yet!)
	 */

} CpPreferences;

/*
 * end of CoPilot structures
 *
 **************************************************/

/**************************************************************************
 *
 * private functions
 *
 * Are predeclared here, to allow them to be placed into different
 * code sections
 *
 */

static Boolean DatabaseExists(const char *name) CPINTERFACE_SECTION;

static CpPreferences *GetPreferences(void) CPINTERFACE_SECTION;

static WaypointIDType GetLegWaypointID(CpLegWaypoint *wp) CPINTERFACE_SECTION;

static Int16 SurfaceSearchFunc(void const *searchData, void const *arrayData,
		Int32 other) CPINTERFACE_SECTION;

static Int16 RunwayCompareFunc(void *p1, void *pw, Int32 other)
	CPINTERFACE_SECTION;

//PRV:


/*
 * DatabaseExists
 * 
 */

static Boolean DatabaseExists(const char *name) {
	
	DmOpenRef db = DBOpen(name, true, false);
	Boolean exists = false;
	
	if (db) {
		
		exists = true;
		DBClose(db);
		
	}
	
	return exists;
	
}


/*
 * function : GetPreferences
 *
 * Returns a pointer to a newly allocated chunk of memory containing
 * the CoPilot preferences record.
 *
 * It is the caller's responsibility to free the memory chunk
 *
 */

static CpPreferences *GetPreferences(void){
	UInt16    size = 0;
	CpPreferences *ptr = NULL;
	DmOpenRef db;

	db = DBOpen(CoPilotPreferencesDB, true, false);
	ModErrThrowIf(!db);

	ptr = (CpPreferences*) DBRecordGet(db, 0, true);
	DBClose(db);

	return ptr;
}

/*
 * function : GetLegWaypointID
 *
 * Returns the ID of a CoPilot leg waypoint
 *
 */

static WaypointIDType GetLegWaypointID(CpLegWaypoint *wp) {

	if (wp->wpType == system) {

		/*
		 * external database has a links file which holds the unique record Ids
		 * of each system database record, so scan the links file (which is
		 * fast) to look for the unique id of the waypoint
		 *
		 */

		if (WDMIsExternal(WPDataset, FMDS.cpSystem)) {
			
			/*
			 * check the links file
			 *
			 */

			Int16 j = 0;

			if (!linksFile) return wpNotFound;

			while (j < numLinks) {

				if (linksFile[j] == wp->uniqueID)

					/*
					 * found it!!
					 *
					 */

					break;

				j++;

			}

			if (j == numLinks) return wpNotFound;

			return ( FMDS.cpSystem | j );

		} else {
			
			return WDMGetWaypointByUniqueID(WPDataset, FMDS.cpSystem, wp->uniqueID);

		}

	} else {

			/*
			 * get it from the user database
			 *
			 */

			return WDMGetWaypointByUniqueID(WPDataset, FMDS.cpUser, wp->uniqueID);

	}

	return wpNotFound;

}

/*
 * function : SurfaceSearchFunc
 *
 * Callback used by SysBinarySearch in CpGetRunwayInfo
 *
 * searchData is the surface type from the waypoint field,
 * arrayData is a pointer to one of the hard-coded hard-surface strings, which
 * comes from the hardSurfaces array.
 * 
 */

static Int16 SurfaceSearchFunc(void const *searchData, void const *arrayData, Int32 other) {

	return StrCompare((char*)searchData, *((char**)arrayData));
}
/*
 * function : RunwayCompareFunc
 *
 * Callback used by SysInsertionSort in CpGetRunwayInfo
 *
 */

static Int16 RunwayCompareFunc(void *p1, void *p2, Int32 other) {

	CpRunwayInfoType *r1 = p1;
	CpRunwayInfoType *r2 = p2;

	if ( r1->length < r2->length)
		return 1;
	else
		return -1;
}

/**************************************************************************
 *
 * Public functions
 *
 */

/*
 * function : CpInitialise
 *
 */

void CpInitialise(void){ 

	if (initialised)
		return;
	
	flightDb = DBOpen(CoPilotFlightDB, true, false);
	ModErrThrowIf(!flightDb);

	/*
	 * if using an external database, load the links file into memory
	 *
	 */

	if (WDMIsExternal(WPDataset, FMDS.cpSystem)) {
		
		/*
		 * Links file has a UInt32 checksum at the front !!
		 *
		 */

		PFFileRef extLinksFile;

#ifdef AEROPALM
		extLinksFile = PFOpenFile("/Palm/Programs/AeroPalm/AeroPalm_Links", pfFileReadOnly);
#else
		extLinksFile = PFOpenFile("/Palm/Programs/CoPilot/CoPilot_Links", pfFileReadOnly);
#endif

		if (extLinksFile) {

			numLinks = (PFFileSize(extLinksFile) - sizeof(UInt32))/sizeof(UInt32);
			PFSafeMalloc(linksFile, numLinks*sizeof(UInt32));

			PFSeekFile(extLinksFile, pfSeekStart, sizeof(UInt32));
			PFReadFile(extLinksFile, linksFile, numLinks*sizeof(UInt32));

			PFCloseFile(extLinksFile);

		} else {

			linksFile = NULL;

		}

	}

	initialised =true;

}

/*
 * function : CpClose
 *
 */

void CpClose(void){

	DBClose(flightDb);

	if (linksFile) {

		PFMemFree(linksFile);

	}

	initialised = false;

	return;

}

/*
 * function : CpIsIntalled
 *
 */

Boolean CpIsInstalled(void) {

	if (!DatabaseExists(CoPilotFlightDB))
		return false;

	if (!DatabaseExists(CoPilotPreferencesDB))
		return false;

	return true;
}

/*
 * function : CpGetFlightDescription
 *
 */

char *CpGetFlightDescription(UInt16 flightNum){ 

	FlightStruct *recPtr;
	static char desc[MAX_FLIGHT_DESCRIPTION];

	ModErrThrowIf(!initialised);

	recPtr = (FlightStruct*) DBRecordGet(flightDb, flightNum, false);

	if(!recPtr) {

		StrPrintF(desc,"PalmOS bug - %d", flightNum);
		return desc;
		
	}

	StrNCopy(desc, recPtr->description, MAX_FLIGHT_DESCRIPTION);
	DBRecordFree(recPtr);
	
	return desc;

}

/*
 * function : CpImportFlight
 *
 */

Boolean CpImportFlight(UInt16 flightNum, FlightPlanType fp, UInt16 segment, Boolean alternate) {

	UInt32        routeRec;
	FlightStruct *p;
	RouteStruct  *route;
	UInt16        j;
	Boolean       result = true;
	UInt16	      legNum;
	UInt16		  numLegsImported;
	UInt16        alternateLegStart;

	LOGENTRY;

	ModErrThrowIf(!initialised);

	/*
	 * get reference from flightNum to route database 
	 *
	 */

	p = (FlightStruct*) DBRecordGet(flightDb, flightNum, false);
	ModErrThrowIf(!p);

	routeRec = p->route;
	DBRecordFree(p);

	/* 
	 * get the route record, work out number of waypoints 
	 *
	 * (using j as the index)
	 *
	 */
	
	LOGTAG("Looking for flight record number:");
	j = DBRecordGetIndexByPalmID(flightDb, routeRec);
	ModErrThrowIf(j == DBRecordNotFound);
	
	LOGTAG("Flight record number:");
	LOGINT16(j);
	route = DBRecordGet(flightDb, j, false);
	ModErrThrowIf(!route);

	if (route->numLegs == 0) {

		DBRecordFree(route);
		return false;

	}

	/*
	 * Notes on the importing process:
	 *
	 * CoPilot uses RouteStruct.numLegs to indicate the index of the last
	 * leg, not the total number of legs - THEY MAY BE DIFFERENT!! (if the
	 * user has left blank legs for stop-overs)
	 *
	 */

	/*
	 * initialise, count segments 
	 *
	 */

	FpInit(fp);

	loadedSegment = segment;
	alternateLoaded = alternate;
	numSegments = 1;
	planID = flightNum;

	for (j=0;j<route->numLegs;j++) {

		if (route->leg[j].newSegment) numSegments++;

	}

	LOGINT16(numSegments);

	ModErrThrowIf(segment >= numSegments);

	/*
	 * find the requested segment, this leaves legNum pointing at the leg
	 * *after* the segment start - the leg at the start of a segment isn't
	 * of interest to us (see CoPilot if you don't believe me ;-)
	 *
	 */

	legNum = 0;
	while (segment) {

		/*
		 * look for start of next segment
		 *
		 */

		while (++legNum < route->numLegs
			&& !route->leg[legNum-1].newSegment);

		ModErrThrowIf(legNum == route->numLegs);

		segment--;

	}

	/*
	 * determine if an alternate is available in this
	 * segment
	 *
	 */

	alternateAvailable = false;
	for (j=legNum; j<route->numLegs; j++) {

		if (route->leg[j].newSegment) {
			
			/*
			 * new segment, no alternate available
			 *
			 */

			break;

		}

		if (route->leg[j].groundTime == alternateTime) {

			alternateAvailable = true;
			alternateLegStart = j+1;
			break;

		}

	}

	/*
	 * if alternate route required, skip to start of it
	 *
	 */

	if (alternate) {
		
		if (alternateAvailable) 
			legNum = alternateLegStart;
		else
			ModErrThrowIf(1);
		
	}

	/*
	 * deal with case where last segment is empty, or doesn't have a
	 * starting waypoint
	 *
	 */

	if (legNum >= route->numLegs || !route->leg[legNum].waypoint[0].uniqueID) {

		/*
		 * return, with an empty flight plan
		 *
		 */

		DBRecordFree(route);

		LOGEXIT;
		return true;

	}

	/*
	 * import waypoints until start of another segment or until start of an
	 * alternate route
	 *
	 */

	LOGLINE;

	/*
	 * import until :
	 * 	run out of legs
	 * 	invalid end waypoint of leg
	 * 	start of another segment
	 * 	start of an alternate route
	 *
	 */

	LOGLINE;
	result = true;

	numLegsImported = 0;
	while (legNum < route->numLegs 
		&& route->leg[legNum].waypoint[1].uniqueID
		&& !route->leg[legNum].newSegment
		&& route->leg[legNum].groundTime != alternateTime) {

		WaypointIDType wpID1, wpID2;
		Waypoint *wp2;

		LOGLINE;

		wpID1 = GetLegWaypointID(&(route->leg[legNum].waypoint[0]));
		wpID2 = GetLegWaypointID(&(route->leg[legNum].waypoint[1]));

		if (wpID1 == wpNotFound || wpID2 == wpNotFound) {
			
			result = false;
			break;

		}

		wp2 = WDMGetWaypoint(WPDataset, wpID2);

		ModErrThrowIf(!wp2);

		if (numLegsImported == 0) {

			Waypoint *wp1 = WDMGetWaypoint(WPDataset, wpID1);

			ModErrThrowIf(!wp1);

			FpAppendWaypoint(fp, RAD_TO_INT32(wp1->latitude), RAD_TO_INT32(wp1->longitude), wp1->ident, wp1->magVar,0.0);
			PFMemFree(wp1);

		}

		FpAppendWaypoint(fp, RAD_TO_INT32(wp2->latitude), RAD_TO_INT32(wp2->longitude), wp2->ident, 
				wp2->magVar, (float)(route->leg[legNum].altitude));

		PFMemFree(wp2);

		numLegsImported++;
		legNum++;

	}

	DBRecordFree(route);

	LOGEXIT;

	return result;
}

/*
 * function : CpGetActiveFlightNum
 *
 */

UInt16 CpGetActiveFlightNum(void){
	CpPreferences *ptr;
	UInt16 result;

	ModErrThrowIf(!initialised);

	ptr = GetPreferences();
	result = ptr->currentFlightIndex;
	PFMemFree(ptr);
	return result;
}

/*
 * function : CpGetUnitPreferences
 */

void CpGetUnitPreferences(DisplayUnitsType *du){
	CpPreferences *ptr;

	ModErrThrowIf(!initialised);

	ptr = GetPreferences();

	du->distance = (UInt16)(ptr->distanceUnits);
	du->altitude = (UInt16)(ptr->altitudeUnits); 
	du->speed = (UInt16)(ptr->speedUnits); 
	PFMemFree(ptr);
}

/*
 * function : CpGetValidFlights
 *
 */

#define RESULTSIZE 100
void CpGetValidFlights(UInt16 maxResults, UInt16 *total, UInt16 *resultPtr) {

	UInt16        j;
	FlightStruct *p;
	UInt32        routeRef;
	Err           err       = 0;
	UInt16        index;
	
	LOGENTRY;

	*total = 0;

	if (!DBGetNumRecords(flightDb)) {

		return;

	}

	j = 0;
	do {
		LOGINT16(j);
		p = (FlightStruct*)DBRecordGet(flightDb,j, false);
//		index = MemHandleSize(h);

		if ( p->recordType != flightRecord) {

			DBRecordFree(p);
			LOGTAG("Hit end of records");
			break;

		}

		if ( p->description[0] == 0) {
			err = 1;
		} else {
			routeRef = p->route;
			LOGTAG("Locating route record from reference:");
			LOGINT32(routeRef);
			index = DBRecordGetIndexByPalmID(flightDb,routeRef);
		}

		DBRecordFree(p);

		if (index != DBRecordNotFound) {
			
			RouteStruct *r;

			LOGTAG("Querying route record");
			LOGINT32(err);
			LOGINT16(index);

			r = (RouteStruct*)DBRecordGet(flightDb, index, false);
			resultPtr[(*total)++] = j;
			DBRecordFree(r);
			
		}

		j++;

	} while ( j < maxResults );

	LOGINT16(*total);
	LOGEXIT;

}

/*
 * function : CpLaunchCoPilot
 */

void CpLaunchCoPilot(void) {
	DmOpenRef db = DmOpenDatabaseByTypeCreator(AppType, CoPilotCreatorID, 
			dmModeReadOnly);
	UInt16 card;
	LocalID lid;

	ModErrThrowIf(!db);
	DmOpenDatabaseInfo(db, &lid, NULL,NULL,&card,NULL);
	DmCloseDatabase(db);
	SysUIAppSwitch(card,lid,0,NULL);

}


/*
 * function : CpGetRunwayInfo
 *
 */

CpRunwayInfoType *CpGetRunwayInfo(const char *notes, Int16 *count) {

	const char        rwyMarker[]   = "Runways:";
	UInt8             heading;
	UInt16            length;
	UInt16            width;
	Boolean           hardSurface;
	CpRunwayInfoType *ptr         = NULL;
	Boolean           failed      = false;
	const char       *xMarker;
	Int32             position;
	UInt16            numRunways  = 0;
	UInt8             j;
	char              tmp[20];
	const UInt16      maxRunways  = 10;

	/*
	 * notes record looks like this:
	 *
	 *  Runways:\n
	 *  Runway   LxW       Surface\n
	 *  05/23    6437x148  ASPHALT\n
	 *  09L/27R  12742x164 GRASS\n
	 *
	 * The list of runways is terminated by a single \n on its own line.
	 *
	 * Whitespace is not important
	 *
	 */

	LOGENTRY;
	*count = 0;

	if (!notes) return NULL;

	notes = StrStr(notes, rwyMarker);
	if (!notes) return NULL;

	SKIP_LINE(notes);
	if (!notes) return NULL;
	
	/*
	 * point notes to start of runway line
	 *
	 */

	SKIP_LINE(notes);
	if (!notes) return NULL;

	ptr = PFMalloc(sizeof(CpRunwayInfoType)*maxRunways);
	ModErrThrowIf(!ptr);

	while (notes && notes[0] != '\n' && numRunways<maxRunways) {

		/*
		 * extract the heading and the length of the runway
		 *
		 */

		heading = StrAToI(notes);
		if (heading <1 || heading >36) {
			SKIP_LINE(notes);
			continue;
		}
		if (heading > 18) 
			heading -= 18;

		/*
		 * move pointer to runway dimensions...
		 *
		 */

		SKIP_TO_WHITESPACE(notes);
		if (!notes) {
			failed = true;
			break;
		}
		SKIP_WHITESPACE(notes);

		if (!notes) {
			failed = true;
			break;
		}

		length = StrAToI(notes);
		if (length<1 || length>65500) {
			failed = true;
			break;
		}

		/*
		 * locate the 'x' dividing the runway dimensions, and
		 * extract the width
		 *
		 */

		xMarker = StrChr(notes,'x');
		if (!xMarker) {
			failed = true;
			break;
		}

		width = StrAToI(xMarker+1);
		if (width <1 || width > 999) {
			failed = true;
			break;
		}

		/*
		 * move pointer to runway surface...
		 *
		 */

		SKIP_TO_WHITESPACE(notes);
		SKIP_WHITESPACE(notes);
		if (!notes) {
			failed = true;
			break;
		}

		/*
		 * determine what sort of surface the runway has - hard or soft
		 *
		 * point notes at the surface string, copy the surface string from
		 * there to the tmp variable so we can zero-terminate it. Then invoke
		 * the system binary search to look for the surface in the list
		 * of known hard surfaces.
		 *
		 */

		for (j=0;notes[j] > 32 && j<sizeof(tmp);j++) {
			tmp[j] = notes[j];
		}
		tmp[j] = 0;

		hardSurface=SysBinarySearch((void*)hardSurfaces, numHardSurfaces,
				sizeof(char*), SurfaceSearchFunc, 
				(void*)&tmp, 0, &position, false);

		/*
		 * store the information about the runway
		 *
		 */

		ptr[numRunways].heading = heading;
		ptr[numRunways].length  = length;
		ptr[numRunways].width   = width;
		ptr[numRunways].hardSurface = hardSurface;
		numRunways++;

		SKIP_LINE(notes);
	}

	if (failed && !numRunways) {
		PFMemFree(ptr);
		return NULL;
	}

	if (numRunways) {

		PFMallocResize(ptr, numRunways * sizeof(CpRunwayInfoType));
		SysInsertionSort((void*)ptr, numRunways, sizeof(CpRunwayInfoType), RunwayCompareFunc, 0);

	} else {

		PFMemFree(ptr);

		ptr = NULL;

	}

	*count = numRunways;

	LOGEXIT;
	return ptr;
}

/*
 * function : CpGetNavFrequency
 *
 */

char *CpGetNavFrequency(const Waypoint *wp) {

	const char  freqText[]   = "Frequency:";
	const char *freq;
	const char *eol;
	char       *result;

	freq = GetStringFromList(wp->ident,2);
	if (!freq) return NULL;

	freq = StrStr(freq,freqText);
	if (!freq) return NULL;

	freq += StrLen(freqText);

	SKIP_WHITESPACE(freq);

	eol = StrChr(freq,'\n');
	if (!eol) return NULL;
	
	result = PFMalloc((eol-freq)+1);

	StrNCopy(result, freq, (eol-freq));
	result[eol-freq] = 0;

	return result;
}

/*
 * function : CpGetNavFrequencies
 *
 */

const char *CpGetAirportFrequencies(const Waypoint *wp) {

	const char freq[] = "Frequencies:";
	const char *ptr = GetStringFromList(wp->notes,2);

	if (!ptr) return NULL;

	ptr = StrStr(ptr, freq);

	if (!ptr) return NULL;

	SKIP_LINE(ptr);
	SKIP_LINE(ptr);

	if (!ptr) return NULL;

	return ptr;

}


/*
 * Simple stuff....
 *
 */

Int16 CpGetNumSegments(void) { return numSegments; }
Int16 CpGetSegmentNum(void) { return loadedSegment; }
UInt16 CpGetPlanID(void) { return planID; }
Boolean CpIsAlternateAvailable(void) { return alternateAvailable; }
Boolean CpIsAlternateLoaded(void) { return alternateLoaded; }

/*
 * function : GetObstacleAltitude
 *
 * Returns altitude of obstacle waypoint, in feet
 *
 */

Int32 GetObstacleAltitude(const Waypoint *wp) {

	const char *notes = GetStringFromList(wp->ident, 2);
	const char alt[] = "Alt:";
	
	notes = StrStr(notes, alt);

	if (!notes) return -1;

	notes += 5;

	LOGSTR(wp->ident);
	LOGSTR(notes);

	LOGINT32(StrAToI(notes));

	return (Int32)StrAToI(notes);

}

