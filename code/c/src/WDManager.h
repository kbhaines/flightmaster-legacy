/*
 * Waypoint Dataset Manager (WDM)
 *
 * (c) 2005 Blackhawk Systems Ltd
 *
 */

#ifndef WDM_H_INCLUDED
#define WDM_H_INCLUDED

#include "Platform.h"
#include "Constants.h"

#define wdmNotFound 0
#define wdmAll      0xFFFF0000
#define wdmTempDB   0x80000000
#define wdmSysTempDB 0x40000000


/*
 * WaypointIDType is formed by OR'ing the WDMDatabaseIDType with the record
 * number of the waypoint to give a unique 32 bit value for the waypoint
 *
 */

typedef UInt32 WaypointIDType;

#define wpNotFound 0

/* 
 * Waypoint types
 *
 * The way these are defined allows them to be or'd together to produce
 * sets/masks etc.
 *
 */

typedef enum {

	wpAirfield = 0x10,

	/*
	 * airfield sub types
	 *
	 * (subtypes 0-9 are the airfield runway icons)
	 *
	 */

	wpDisused 	= 0x001A,
	wpMicrolight	= 0x001B,
	wpGlider  	= 0x001C,

	wpLargeAirfield= 0x20,

	wpVOR      = 0x40,
	wpVORDME   = 0x41,
	wpDME      = 0x42,
	wpTACAN    = 0x43,
	wpVORTAC   = 0x43,

	wpNDB      = 0x80,
	wpNDBDME   = 0x81,

	wpIntersection = 0x100,

	wpAnyObstacle = 0x4000,

	wpObstacle = 0x4001,
	wpObstacles= 0x4002,
	wpLightObstacle=0x4003,
	wpLightObstacles = 0x4004,

	wpOther        = 0x8000,

	/*
	 * wpOther sub types
	 *
	 */

	wpTown         = 0x8001,
	wpVRP          = 0x8002,
	wpHangGlider   = 0x8003,
	wpParachute    = 0x8004,
	wpMarker       = 0x8005,
	wpRouteMark	   = 0x8006,

	wpAllAirfields = (wpAirfield | wpLargeAirfield),

	wpAny      = 0xFFF0,

	wpClassMask = 0xFFF0, 
	wpSubClassMask = 0xF

} WaypointClassType;
	
typedef struct {

	/*
	 * the astute among you will notice that this is the CoPilot
	 * waypoint format ;-)
	 *
	 */

	double latitude;  /* north +ve */
	double longitude; /* west +ve */
	float  magVar;    /* west +ve */
	float  elevation; /* feet */
	char   ident[1];
	char   name[1];
	char   notes[1];

} Waypoint_v4_0;

typedef Waypoint_v4_0 Waypoint;

/*
 * a short waypoint corresponds to the data held in the index - therefore a
 * request for such a waypoint can be satisfied much quicker.
 *
 */

typedef struct {

	Int32 lat;
	Int32 lon;

	WaypointIDType wpId;		// use to get 'full' waypoint
	WaypointClassType type;

	/*
	 * 5 bytes of type-specific content.
	 *
	 * Most waypoints: up to 5 chars of the ident
	 *
	 * Obstacles: [1-2] = altitude in feet (MSB LSB)
	 *
	 */

	UInt8 extra[5];

} ShortWaypointType;


typedef struct WDMHandleStructType * WDMHandle;
typedef struct WDMSearchStructType * WDMSearchHandle;

typedef UInt32 WDMDatabaseIDType;


/*******************************************************************************
 *
 * public functions
 *
 */

/*
 * WDMNew
 *
 * Create a new dataset
 *
 */

extern WDMHandle WDMNew(void) ADT3_SECTION;

/*
 * WDMFree
 *
 * Closes the dataset
 *
 * Call when finished working with the specified dataset
 *
 */

extern void WDMFree(WDMHandle ds) ADT3_SECTION;

/*
 * WDMAddDB
 *
 * Add specified database to the dataset. Also opens the database if
 * successful.
 *
 * If externalName is not null then it checks (and uses) the external database
 * if it exists.
 *
 * compactDBName is used to check for a compact dataset on the SD card.
 *
 * Otherwise it uses the internal database.
 *
 * Returns wdmNotFound if the data was not found, otherwise returns the unique
 * Id of the dataset.
 *
 */

extern WDMDatabaseIDType WDMAddDB(WDMHandle ds, const char *dbname, const char *externalName, 
		const char *compactDBName, Int16 (*updateFunc)(Int16)) ADT3_SECTION;

/*
 * WDMIsExternal
 *
 * Returns true if the database is located in external storage
 *
 */

extern Boolean WDMIsExternal (WDMHandle ds, WDMDatabaseIDType dbid) ADT3_SECTION;

/*
 * WDMMoveExternal
 *
 * Moves the specified database to external storage
 *
 * Returns true if the operation succeeded
 *
 */

extern Boolean WDMMoveExternal(WDMHandle ds, WDMDatabaseIDType dbid) 
	ADT3_SECTION;

/*
 * WDMMoveInternal
 *
 * Moves the specified database to internal storage
 *
 * Returns true if the operation succeeded
 *
 */

extern Boolean WDMMoveInternal(WDMHandle ds, WDMDatabaseIDType dbid) 
	ADT3_SECTION;

/*
 * WDMIndexOK
 *
 * Returns true if the indices of the dataset are up to date
 *
 */

extern Boolean WDMIndexOK(WDMHandle ds) ADT3_SECTION;

/*
 * WDMUpdateIndex
 *
 * Updates the indices of the dataset. Accepts
 * a pointer to a callback function to allow a progress
 * indicator to be displayed.
 *
 */

extern Boolean WDMUpdateIndex(WDMHandle ds, Int16 (*f)(Int16) ) ADT3_SECTION;


/*
 * WDMScanArea
 *
 * Scans the specified area for waypoints from the specified databases
 *
 * 'dbids' is the subset of databases to scan. It is constructed by OR'ing
 * together the results returned by calling WDMAddDB, or use wdmAll to scan
 * the entire dataset.
 *
 * Stores a set of WaypointIDType in *wpID, up to a maximum of 'max'.
 *
 * Returns true if the scan was completed, false otherwise
 *
 * Only a single area-scan can be active at once
 *
 */

extern Boolean WDMScanArea(WDMHandle ds, WDMDatabaseIDType dbids, 
		Int16 lat1, Int16 lon1, Int16 lat2, Int16 lon2,
		WaypointClassType wpType,
		ShortWaypointType *wpList, Int16 *numIDs, Int16 max) ADT3_SECTION;

/*
 * WDMNewProxScan
 *
 * Creates and initialises a new proximity search, centred on the specified
 * datum location.
 *
 * The search looks in only the database specified in dbids (formed by OR'ing
 * results of values returned by WDMAddDB)
 *
 * Returns a handle to the proximity search, which is used in subsequent calls
 * to WDMGetProxWaypoint to perform the actual searching.
 *
 * Multiple simultaneous proximity searches are allowed.
 *
 */

extern WDMSearchHandle WDMInitProxScan(WDMHandle ds,
		Int32 lat, Int32 lon, 
		WDMDatabaseIDType dbsIds,
		WaypointClassType wpTypes) ADT3_SECTION;

/*
 * WDMFreeSearch
 *
 * Deletes the specified proximity search
 *
 */

extern void WDMFreeSearch(/*@out@*/ /*@only@*/ WDMSearchHandle sh) ADT3_SECTION;

/*
 * WDMGetOrCreateWaypoint
 *
 * Searches for a waypoint at the specified location and with the specified
 * ident, and returns the id in wpID
 *
 * If it can't find that waypoint then creates a new waypoint and returns it
 * 
 * In any event, returns a pointer to the waypoint (caller must free)
 *
 */

extern Waypoint *WDMGetOrCreateWaypoint(WDMHandle ds, char *ident, Int32 lat, Int32 lon, WaypointIDType *wpID) ADT3_SECTION;


/*
 * WDMGetProxWaypoint
 *
 * Returns the next waypoint found which is within the range specified
 * of the datum point for the proximity search. Range is in radians.
 *
 * Returns NULL if the search ended.
 *
 */

extern ShortWaypointType /*@shared@*/ *WDMGetProxWaypoint(WDMSearchHandle sh, Int32 maxRange)
	ADT3_SECTION;


/*
 * WDMGetWaypoint
 *
 * Retreives a copy of the specified waypoint
 *
 */

extern Waypoint *WDMGetWaypoint(WDMHandle ds, WaypointIDType wpID) 
	ADT3_SECTION;

/*
 * WDMGetShortWaypoint
 *
 * Retreives a copy of the specified short-version of the waypoint. This
 * data is retrievable from the index, and is much quicker than
 * WDMGetWaypoint.
 * 
 */

extern ShortWaypointType *WDMGetShortWaypoint(WDMHandle ds, WaypointIDType wpID)
	ADT3_SECTION;

/*
 * WDMGetWaypointFromDB
 *
 * Retrieves a copy of the specified waypoint from the specified database
 *
 */

extern Waypoint *WDMGetWaypointFromDB(WDMHandle ds, WDMDatabaseIDType db,
		UInt16 wpNum) ADT3_SECTION;

/*
 * WDMGetWaypointByUniqueID
 *
 * Helper function for importing CoPilot flights. Waypoint IDs are
 * stored in flight plans by unique system IDs, this function
 * returns a WaypointIDType for a given unique ID using
 * the DmFindRecordByID function from PalmOS
 *
 */

extern WaypointIDType WDMGetWaypointByUniqueID(WDMHandle ds, WDMDatabaseIDType db, UInt32 uniqueID) 
	ADT3_SECTION;
	
/*
 * WDMGetWaypointDatabase
 *
 * Returns the Database ID of the waypoint
 *
 */

extern WaypointIDType WDMGetWaypointDatabase(WaypointIDType wpID) ADT3_SECTION;


/*
 * WDMGetWaypointType
 *
 * Returns the type of the specified waypoint. This is determined
 * by looking into the 'Notes' field of the waypoint and
 * analysing the text "Type: <type>"
 *
 */

extern WaypointClassType WDMGetWaypointType(const Waypoint *wp) ADT3_SECTION;

/*
 * WDMSaveWaypoint
 *
 * Saves the waypoint into the specified database (and updates the index)
 *
 * Returns a waypoint id if successful, or wpNotFound otherwise
 *
 * NB The WDManager 'owns' the passed pointer from now on, and will free the
 * data upon the Dataset DS being freed.
 *
 */

extern WaypointIDType WDMSaveWaypoint(WDMHandle ds, WDMDatabaseIDType db, 
		const Waypoint *wp) ADT3_SECTION;

/*
 * WDMDeleteWaypoint
 *
 * Deletes the specified waypoint of the dataset (updates the index too)
 *
 */

extern Boolean WDMDeleteWaypoint(WDMHandle ds, WaypointIDType wpID) ADT3_SECTION;

/*
 * WDMPurgeDatabase
 *
 * Deletes all waypoints in the specified database (tempDBs only supported)
 *
 */

extern void WDMPurgeDatabase(WDMHandle ds, WDMDatabaseIDType db) ADT3_SECTION;

/*
 * WDMIntegrityCheck
 *
 * Checks the integrity of the indices for the dataset
 *
 */

extern void WDMIntegrityCheck(WDMHandle ds) ADT3_SECTION;

/*
 * WDMSaveDatabase
 *
 * Saves the specified database into the named file.
 * Only supports wdmTempDB
 *
 * Returns true if the operation succeeds
 *
 */

extern Boolean WDMSaveDatabase(WDMHandle ds, WDMDatabaseIDType dbid, const char *filename) ADT3_SECTION;

/*
 * WDMLoadDatabase
 *
 * Loads the named file into the specified database
 * Only supports wdmTempDB
 *
 * Returns true if the operation succeeds
 *
 */

extern Boolean WDMLoadDatabase(WDMHandle ds, WDMDatabaseIDType dbid, const char *filename) ADT3_SECTION;

/*
 * function : WDMNewWaypoint
 *
 */

extern Waypoint *WDMNewWaypoint(const char *ident, const char *name, Int32 lat, Int32 lon, float magVarn) ADT3_SECTION;

/*
 * function : WDMSearchForWaypointByLocation
 *
 * Scan the area up to 'range' from lat/lon, looking for a waypoint named
 * 'ident'.
 *
 * Set range = 0 to search entire database (i.e. an ident search)
 *
 * Returns the WaypointIDType of the waypoint if it finds one, or returns
 * wpNotFound if no waypoint exists there. Always tries to find the nearest
 * waypoint.
 *
 */

extern WaypointIDType WDMSearchForWaypointByLocation(WDMHandle ds, const char *ident, Int32 lat, Int32 lon, Int32 range) ADT3_SECTION;

#endif
