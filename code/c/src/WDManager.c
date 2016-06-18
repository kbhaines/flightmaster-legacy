/*
 * Waypoint Dataset Manager (WDM)
 *
 * (c) 2005 Blackhawk Systems Ltd
 *
 * NB set tabstop=4 for this file
 *
 */

#include "Platform.h"
#include "MathLib.h"
#include "WDManager.h"
#include "Modules.h"
#include "Utils.h"
#include "Constants.h"

#include "GlobalTypes.h"
#include "CpInterface.h"

extern char ErrExtra[];

extern const UserConversionType UC;

/*******************************************************************************
 *
 * data types
 *
 */

//TYP:

#define MAX_DBS 12
#define MAX_SYS_TEMP 128
#define MAX_TEMP 5

enum { idIdxRec = 0, latIdxRec = 1, lonIdxRec = 2, typeIdxRec = 3, latLonIdxRec = 4, 
		extra1IdxRec = 5, extra2IdxRec = 6, lastIdxRec = 7};

/*
 * Index pointers
 *
 * Pointers to the records in the index for a database
 *
 */

typedef struct {


	/*
	 * lat & lon, most significant 16 bits
	 *
	 */

	Int16 *lat;
	Int16 *lon;

	WaypointClassType *type;
	UInt16 *id;

	/*
	 * least significant 4 bits of lat & lon,
	 * and byte 1 of 'extra' data
	 *
	 */

	UInt16 *latlon;
	
	/*
	 * bytes 2-5 of extra data
	 *
	 */

	UInt16 *extra1;
	UInt16 *extra2;

} IndexPtrs;

/*
 * Database type. Every dataset comprises a set of these databases
 * 
 * A database has references to the internal/external file (external
 * is looked for first) and to the index for the database.
 *
 */

typedef struct {

	WDMDatabaseIDType id;

	Int16 numWaypoints;

	enum { compact, external, internal } source;

	/*
	 * only one of these is used, depending on
	 * 'dbSource'
	 *
	 */

	DmOpenRef intDB;
	PFFileRef extDB;

	IndexPtrs index;

	/*
	 * corresponding index file (always internal)
	 *
	 */

	DmOpenRef indexDB;

} DatabaseType;


/*
 * Dataset type
 *
 * A dataset is a set of databases and some temporary storage for waypoints
 * tempWaypoints
 *
 */

struct WDMHandleStructType {

	Int16 numDatabases;
	DatabaseType db[MAX_DBS];

	Int16 numTempWp;
	Int16 nextFreeTemp;
	Waypoint *tempWaypoints[MAX_TEMP];

	//Notes: ref/temp-waypoints.txt
	
};

		
/*
 * Search type
 *
 * Holds stateful information about a proximity waypoint
 * search
 *
 */

struct WDMSearchStructType {

	WDMHandle ds;

	/*
	 * set of databases to scan
	 *
	 */

	WDMDatabaseIDType dbids;

	/*
	 * reference position, centre of scan
	 *
	 */

	Int16 lat;
	Int16 lon;

	double coslat;

	/*
	 * types of waypoints to include
	 *
	 */

	WaypointClassType wpType;

	/*
	 * number of which database is being scanned and a pointer to it
	 *
	 * The variables which follow are related to the scan state within this
	 * database.
	 *
	 */

	Int16 scanDB;
	DatabaseType *db;
	IndexPtrs index;

	/*
	 * Scan looks north and south alternately
	 *
	 */

	enum {scanNorth, scanSouth} subScanState;

	/*
	 *
	 * Scanning limits
	 *
	 * maximum longitude range allowed, and last max range specified
	 * in call to WDMGetProxWaypoint
	 *
	 */

	Int16 maxLonRange;
	Int32 lastMaxRange;
	Int16 minLat, maxLat;

	/*
	 * pointers into the index and boolean control values to see if we've
	 * finished looking one direction or the other.
	 *
	 * Int32 used because of the parameter requirement to MyBinarySearch
	 *
	 */

	Int32 northIndex;
	Int32 southIndex;

	Boolean northDone;
	Boolean southDone;

	Boolean searchCompleted;

};

/*************************************************************************
 *
 * module variables
 *
 */

//VAR:

static const WaypointIDType    wdmDatabaseMask = 0xFFFF0000;
static const WaypointIDType    wdmWaypointMask = 0x0000FFFF;

static const UInt32 fileMagic = 0x64115566;		// serial number for temp waypoints

/******************************************************************************
 *
 * private functions
 *
 */

#define ModuleID WDMModuleID


static Int16 IDToDBNumber(WDMDatabaseIDType id) ADT_SECTION;
static Boolean CheckIndex(DatabaseType *db) ADT_SECTION;
static Boolean CreateIndex(WDMHandle ds, DatabaseType *db, Int16 (*f)(Int16)) ADT_SECTION;
static Int16 LatitudeCompareFunc(void const *lat1, void const *lat2, Int32
		other) ADT_SECTION;
static void StoreIndexRecord(DmOpenRef db, void *recordToStore, UInt16 size)
	ADT_SECTION;
static /*@shared@*/ const char *GetAirportFrequencies(const Waypoint *wp) ADT_SECTION;
static void InitProxDBSearch(WDMSearchHandle sh) ADT_SECTION;

//PRV:


/*
 * IDToDBNumber
 *
 * Convert from a database ID to a database number
 *
 */

static Int16 IDToDBNumber(WDMDatabaseIDType id) {

	Int16 result = 0;

	ModErrThrowIf(!id);

	while (! (id & 0x10000)) {
		
		id >>= 1;
		result ++;

	}

	return result;

}

/*
 * CheckIndex
 *
 * Returns true if the index of the database is OK (no update required)
 *
 */

static Boolean CheckIndex(DatabaseType *db) {

	DmOpenRef indexDB  = db->indexDB;
	UInt32    crDate;
	UInt32    modDate;
	UInt32    indexTimeStamp;
	UInt16    numIndexRecs;

	LOGENTRY;

	if (DBGetNumRecords(indexDB) < lastIdxRec ) {

		LOGEXIT;
		return false;

	}

	if (db->source == compact) {

		LOGEXIT;
		return true;

	}

	/*
	 * get the timestamps for the index and database
	 *
	 */

	DBGetDates(indexDB, NULL, &indexTimeStamp);

	if (db->source == external) {

		DBFGetDates(db->extDB, &crDate, &modDate);

	} else if (db->source == internal) {

		DBGetDates(db->intDB, &crDate, &modDate);

	} else {

		ModErrThrow();

	}

	numIndexRecs = DBRecordGetSize(indexDB,extra2IdxRec)/sizeof(UInt16);

	LOGUINT32(crDate);
	LOGUINT32(modDate);
	LOGINT16(db->numWaypoints);

	LOGUINT32(indexTimeStamp);
	LOGINT16(numIndexRecs);

	LOGEXIT;

	return (db->numWaypoints == numIndexRecs && crDate <= indexTimeStamp 
			&& modDate <= indexTimeStamp);

}

/*
 * CreateIndex
 *
 * Create an index for the specified database
 *
 * Returns true if successful
 *
 */

static Boolean CreateIndex(WDMHandle ds, DatabaseType *db, Int16 (*f)(Int16)) {

	UInt16     indexSize = 0; 
	UInt16     j;

	UInt16    *idxId;
	Int16     *idxLatitude;
	Int16     *idxLongitude;
	WaypointClassType *idxType;

	UInt16 *idxLatlon;
	UInt16 *idxExtra1;
	UInt16 *idxExtra2;

	Int16      numWaypoints = db->numWaypoints;

	DmOpenRef    idx= db->indexDB;

	LOGENTRY;

	/*
	 * indexDB comprises N records, delete them if they
	 * already exist
	 *
	 */

	if (DBGetNumRecords(idx) > 1 ) {

		LOGTAG("Deleting");
		while (DBGetNumRecords(idx)) {
			
			DBRecordDelete(idx, 0);

		}

	/*@i1@*/ } // ignore memory leak detection from above

	/*
	 * the index records are constructed in main memory, then
	 * copied to the index database
	 *
	 */

	if (numWaypoints == 0) {
			
		LOGEXIT;
		return true;
		
		idxId = (UInt16 *) PFMalloc(1);
		idxLatitude=(Int16*)PFMalloc(1);
		idxLongitude=(Int16*)PFMalloc(1);
		idxType = (WaypointClassType*) PFMalloc(1);
		idxLatlon = (UInt16*)PFMalloc(1);
		idxExtra1 = (UInt16*)PFMalloc(1);
		idxExtra2 = (UInt16*)PFMalloc(1);

	} else {

		idxId = (UInt16 *) PFMalloc(numWaypoints*sizeof(UInt16));
		idxLatitude=(Int16*)PFMalloc(numWaypoints*sizeof(Int16));
		idxLongitude=(Int16*)PFMalloc(numWaypoints*sizeof(Int16));
		idxType = (WaypointClassType*) PFMalloc(numWaypoints*sizeof(WaypointClassType));
		idxLatlon =(UInt16*)PFMalloc(numWaypoints*sizeof(Int16));
		idxExtra1 =(UInt16*)PFMalloc(numWaypoints*sizeof(Int16));
		idxExtra2 =(UInt16*)PFMalloc(numWaypoints*sizeof(Int16));
	}

	ModErrThrowIf(!idxId);
	ModErrThrowIf(!idxLatitude);
	ModErrThrowIf(!idxLongitude);
	ModErrThrowIf(!idxType);
	ModErrThrowIf(!idxLatlon);
	ModErrThrowIf(!idxExtra1);
	ModErrThrowIf(!idxExtra2);

	/*
	 * iterate through the database, finding an insert position for 
	 * the current waypoint, j.
	 *
	 */

	if (f && numWaypoints == 0) f(0);

	LOGTAG("Scanning");

	for (j=0;j<numWaypoints;j++) {

		Waypoint *wp;
		Int32    insertPos;
		Int32    lat32, lon32;
		Int16    intLongitude;
		Int16    intLatitude;
		WaypointClassType wpType;
		UInt16  latlon;
		UInt16  extra1;
		UInt16  extra2;

		/*
		 * send an update to the callback function
		 *
		 */

		if ( (j & 127) == 0) {

			if (f) f(j);

		}

		/*
		 * get waypoint longitude, convert to Int16
		 *
		 */

		wp = WDMGetWaypointFromDB(ds, db->id, j);
		lat32 = RAD_TO_INT32(wp->latitude);
		lon32 = RAD_TO_INT32(wp->longitude);

		if (lat32 > 0) lat32 += 0x7FF; 
			else lat32 -= 0x7FF;
		if (lon32 > 0) lon32 += 0x7FF; 
			else lon32 -= 0x7FF;
			
		intLongitude = (lon32 & 0xFFFF0000) >> 16;
		intLatitude  = (lat32 & 0xFFFF0000) >> 16;

		/*
		 * put four more bits of lat & lon into top byte
		 * of latlon
		 *
		 */

		latlon = ((lat32 & 0xf000)) | ((lon32 & 0xf000)>>4);

		wpType = WDMGetWaypointType(wp);

		switch (wpType & wpClassMask) { 

		/*
		 * store Ident:
		 *
		 */

		case wpAnyObstacle:			
			extra1 = (Int16)GetObstacleAltitude(wp);
			extra2 = 0;
			break;

		default:
			latlon |= wp->ident[0];
			extra1 = wp->ident[1] * 256 + wp->ident[2];
			extra2 = wp->ident[3] * 256 + wp->ident[4];
			break;

		}

		PFMemFree(wp);

		if (j == 0) {

			/*
			 * first element, just store it and move on
			 *
			 */

			idxId[0] = j;
			idxLongitude[0]=intLongitude;
			idxLatitude[0]=intLatitude;
			idxType[0]=wpType;
			idxLatlon[0] = latlon;
			idxExtra1[0] = extra1;
			idxExtra2[0] = extra2;

			indexSize++;

			continue;

		}
		
		//LOGINT16(intLatitude);
		//LOGINT16(intLongitude);

		MyBinarySearch((void*)idxLatitude, indexSize,
				sizeof(Int16), LatitudeCompareFunc,
				&intLatitude, 0, &insertPos, false);

		//LOGINT32(insertPos);

		if (insertPos < indexSize) {

			/*
			 * make space for new entry
			 *
			 */

			PFMemMove(&idxId[insertPos+1],
					&idxId[insertPos],
					(indexSize - insertPos) *
					sizeof(UInt16));

			PFMemMove(&idxLatitude[insertPos+1],
					&idxLatitude[insertPos],
					(indexSize - insertPos) *
					sizeof(Int16));

			PFMemMove(&idxLongitude[insertPos+1],
					&idxLongitude[insertPos],
					(indexSize - insertPos) *
					sizeof(Int16));

			PFMemMove(&idxType[insertPos+1],
					&idxType[insertPos],
					(indexSize - insertPos) *
					sizeof(WaypointClassType));

			PFMemMove(&idxLatlon[insertPos+1],
					&idxLatlon[insertPos],
					(indexSize - insertPos) *
					sizeof(UInt16));

			PFMemMove(&idxExtra1[insertPos+1],
					&idxExtra1[insertPos],
					(indexSize - insertPos) *
					sizeof(UInt16));

			PFMemMove(&idxExtra2[insertPos+1],
					&idxExtra2[insertPos],
					(indexSize - insertPos) *
					sizeof(UInt16));

		}

		idxId[insertPos] = j;
		idxLongitude[insertPos]=intLongitude;
		idxLatitude[insertPos]=intLatitude;
		idxType[insertPos]=wpType;
		idxLatlon[insertPos] = latlon;
		idxExtra1[insertPos] = extra1;
		idxExtra2[insertPos] = extra2;

		indexSize++;

	}

	/*
	 * store the index records
	 *
	 */

	LOGTAG("Store index");

	indexSize *= sizeof(UInt16);
	
	StoreIndexRecord(idx, idxId, indexSize);	//idIdxRec
	PFMemFree(idxId);

	StoreIndexRecord(idx, idxLatitude, indexSize); 	// latIdxRec
	PFMemFree(idxLatitude);

	StoreIndexRecord(idx, idxLongitude, indexSize);	// lonIdxRec
	PFMemFree(idxLongitude);

	StoreIndexRecord(idx, idxType, indexSize);	// typeIdxRec
	PFMemFree(idxType);

	StoreIndexRecord(idx, idxLatlon, indexSize); // latLonIdxRec
	PFMemFree(idxLatlon);

	StoreIndexRecord(idx, idxExtra1, indexSize); // extra1IdxRec
	PFMemFree(idxExtra1);

	StoreIndexRecord(idx, idxExtra2, indexSize); // extra2IdxRec
	PFMemFree(idxExtra2);

	LOGEXIT;

	return true;

} 

/*
 * LatitudeCompareFunc
 *
 * This is a callback function used by the BinarySearch
 *
 * It receives pointers to two Int16 values, each of these is a latitude.  The
 * function returns 1 if lat1 is north of lat2, where north is +ve.
 *
 */

static Int16 LatitudeCompareFunc(void const *lat1, void const *lat2, 
		/*@unused@*/ Int32 other) {

	Int16    result = 0;

	/*
	 * North is positive, so the sense of the comparisons is reversed.
	 * Greater latitudes are further north, we want them earlier in the
	 * list
	 *
	 */

	//LOGINT16(*(Int16*)lat1);
	//LOGINT16(*(Int16*)lat2);

	if (*(Int16*)lat1 > *(Int16*)lat2 )
		result = -1;
	else if (*(Int16*)lat1 < *(Int16*)lat2)
		result = 1;

	return result;
}

/*
 * StoreIndexRecord
 *
 * Does exactly what it says on the tin ;-)
 *
 */

static void StoreIndexRecord(DmOpenRef db, void *recordToStore, UInt16 size) {

	UInt16 	  recNum = DBNewRecord;

	LOGENTRY;
	
	recNum = DBRecordCreate(db, recNum, recordToStore, size);
	ModErrThrowIf(recNum > lastIdxRec );
	
	LOGEXIT;

/*@i2@*/}  // suppress splint mem-leak detection

/*
 * GetAirportFrequencies
 *
 * Returns a pointer to the first line of the airport
 * frequencies which are contained in the notes of the
 * specified waypoint
 *
 */

static /*@shared@*/ const char *GetAirportFrequencies(const Waypoint *wp) {

	// splint complains that StrStr allocates fresh storage...

	/*@ignore@*/
	const char freq[] = "Frequencies:";
	const char *ptr = GetStringFromList(wp->notes,2);

	if (!ptr) return NULL;

	ptr = StrStr(ptr, freq);

	if (!ptr) return NULL;

	SKIP_LINE(ptr);
	SKIP_LINE(ptr);

	if (!ptr) return NULL;

	return ptr;
	/*@end@*/

}

/*
 * InitProxDBSearch
 *
 * Initialises the specified search handle to search
 * the current database of the dataset (as indicated by sh->scanDB)
 *
 */

static void InitProxDBSearch(WDMSearchHandle sh) {

	LOGENTRY;

	LOGINT16(sh->scanDB);

	sh->db = &sh->ds->db[sh->scanDB];
	sh->subScanState = scanNorth;
	sh->northDone    = false;
	sh->southDone    = false;

	sh->index = sh->db->index;

	/*
	 * find the waypoint at closest latitude
	 *
	 */

	MyBinarySearch((void*)sh->index.lat, sh->db->numWaypoints,
					sizeof(Int16), LatitudeCompareFunc, &sh->lat, 0,
					&sh->northIndex, false);
	
	sh->southIndex = sh->northIndex + 1;

	if (sh->southIndex >= sh->db->numWaypoints) sh->southDone = true;

	if (sh->northIndex >= sh->db->numWaypoints) sh->northIndex = sh->db->numWaypoints - 1;

	LOGINT16(sh->northIndex);
	LOGINT16(sh->southIndex);

	LOGEXIT;

}

/******************************************************************************
 *
 * public functions
 *
 */

//PUB:


/*
 * WDMNew
 *
 */

WDMHandle WDMNew(void) {

	Int16 j;
	WDMHandle wdm = NULL;
	
	PFSafeMalloc(wdm, sizeof(struct WDMHandleStructType));

	wdm->numDatabases = 0;

	for (j=0;j<MAX_TEMP;j++) wdm->tempWaypoints[j] = NULL;

	wdm->numTempWp = 0;
	wdm->nextFreeTemp = 0;

	return wdm;

}

/*
 * WDMFree
 *
 */

void WDMFree(WDMHandle this) {

	Int16 j;
	
	for  (j=0;j<this->numDatabases;j++) {

		DatabaseType *db = &this->db[j];
		
		/*
		 * free the index pointers
		 *
		 */

		DBRecordFree(db->index.lat);
		DBRecordFree(db->index.lon);
		DBRecordFree(db->index.type);
		DBRecordFree(db->index.id);
		DBRecordFree(db->index.latlon); 
		DBRecordFree(db->index.extra1);
		DBRecordFree(db->index.extra2);

		if (db->source == external) {
			
			PFCloseFile(db->extDB);
			DBClose(db->indexDB);

		} else if (db->source == internal) {
			
			DBClose(db->intDB);
			DBClose(db->indexDB);

		} else if (db->source == compact) {

//			/*@i1@*/MemHandleFree(MemPtrRecoverHandle(db->index.lat));
//			/*@i1@*/MemHandleFree(MemPtrRecoverHandle(db->index.lon));
//			/*@i1@*/MemHandleFree(MemPtrRecoverHandle(db->index.type));
//			/*@i1@*/MemHandleFree(MemPtrRecoverHandle(db->index.id));
//			/*@i1@*/MemHandleFree(MemPtrRecoverHandle(db->index.latlon)); 
//			/*@i1@*/MemHandleFree(MemPtrRecoverHandle(db->index.extra1));
//			/*@i1@*/MemHandleFree(MemPtrRecoverHandle(db->index.extra2));

		}

	}

	for (j=0;j<MAX_TEMP;j++) if (this->tempWaypoints[j]) PFMemFree(this->tempWaypoints[j]);

	PFMemFree(this);

}

/*
 * WDMAddDB
 *
 */

WDMDatabaseIDType WDMAddDB(WDMHandle this, const char *dbname, const char *externalName, const char *compactDBName,
		Int16 (*updateFunc)(Int16)) {

#ifdef AEROPALM

	const char *indexName[MAX_DBS] = {
		"AeroPalm-Index0",
		"AeroPalm-Index1",
		"AeroPalm-Index2",
		"AeroPalm-Index3",
		"AeroPalm-Index4",
		"AeroPalm-Index5",
		"AeroPalm-Index6",
		"AeroPalm-Index7",
		"AeroPalm-Index8",
		"AeroPalm-Index9",
		"AeroPalm-Index10",
		"AeroPalm-Index11" };

#else
	
	const char *indexName[MAX_DBS] = {
		"FlightMaster7a-Index0",
		"FlightMaster7a-Index1",
		"FlightMaster7a-Index2",
		"FlightMaster7a-Index3",
		"FlightMaster7a-Index4",
		"FlightMaster7a-Index5",
		"FlightMaster7a-Index6",
		"FlightMaster7a-Index7",
		"FlightMaster7a-Index8",
		"FlightMaster7a-Index9",
		"FlightMaster7a-Index10",
		"FlightMaster7a-Index11" };

#endif
	
	DatabaseType *db = &this->db[this->numDatabases];
	WDMDatabaseIDType newid  = 0x10000 << this->numDatabases;

	LOGENTRY;

	ModErrThrowIf(this->numDatabases == MAX_DBS);

	/*
	 * check for external databases first
	 *
	 */

	db->source = internal;	// changes if external or compact database is found

	/*
	 * check on an external card for FULL database
	 *
	 */

	if (externalName && (db->extDB = PFOpenFile(externalName, pfFileReadOnly))) {

		/*
		 * found database, find no. of waypoints
		 *
		 */

		db->numWaypoints = DBFGetNumRecords(db->extDB);
		db->source = external;

	}

	/*
	 * check on external card for COMPACT database
	 *
	 */

	if (db->source == internal && compactDBName && (db->extDB = PFOpenFile(compactDBName, pfFileReadOnly))) {

		/*
		 * load the index records from the file. 
		 *
		 */

		LOGTAG("Loading from Compact DB");
		LOGSTR(compactDBName);

		db->index.id = DBFRecordGet(db->extDB, idIdxRec, false);
		db->index.lat = DBFRecordGet(db->extDB, latIdxRec, false);
		db->index.lon = DBFRecordGet(db->extDB, lonIdxRec, false);
		db->index.type = DBFRecordGet(db->extDB, typeIdxRec, false);
		db->index.latlon = DBFRecordGet(db->extDB, latLonIdxRec, false);
		db->index.extra1 = DBFRecordGet(db->extDB, extra1IdxRec, false);
		db->index.extra2 = DBFRecordGet(db->extDB, extra2IdxRec, false);

		/*
		 * how many waypoints?
		 *
		 */

		LOGLINE;

		db->numWaypoints = PFMallocSize(db->index.type)/sizeof(UInt16);

		LOGINT16(db->numWaypoints);

		/*
		 * don't need the file anymore
		 *
		 */

		PFCloseFile(db->extDB);
		db->source = compact;

	}

	/*
	 * if not external or compact DB, check internally
	 *
	 */

	if (db->source == internal) {

		db->intDB = DBOpen(dbname, true, false);

		if (db->intDB) {
		
			db->source = internal;
			db->numWaypoints = DBGetNumRecords(db->intDB);

			if (!db->numWaypoints) {
				
				DBClose(db->intDB);
				return wdmNotFound;
				
			}
		} else {

			LOGEXIT;
			return wdmNotFound;
		}

	}

	/*
	 * if there is a database we've opened it. Now open the index for FULL databases
	 *
	 */

	db->id = newid;

	if (db->source != compact) {

		db->indexDB = DBOpen(indexName[this->numDatabases], false, true);

		ModErrThrowIf(!db->indexDB);

		if (!CheckIndex(db)) {
		
			CreateIndex(this, db, updateFunc);

		}

		db->index.lat = DBRecordGet(db->indexDB, latIdxRec, false);
		db->index.lon = DBRecordGet(db->indexDB, lonIdxRec,false);
		db->index.type= DBRecordGet(db->indexDB, typeIdxRec,false);
		db->index.id = DBRecordGet(db->indexDB, idIdxRec,false);

		db->index.latlon = DBRecordGet(db->indexDB, latLonIdxRec,false);
		db->index.extra1 = DBRecordGet(db->indexDB, extra1IdxRec,false);
		db->index.extra2 = DBRecordGet(db->indexDB, extra2IdxRec,false);

	}

	this->numDatabases++;

	LOGEXIT;

	return newid;

}

/*
 * WDMIsExternal
 *
 */

Boolean WDMIsExternal (WDMHandle this, WDMDatabaseIDType dbid) {

	Int16 dbnum = IDToDBNumber(dbid);

	ModErrThrowIf(dbnum >= this->numDatabases);

	return this->db[dbnum].source == external;

}

/*
 * WDMMoveExternal
 *
 */

Boolean WDMMoveExternal(WDMHandle this, WDMDatabaseIDType dbid) {

	return false;

}

	

/*
 * WDMMoveInternal
 *
 */

Boolean WDMMoveInternal(WDMHandle this, WDMDatabaseIDType dbid) {

	return false;

}

	

/*
 * WDMIndexOK
 *
 */

Boolean WDMIndexOK(WDMHandle this) {

	Int16     j;
	
	LOGENTRY;

	LOGEXIT;
	return true;

	for (j=0;j<this->numDatabases;j++) {

		if (!CheckIndex(&this->db[j]))  {

			LOGEXIT;
			return false;

		}

	}

	LOGEXIT;
	return true;

}

/*
 * WDMUpdateIndex
 *
 */

Boolean WDMUpdateIndex(WDMHandle this, Int16 (*f)(Int16) ) {

	Int16 j;

	LOGENTRY;

	for (j=0;j<this->numDatabases;j++) {

		DatabaseType *db= &this->db[j];

		LOGINT16(j);

		if (!CheckIndex(db)) CreateIndex(this, db, f);

	}

	LOGEXIT;
	return true;

}

/*
 * WDMScanArea
 *
 */

Boolean WDMScanArea(WDMHandle this, WDMDatabaseIDType dbids, 
		Int16 lat1, Int16 lon1, Int16 lat2, Int16 lon2,
		WaypointClassType wpType,
		ShortWaypointType *wpList, Int16 *numIDs, Int16 maxIDs) {

	Int16  j;

	LOGENTRY;
	LOGTIMESTART;

	LOGINT16(wpType);

	*numIDs = 0;

	/*
	 * scan temporary waypoints
	 *
	 */

	for (j = 0; *numIDs < maxIDs && j < MAX_TEMP; j++) {

		if (this->tempWaypoints[j]) {

			Int32 lat32 = RAD_TO_INT32(this->tempWaypoints[j]->latitude);
			Int32 lon32 = RAD_TO_INT32(this->tempWaypoints[j]->longitude);
			Int16 lat = lat32 / 65536;
			Int16 lon = lon32 / 65536;

			if (lat > lat1 || lat < lat2) continue;

			/*
			 * west is -ve in longitude
			 *
			 */

			if ( (lon1 < lon2 && (lon < lon1 || lon > lon2))
				||  (lon1 > lon2 && (lon < lon1 && lon > lon2))) continue;

			wpList->lat = lat32;
			wpList->lon = lon32;
			wpList->type = wpMarker;
			wpList->wpId = j | wdmTempDB;
			StrNCopy(wpList->extra, this->tempWaypoints[j]->ident,5);

			(*numIDs)++;
			wpList++;

		}

	}

	for (j=0; j<this->numDatabases;j++) {

		const DatabaseType *db = &this->db[j];

		/*
		 * pointers to the index records
		 *
		 */

		IndexPtrs idx;
		Int32  scanPos;

		LOGINT16(j);

		/*
		 * skip database if not included in 'dbids', or if its got no 
		 * records
		 *
		 */

		if ( ! (db->id & dbids) || db->numWaypoints == 0 ) {
				
				LOGTAG("Skipping");
				continue;

		}
		
		idx=db->index;

		LOGLINE;

		/*
		 * find the record at the north of the range rectangle
		 *
		 */

		MyBinarySearch((void*)idx.lat, db->numWaypoints, 
				sizeof(Int16),
				LatitudeCompareFunc, &lat1, 0, &scanPos, false);

		/*
		 * scan from north to south, terminate if we've filled maxIds
		 * or run out of waypoints
		 *
		 */

		LOGINT16(scanPos);

		while (*numIDs < maxIDs && 
			idx.lat[scanPos] > lat2 &&
			scanPos < db->numWaypoints) {

			if (wpType & idx.type[scanPos]) {

				/*
				 * check longitude limits. second block handles
				 * wrap-around case
				 *
				 */
				
				// TODO - update obstacle databases so this is unnecessary?
				Int16 idxlon = db->source == compact ? -idx.lon[scanPos] : idx.lon[scanPos];
				
				if ( (lon1 < lon2
					&& idxlon > lon1 
					&& idxlon < lon2) 
						
				||  (lon1 > lon2 
					&& (idxlon > lon1
					|| idxlon < lon2))
					) {

					/*
					 * put the 20bit lat/lon values back together from the index. 16 bits
					 * held in lat, 4 bits held in MSB of latlon parts.
					 *
					 */

					wpList->lat = ((Int32)idx.lat[scanPos] << 16) | (idx.latlon[scanPos] & 0xf000);
					wpList->lon = ((Int32)idx.lon[scanPos] << 16) | ((idx.latlon[scanPos] & 0x0f00)<< 4);
					
					// TODO - update obstacle databases so this is unnecessary?
					if (db->source == compact) wpList->lon = -(wpList->lon);

					wpList->type = idx.type[scanPos];

					/*
					 * if the database is a compact one, the lower part of the
					 * waypoint ID is NOT stored in the index field, it is
					 * simply the scan position.
					 *
					 */

					wpList->wpId = db->id | (db->source == compact?scanPos:idx.id[scanPos]);

					wpList->extra[0] = idx.latlon[scanPos] & 0xff;
					wpList->extra[1] = (idx.extra1[scanPos] / 256) &0xff;
					wpList->extra[2] = idx.extra1[scanPos] & 0xff;
					wpList->extra[3] = (idx.extra2[scanPos] / 256) &0xff;
					wpList->extra[4] = idx.extra2[scanPos] & 0xff;

					LOGINT32(wpList->type);
					(*numIDs)++;
					wpList++;

				} 

			}

			scanPos ++;

		}

	}

	LOGINT16(*numIDs);

	LOGTIMESTOP;

	LOGEXIT;

	return true;
}


/*
 * WDMInitProxScan
 *
 */

WDMSearchHandle WDMInitProxScan(WDMHandle this, Int32 lat, Int32 lon, 
		WDMDatabaseIDType dbsIds, WaypointClassType wpTypes) {

	WDMSearchHandle sh = NULL;
	Int16 j;

	LOGENTRY;

	/*
	 * find the first database that will be searched
	 *
	 */

	for (j=0; j<this->numDatabases; j++) 

			if ((dbsIds & this->db[j].id) && this->db[j].numWaypoints) 
				break;

	if (j == this->numDatabases) {
			
			LOGEXIT;
			return NULL;

	}
	
	PFSafeMalloc(sh, sizeof(struct WDMSearchStructType));

	/*@i1@*/sh->ds = this;
	sh->dbids = dbsIds;
	sh->scanDB = j;
	sh->lat = lat / 65536;
	sh->lon = lon / 65536; 
	sh->coslat=cos(INT32_TO_RAD(lat));
	sh->wpType= wpTypes;
	sh->maxLonRange = 0;
	sh->lastMaxRange = -1;
	sh->searchCompleted = false;

	InitProxDBSearch(sh);

	LOGEXIT;

	return sh;

}

/*
 * WDMFreeSearch
 *
 */

void WDMFreeSearch(WDMSearchHandle /*@out@*/ /*@only@*/ this) {

	/*@i10@*/PFMemFree(this);

}

/*
 * WDMGetProxWaypoint
 *
 */

ShortWaypointType *WDMGetProxWaypoint(WDMSearchHandle this, Int32 maxRange) {

	UInt16 wpIndex;
	Int32  lonRange;
	static ShortWaypointType swp;

	/*
	 * calculate new scan limits if the maxRange has changed
	 *
	 */

	//LOGENTRY;

	ModErrThrowIf(this->searchCompleted);

	if (this->lastMaxRange != maxRange) {

		if (maxRange == 0) {

			this->minLat = -16384;
			this->maxLat = 16384;
			this->lastMaxRange = this->maxLonRange = 0;

		} else {
		
			Int16  rng = maxRange / 65536;
		
			this->maxLonRange = (Int16)(RAD_TO_INT32(
						INT32_TO_RAD(maxRange) /
						this->coslat) / 65536);

			this->maxLonRange = MAX(1, this->maxLonRange);
			this->minLat = MAX(this->lat - rng, -16384);
			this->maxLat = MIN(this->lat + rng, 16384);

			this->lastMaxRange = maxRange;

		}

		LOGINT32(maxRange);
		LOGINT16(this->maxLonRange);
		LOGINT16(this->minLat);
		LOGINT16(this->maxLat);

	}

	/*
	 * dataset-wide scanning
	 *
	 */

	while (this->scanDB < this->ds->numDatabases) {

		/*
		 * database-wide scanning
		 *
		 * waypoint databases are scanned using the index which contains indexes of
		 * the records sorted by latitude.
		 *
		 * The while loop is continued at any point that the waypoint under
		 * consideration is rejected.
		 *
		 */

		while (!this->southDone || !this->northDone) {

			if (this->subScanState == scanSouth) {

				wpIndex = this->southIndex;
				this->southIndex++;

				if (this->southIndex == this->db->numWaypoints) 
					this->southDone = true;

				if (!this->northDone) this->subScanState = scanNorth;

				if (!( this->index.type[wpIndex] & this->wpType ) ) {

					continue;

				}
				
				if ( this->index.lat[wpIndex] < this->minLat) {

					/*
					 * end of southwards scan
					 * 
					 */

					this->southDone = true;
					continue;

				}

			} else {

				/*
				 * north scanning
				 *
				 */

				wpIndex = this->northIndex;
				if (this->northIndex == 0) this->northDone = true;

				this->northIndex--;

				if (!this->southDone) this->subScanState = scanSouth;
				
				if (!( this->index.type[wpIndex] & this->wpType ) ) {

					continue;

				}

				if (this->index.lat[wpIndex] > this->maxLat) {

					/*
					 * end of northwards scan
					 *
					 */	
					LOGTAG("Northdone");
					LOGINT16(wpIndex);
					LOGINT16(this->index.lat[wpIndex]);
					LOGINT16(this->maxLat);

					this->northDone = true;

					continue;

				}

			}

			if (this->maxLonRange) {

				Int32 wpLon = (Int32)this->index.lon[wpIndex];
				
				if (this->db->source == compact) wpLon =- wpLon;

				lonRange = wpLon - (Int32)this->lon;
				if (lonRange < 0) lonRange = -lonRange;
				WRAPMAX(lonRange, 32768);

				if ((Int16)lonRange > this->maxLonRange) {

					continue;

				}

			}

			/*
			 * found waypoint
			 *
			 */

			swp.lat = (Int32)this->index.lat[wpIndex] * 0x10000 + (this->index.latlon[wpIndex] & 0xf000);
			swp.lon = (Int32)this->index.lon[wpIndex] * 0x10000 + ((this->index.latlon[wpIndex] * 0x10) & 0xf000);
			
			// TODO - update obstacle databases so this is unecessary
			if (this->db->source == compact) swp.lon = -swp.lon;
			
			swp.type = this->index.type[wpIndex];
			swp.wpId = this->db->id | this->index.id[wpIndex];

			swp.extra[0] = this->index.latlon[wpIndex] & 0xff;
			swp.extra[1] = (this->index.extra1[wpIndex] / 256) &0xff;
			swp.extra[2] = this->index.extra1[wpIndex] & 0xff;
			swp.extra[3] = (this->index.extra2[wpIndex] / 256) &0xff;
			swp.extra[4] = this->index.extra2[wpIndex] & 0xff;
			//LOGEXIT;

			//return ( sh->db->id | sh->index.id[wpIndex] );

			return &swp;

		}

		/*
		 * if here, then we've finished scanning the database, and should look
		 * to see if another database can be scanned
		 *
		 * (looking for a database with waypoints, and whose Id matches one
		 * specified in sh->dbids)
		 *
		 */


		while (++this->scanDB < this->ds->numDatabases &&
			!(this->ds->db[this->scanDB].numWaypoints &&
				(this->ds->db[this->scanDB].id & this->dbids)));

		if (this->scanDB < this->ds->numDatabases) InitProxDBSearch(this);

		LOGINT16(this->scanDB);

	}

	this->searchCompleted = true;

	//LOGEXIT;

	return NULL;

}

/*
 * WDMGetWaypoint
 *
 */

Waypoint *WDMGetWaypoint(WDMHandle this, WaypointIDType wpID) {

#ifdef XXXX
	LOGENTRY;
	LOGINT32(wpID);
	LOGINT32(wpID & wdmDatabaseMask);
	LOGINT32(wpID & wdmWaypointMask);
	LOGEXIT;
#endif

	ModErrThrowIf(!wpID);
	return WDMGetWaypointFromDB(this, wpID & wdmDatabaseMask, wpID & wdmWaypointMask);

}

/*
 * WDMGetShortWaypoint
 *
 */

ShortWaypointType *WDMGetShortWaypoint(WDMHandle this, WaypointIDType wpID) {

	ModErrThrow();
	return NULL;

}

/*
 * WDMGetWaypointFromDB
 *
 */

Waypoint *WDMGetWaypointFromDB(WDMHandle this, WDMDatabaseIDType dbID,
		UInt16 wpNum) {

	Waypoint     *wp = NULL;
	DatabaseType *db;

	if (dbID & wdmTempDB) {

		/*
		 * temporary waypoint, check validity and make a copy
		 *
		 */

		Waypoint *srcWp;

		srcWp = this->tempWaypoints[wpNum];
		ModErrThrowIf(!srcWp);

		PFSafeMalloc(wp, PFMallocSize((void*)srcWp));
		PFMemMove(wp, srcWp, PFMallocSize((void*)srcWp));

		LOGSTR(wp->ident);

	} else {

		db = &this->db[IDToDBNumber(dbID)];
		
		if (db->source == external) {

//			MemHandle     h;

			wp = DBFRecordGet(db->extDB, wpNum, true);
			ModErrThrowIf(!wp);

//			PFSafeMalloc(wp,MemHandleSize(h));
//			/*@i1@*/PFMemMove(wp, MemHandleLock(h), MemHandleSize(h));
//			MemHandleUnlock(h);
//			MemHandleFree(h);

			/*
			 * Record format stores longitude with east = -ve, convert
			 * to our internal standard
			 * 
			 */
			
			wp->longitude = -wp->longitude;
			wp->magVar = -wp->magVar;

		} else if (db->source == internal) {
			
			wp = (Waypoint*)DBRecordGet(db->intDB, wpNum, true);
			ModErrThrowIf(!wp);

			/*
			 * Record format stores longitude with east = -ve, convert
			 * to our internal standard
			 * 
			 */

			wp->longitude = -wp->longitude;
			wp->magVar = -wp->magVar;
			
		} else {
			
			/*
			 *  compact database
			 *
			 *  All the data for the waypoint is derived from the index, there
			 *  is no corresponding database.
			 *
			 *  We therefore need to create a Waypoint from the data in the
			 *  index.
			 *
			 */

			ShortWaypointType swp;
			IndexPtrs idx;
			char ident[7], name[30];
			float altitude = 0.0;

			idx = db->index;
			
			swp.lat = (Int32)idx.lat[wpNum] * 0x10000 + (idx.latlon[wpNum] & 0xf000);
			swp.lon = (Int32)idx.lon[wpNum] * 0x10000 + ((idx.latlon[wpNum] * 0x10) & 0xf000);
			swp.lon = -swp.lon;
			
			// TODO - update obstacle database for new lon orientation
			swp.type = idx.type[wpNum];
			swp.wpId = db->id | idx.id[wpNum];

			swp.extra[0] = idx.latlon[wpNum] & 0xff;
			swp.extra[1] = (idx.extra1[wpNum] / 256) &0xff;
			swp.extra[2] = idx.extra1[wpNum] & 0xff;
			swp.extra[3] = (idx.extra2[wpNum] / 256) &0xff;
			swp.extra[4] = idx.extra2[wpNum] & 0xff;

			switch (swp.type & wpClassMask) {

			case wpAnyObstacle:
				
				/*
				 * set ident = obstacle altitude, stored in extra [1] & [2].
				 *
				 */
				
				altitude = swp.extra[1]*256+swp.extra[2];
				
				StrPrintF(ident, "%s", FloatToStr((UC.altitudeConv/100)*(altitude+50.0) ,0));

				switch (swp.type) {
				case wpObstacles: StrCopy(name, "Obstacles");break;
				case wpLightObstacle: StrCopy(name, "Obstacle(Lit)");break;
				case wpLightObstacles: StrCopy(name, "Obstacles(Lit)");break;
				default: StrCopy(name, "Obstacle");break;
				}

				break;

			default:
				StrNCopy(ident, swp.extra, 5);
				name[0] = 0;
				ident[5] = 0;
				break;

			}

			wp = WDMNewWaypoint(ident, name, swp.lat, swp.lon, 0.0);
			wp->elevation = altitude;
				
		}
	
	}

	return wp;

}

/*
 * WDMGetWaypointByUniqueID
 *
 */

WaypointIDType WDMGetWaypointByUniqueID(WDMHandle this, WDMDatabaseIDType db, UInt32 uniqueID)  {

		UInt16 index;
		Int16 dbNum = IDToDBNumber(db);

		LOGENTRY;

		LOGINT32(db);
		LOGINT16(dbNum);

		index = DBRecordGetIndexByPalmID(this->db[dbNum].intDB, uniqueID);
		
		if (index == DBRecordNotFound) {

			LOGEXIT;
			return wpNotFound;

		}

		LOGINT32(db | index);

		LOGEXIT;
		return db | index;

}

/*
 * WDMGetWaypointDatabase
 *
 */

WaypointIDType WDMGetWaypointDatabase(WaypointIDType wpID) {

	if (wpID == wpNotFound) wpID = 0;

	return wpID & wdmDatabaseMask;

}

/*
 * WDMGetWaypointType
 *
 */

WaypointClassType WDMGetWaypointType(const Waypoint *wp) {

	/*
	 * if the waypoint isn't one of these, then it is of class
	 * 'other'
	 *
	 */

	const char  TypeStr[]    = "Type:";
	const char  AirportStr[] = "AIRPORT";
	const char  VorDMEStr[]  = "VOR/DME";
	const char  VorStr[]     = "VOR";
	const char  TACANStr[]   = "TACAN";
	const char  NDBStr[]     = "NDB";
	const char  DMEStr[]     = "DME";
	const char  NDBDMEStr[]  = "NDB/DME";
	const char  VORTACStr[]  = "VORTAC";
	const char  VORTACANStr[]= "VOR/TACAN";
	const char  obstacleStr[]= "OBSTACLE";
	const char  obstaclesStr[]= "OBSTACLES";
	const char  lightObstacleStr[]= "OBSTACLE-L";
	const char  lightObstaclesStr[]= "OBSTACLES-L";
	const char  intersect1[] = "REP-PT";
	const char  intersect2[] = "RNAV-WP";
	const char  intersect3[] = "CNF";
	const char  intersect4[] = "WAYPOINT";
	const char  town[]       = "TOWN";
	const char  vrp[]        = "VRP";
	const char  micro[]      = "MICROLIGHT";
	const char  glider[]     = "GLIDER";
	const char  parachute[]  = "PARACHUTE";
	const char  disused[]  = "DISUSED";

	const char  *notesPtr;

	char typeStr[50];
	const char *eol;
	Int16 j;

	notesPtr = GetStringFromList(wp->ident,2);

	if (notesPtr[0]==0 || StrNCompare(notesPtr,TypeStr,sizeof(TypeStr)-1)!=0 )

		return wpOther;

	notesPtr += StrLen(TypeStr);
	
	SKIP_WHITESPACE(notesPtr);

	if (!notesPtr) return wpOther;

	eol = notesPtr;
	SKIP_TO_WHITESPACE(eol);

	if (!eol) return wpOther;

	for (j=0; j < eol-notesPtr; j++) typeStr[j] = notesPtr[j];
	typeStr[j]=0;

	if (StrCompare(typeStr,AirportStr) == 0) {

		UInt16 count = 0;
		const char *f = GetAirportFrequencies(wp);
		WaypointClassType wpType = wpAirfield;
		CpRunwayInfoType *runways;
		Int16 orientation;
		UInt16 numRunways;

		if (f) { 

			while (f && f[0]!='\n') {

				count++;
				SKIP_LINE(f);

			}

			if (count > 2) wpType = wpLargeAirfield;

		}

		/*
		 * now figure out the subtype of the airfield:
		 *
		 * 0-7 are for depicting the longest runway, 8 is
		 * a hollow circle (no runway info)
		 *
		 */

		runways = CpGetRunwayInfo(GetStringFromList(wp->ident,2),
					&numRunways);

		if (runways) {

			/*
			 * 8 icons in 180 degrees = 22.5 degrees per icon.
			 * Multiply everything by 4 to get integer maths
			 *
			 */

			orientation = (UInt8)
				(((runways[0].heading*10+5)*4 +
				  (UInt16)(wp->magVar*4*DEG_PER_RAD) + 45) /
				 90);

			if (orientation > 7) orientation -= 8;

			PFMemFree(runways);

		} else {

			orientation = 8;

		}

		return wpType | orientation;

	} else if (StrCompare(typeStr,VorDMEStr) == 0) {
		return wpVORDME;
	} else if (StrCompare(typeStr,VorStr) == 0) {
		return wpVOR;
	} else if (StrCompare(typeStr,TACANStr) == 0) {
		return wpTACAN;
	} else if (StrCompare(typeStr,NDBStr) == 0) {
		return wpNDB;
	} else if (StrCompare(typeStr,DMEStr) == 0) {
		return wpDME;
	} else if (StrCompare(typeStr,NDBDMEStr) == 0) {
		return wpNDBDME;
	} else if (StrCompare(typeStr,VORTACStr) == 0) {
		return wpVORTAC;
	} else if (StrCompare(typeStr,VORTACANStr) == 0) {
		return wpVORTAC;
	} else if (StrCompare(typeStr,obstacleStr) == 0) {
		return wpObstacle;
	} else if (StrCompare(typeStr,obstaclesStr) == 0) {
		return wpObstacles;
	} else if (StrCompare(typeStr,lightObstacleStr) == 0) {
		return wpLightObstacle;
	} else if (StrCompare(typeStr,lightObstaclesStr) == 0) {
		return wpLightObstacles;
	} else if (StrCompare(typeStr, intersect1) == 0
			|| StrCompare(typeStr, intersect2) == 0 
			|| StrCompare(typeStr, intersect3) == 0 
			|| StrCompare(typeStr, intersect4) == 0 ) {
		return wpIntersection;
	} else if (StrCompare(typeStr, town) ==0) {
		return wpTown;
	} else if (StrCompare(typeStr, vrp) == 0) {
		return wpVRP;
	} else if (StrCompare(typeStr, micro) == 0) {
		return wpMicrolight;
	} else if (StrCompare(typeStr, disused) == 0) {
		return wpDisused;
	} else if (StrCompare(typeStr, glider) == 0) {
		return wpGlider;
	} else if (StrCompare(typeStr, parachute) == 0) {
		return wpParachute;
	}

	return wpOther;

}


/*
 * WDMSaveWaypoint
 *
 */

WaypointIDType WDMSaveWaypoint(WDMHandle this, WDMDatabaseIDType db, 
		const Waypoint *wp) {

	Int16 j;

	LOGENTRY;

	if (db & wdmTempDB) {

	}  else {

		ModErrThrow();

	}

	/*
	 * j is where we'll store the waypoint, and we need
	 * to see if we're overwriting an existing (old) waypoint
	 * in order to make room
	 *
	 */

	j = this->nextFreeTemp;
	if (this->tempWaypoints[j]) {

		/*
		 * get rid of the existing waypoint - it's too old
		 * anyway ;-)
		 *
		 */

		PFMemFree(this->tempWaypoints[j]);

	}

	this->tempWaypoints[j] = (Waypoint*) wp;

	if (++this->nextFreeTemp == MAX_TEMP) this->nextFreeTemp = 0;

	this->numTempWp = MIN(this->numTempWp+1, MAX_TEMP);

	LOGEXIT;

	return db | j;

}


/*
 * WDMDeleteWaypoint
 *
 * Notes: ref/temp-waypoints.txt
 *
 */

Boolean WDMDeleteWaypoint(WDMHandle this, WaypointIDType wpID) {

	Int16 wpidx = wpID & wdmWaypointMask;
	Int16 j,k;
	Waypoint *newWp[MAX_TEMP];

	LOGENTRY;

	LOGINT32(wpID);

	ModErrThrowIf(!this->tempWaypoints[wpidx]);

	/*
	 * For details of the following operations, see the following reference:
	 *
	 * REF1: ref/temp-waypoints.txt 1
	 *
	 */

	for (j=0;j<MAX_TEMP;j++) newWp[j] = NULL;

	if (this->numTempWp == MAX_TEMP) {
		
		j = this->nextFreeTemp;

	} else {

		j = 0;

	}

	k = 0;
	do {

		/*
		 * don't copy the waypoint being deleted
		 *
		 */

		if (j != wpidx) newWp[k++] = this->tempWaypoints[j];
		if (++j == this->numTempWp) j = 0;
	   
	} while	(k < this->numTempWp - 1);

	for (j=0;j<MAX_TEMP;j++) this->tempWaypoints[j] = newWp[j];

	this->numTempWp--;
	this->nextFreeTemp = this->numTempWp;

	LOGEXIT;
	return true;

}

/*
 * WDMIntegrityCheck
 *
 * Checks the integrity of the indices for the dataset
 *
 */

void WDMIntegrityCheck(WDMHandle this) {

	Int16 j;

	for (j=0; j<this->numDatabases;j++) {

		Int16 i;
		const DatabaseType *db = &this->db[j];
		IndexPtrs idx;
		char s[32];
		Waypoint *wp;

		idx=db->index;

		/*
		 * simple waypoint count
		 *
		 */

		for (i=0; i < db->numWaypoints; i++) {

			StrPrintF(s, "1-%d-%d", j,i);
			PFDrawChars(s, StrLen(s), 0,0);
			wp = WDMGetWaypointFromDB(this, db->id, i);
			PFMemFree(wp);

		}

		/*
		 * now run through the index itself
		 *
		 */

		for (i=0; i < db->numWaypoints; i++) {

			StrPrintF(s,"2-%d-%d-%d", j, i, idx.id[i]);
			PFDrawChars(s, StrLen(s), 0,0);
			wp = WDMGetWaypointFromDB(this, db->id, idx.id[i]);
			PFMemFree(wp);

		}
			
	}

}

/*
 * WDMSaveDatabase
 *
 */

Boolean WDMSaveDatabase(WDMHandle this, WDMDatabaseIDType dbid, const char *filename) {

	PFFileRef file;
	Int16 j;

	ModErrThrowIf(dbid != wdmTempDB);

	file = PFOpenFile(filename, pfFileTruncate);

	if (!file) return false;

	/*
	 * store file magic, if this can't be read back then the temp waypoints file
	 * is void
	 *
	 */

	PFWriteFile(file, &fileMagic, sizeof(fileMagic));

	PFWriteFile(file, &this->nextFreeTemp, sizeof(this->nextFreeTemp));

	/*
	 * Save all waypoints, with a 4-byte header for each waypoint containing
	 * the size of the waypoint. Empty waypoints are stored with size=0, and no
	 * data body.
	 *
	 */

	for (j=0;j<MAX_TEMP;j++) {

		Int32 size;

		if (this->tempWaypoints[j]) {

			size = PFMallocSize((void*)this->tempWaypoints[j]);
			PFWriteFile(file, &size, sizeof(size));
			PFWriteFile(file, this->tempWaypoints[j], size);

		} else {

			/*
			 * empty record
			 *
			 */

			size = 0;
			PFWriteFile(file, &size, sizeof(size));

		}

	}

	PFCloseFile(file);

	return true;

}

/*
 * WDMLoadDatabase
 *
 */

extern Boolean WDMLoadDatabase(WDMHandle this, WDMDatabaseIDType dbid, const char *filename) {

	PFFileRef file;
	Int16 j;
	UInt32 magic;

	ModErrThrowIf(dbid != wdmTempDB);

	file = PFOpenFile(filename, pfFileReadOnly);
	if (!file) return false;

	/*
	 * check for magic
	 *
	 */

	PFReadFile(file, &magic, sizeof(magic));
	if (magic != fileMagic) {
		
		PFCloseFile(file);
		return false;

	}

	PFReadFile(file, &this->nextFreeTemp, sizeof(this->nextFreeTemp));

	for (j=0;j<MAX_TEMP;j++) {

		Int32 size;

		if (PFReadFile(file, &size, sizeof(size)) != sizeof(size)) {
			
			PFCloseFile(file);
			return false;

		}

		/*
		 * empty waypoint records are stored with zero size, and no
		 * body
		 *
		 */

		if (size) {

			PFSafeMalloc(this->tempWaypoints[j],size);

			if (PFReadFile(file, (void*)this->tempWaypoints[j], size) != size) {

				PFCloseFile(file);
				return false;

			}

			this->numTempWp++;

		}

	}

	PFCloseFile(file);
	return true;

}

/*
 * function : WDMNewWaypoint
 *
 */

Waypoint *WDMNewWaypoint(const char *ident, const char *name, Int32 lat, Int32 lon, float magVarn) {
	
	Waypoint *newWp = NULL;
	Int16 newWpSize;
	Int16 nameLength = 0;
	
	ModErrThrowIf(!ident);
	
	newWpSize = sizeof(Waypoint)+StrLen(ident);
	
	if (name) {
		
		nameLength = StrLen(name);
		newWpSize += nameLength;
		
	}
   
	PFSafeMalloc(newWp, newWpSize); // Waypoint already contains 3 \0 terminators

	newWp->latitude = INT32_TO_RAD(lat);
	newWp->longitude = INT32_TO_RAD(lon);
	newWp->magVar = magVarn;
	newWp->elevation = 0.0;

	/*
	 * copy ident and name, and null-terminate name and notes fields
	 * 
	 */

	StrCopy(newWp->ident, ident);
	if (name) {
		
		StrCopy(&newWp->ident[StrLen(ident)+1], name);
	
	}
	
	newWp->ident[StrLen(ident)+nameLength+2] = 0;

	return newWp;

}

/*
 * WDMSearchForWaypointByLocation
 * 
 */

WaypointIDType WDMSearchForWaypointByLocation(WDMHandle this, const char *ident, Int32 lat, Int32 lon, Int32 range) {

	WDMSearchHandle search;
	ShortWaypointType *swp;
	WaypointIDType found = wpNotFound;
	Int32 foundRangeSquared;

	LOGENTRY;
	
	if (range) {

		foundRangeSquared = (range/32768)*(range/32768) + 1;

	} else {

		/*
		 * look for any waypoint, irrespective of range
		 *
		 */

		foundRangeSquared = 2147483647;

	}

	search = WDMInitProxScan(this, lat, lon, wdmAll, wpAny);
	if (!search) return wpNotFound;

	do {

		swp = WDMGetProxWaypoint(search, range);
		
		if (swp && StrNCompare(swp->extra, ident, 5) == 0) {

			Waypoint *wp;

			/*
			 * make range check before retrieving the waypoint
			 * for further ident checking
			 *
			 */

			Int32 deltaLat = (lat - swp->lat)/32768;
			Int32 deltaLon = (lon - swp->lon)/32768;
			Int32 newRange = (deltaLat*deltaLat)+(deltaLon*deltaLon);

			if (newRange < foundRangeSquared) {

				wp = WDMGetWaypoint(this, swp->wpId);
				if (StrCompare(wp->ident, ident) == 0) {

					foundRangeSquared = newRange;
					found = swp->wpId;

				}
				PFMemFree(wp);

			}

		}

	} while (swp);

	WDMFreeSearch(search);

	LOGEXIT;

	return found;
		 
}

