/*
 * AsDatabase.c
 *
 * Airspace database module
 *
 */

#include "Platform.h"
#include "FMStrings.h"

#include "GlobalTypes.h"
#include "AsDatabase.h"
#include "Constants.h"
#include "Modules.h"
#include "AvCalcs.h"
#include "Utils.h"
#include "MathLib.h"

#define ModuleID AirspaceModuleID

const char *suasTypes[] = { "ALR","DGR","MOA","PHB","RST","TRA","WAR" };

enum {lat1IdxRec = 0, lon1IdxRec, lat3IdxRec, lon3IdxRec, typeIdxRec, lastIdxRec };

typedef struct {

	DmOpenRef intDB;		// used if AS database is internal
	PFFileRef extDB;		// used if AS database is external

	Int16 numRecords;		// number of Airspace records in DB

	Boolean   external;		// true if AS database is external

	/*
	 * index pointers
	 *
	 */
	
	Int16 *lat1Idx, *lon1Idx;
	Int16 *lat3Idx, *lon3Idx;

	AirspaceClassType *typeIdx;

} AirspaceDBType;
	
	
/****************************************************************************
 *
 * module variables
 *
 */

/*
 * airspace and TFR databases
 *
 */
//TODO - change to 4

#define MAX_AIRSPACE_DATABASES 2
static AirspaceDBType *asdb[MAX_AIRSPACE_DATABASES+2] = { NULL, NULL, NULL, NULL };

static char *airspaceDBNames[MAX_AIRSPACE_DATABASES+2] = { 
	APPNAME"-Airspace",
	APPNAME"-TFR",			// TODO - Airspace2
	APPNAME"-Airspace3",
	APPNAME"-TFR"
};

/****************************************************************************
 *
 * Private functions
 *
 */

static AirspaceType *ImportRecord(PFFileRef f, /*@out@*/ Int16 *lat1, /*@out@*/ Int16 *lon1,
	   	/*@out@*/ Int16 *lat3, /*@out@*/ Int16 *lon3, void (*cb)(const char *)) ASDB_SECTION;
static void AddIntToRecord(DmOpenRef db, UInt16 recNum, UInt16 val) ASDB_SECTION;
static Boolean AsImportDB(PFFileRef f, const char *dbname, UInt32 dbType, void (*cb)(const char *)) ASDB_SECTION;
static Int16 AsSearchDB(Int16 dbnum, Int16 lat1, Int16 lon1, Int16 lat3, Int16 lon3,
		AirspaceClassType filter, UInt16 *results, UInt16 maxResults) ASDB_SECTION;

static AirspaceDBType *OpenDatabase(char *intName) ASDB_SECTION;
static void CloseDatabase(AirspaceDBType *db) ASDB_SECTION;

/*
 * function : ImportAltitude
 *
 * Reads and decodes altitude from intermediate format string
 *
 * A = AMSL
 * G = AGL
 * F = FL
 * B = BYNOTAM
 *
 */

static void ImportAltitude(const char *line, AltReferenceType *ar, UInt16 *alt) {

	switch (line[0]) {

	case 'A':
		*ar = altAmsl;
		break;

	case 'G':
		*ar = altAgl;
		break;

	case 'F':
		*ar = altFL;
		break;

	case 'B':
		*ar = altNotam;
		break;

	}

	*alt = (UInt16)StrAToI(&line[1]);

}

/*
 * function : ImportRecord
 *
 * Reads a single airspace record in intermediate format from the file and
 * creates an airspace record from it.
 *
 * lat1/lon1 and lat3/lon3 are the north-west and south-east boundaries of
 * the airspace
 *
 */

static AirspaceType *ImportRecord(PFFileRef f, Int16 *lat1, Int16 *lon1, Int16 *lat3, Int16 *lon3,
		void (*cb)(const char *)) {

	char line[256], name[256];
	char *description = PFMalloc(4096);
	char *segStr = PFMalloc(2400);
	Int16 segCount;
	AirspaceType *as = PFMalloc(2400+2400*sizeof(LineSegmentType));
	void *segStart = PFMalloc(2400*sizeof(LineSegmentType));
	void *segPtr   = segStart;
	void *writePtr;
	double blat1, blon1, blat2, blon2;

	LOGENTRY;
	ModErrThrowIf(!description);
	ModErrThrowIf(!segStr);
	ModErrThrowIf(!as);
	ModErrThrowIf(!segStart);

	/*
	 * type & class:
	 *
	 * A-G = class A to G
	 *
	 * S[ADMPRTW]= Special Use (SUAS)
	 *
	 * W[LHB] = Airway, (L)ow level, (H)igh level or Both
	 *
	 * O= Other
	 *
	 */

	if (!PFReadLine(f,line)) {

		PFMemFree(as);
		PFMemFree(description);
		PFMemFree(segStart);
		PFMemFree(segStr);

		LOGEXIT;
		return NULL;

	}

	switch (line[0]) {

	case 'A': as->type = asTypeClassA; break;
	case 'B': as->type = asTypeClassB;break;
	case 'C': as->type = asTypeClassC;break; 
	case 'D': as->type = asTypeClassD;break;
	case 'E': as->type = asTypeClassE;break;
	case 'F': as->type = asTypeClassF;break;
	case 'G': as->type = asTypeClassG;break;

	case 'S':
		switch (line[1]) {

		case 'A': as->type = suasAlert;
			  break;
		case 'D': as->type = suasDanger;
			  break;
		case 'M': as->type = suasMoa;
			  break;
		case 'P': as->type = suasProhibited;
			  break;
		case 'R': as->type = suasRestricted;
			  break;
		case 'T': as->type = suasTra;
			  break;
		case 'W': as->type = suasWarning;
			  break;

		}
		as->type |= asTypeSUAS;
		break;

	case 'W':
		switch(line[1]) {

		case 'L': as->type = asTypeLowAirway;
			  break;

		case 'H': as->type = asTypeHighAirway;
			  break;

		case 'B': as->type = asTypeHighAirway | asTypeLowAirway;
			  break;

		default:ErrThrow(113);
			break;

		}
		break;

	case 'O':
		as->type = asTypeOther;
		break;

	default:
		ErrThrow(114);
		break;

	}

	LOGINT16(as->type);

	/*
	 * name
	 *
	 */

	PFReadLine(f, line);
	StrCopy(name, line);

	(*cb)(name);

	/*
	 * description
	 *
	 */

	*description = 0;
	while (line[0] != '-') {

		StrCat(description, line);
		StrCat(description, "\n");
		PFReadLine(f, line);
		LOGSTR(line);

	}

	/*
	 * frequency or RNP
	 *
	 */

	PFReadLine(f,line);

	if (as->type & asTypeAirway) {

		as->extra = StrAToI(line);

	} else {
		
		as->extra = (StrAToI(line)*256+StrAToI(&line[4]));

	}
	LOGINT16(as->extra);

	/*
	 * lower & upper altitude
	 * 
	 */

	PFReadLine(f,line);
	ImportAltitude(line, &as->lowerAltRef, &as->lowerAlt);

	PFReadLine(f,line);
	ImportAltitude(line, &as->upperAltRef, &as->upperAlt); 

	LOGINT16(as->lowerAlt);
	LOGINT16(as->upperAlt);
	
	/*
	 * boundary segments follow:
	 *
	 * L = Line:
	 *
	 * L<lat> <lon>
	 *
	 * A = Arc:
	 *
	 * A[LR]<lat2> <lon2> <lat3> <lon3> <lat1> <lon1>
	 *
	 * (L=left arc, R=right arc)
	 *
	 * (point 1 is centre, 2 and 3 are start and end points)
	 *
	 * C = Circle:
	 *
	 * C<lat> <lon> <radius>
	 *
	 * segStr is built and the bounding limits of the airspace (blat1 etc)
	 * are updated as the segments are read and processed
	 *
	 */

	blat1 = -PI/2;
	blon1 = -PI;
	blat2 = PI/2;
	blon2 = PI;

	segCount = 0;

	PFReadLine(f,line);

	while (line[0] != 'X') {

		const char *s = line+1;

		if (line[0] == 'L') {

			LineSegmentType l;
			double lat, lon;

			lat = StrToDouble(s);
			l.lat = DEG_TO_INT32(lat);

			SKIP_FIELD(s);
			lon = 0-StrToDouble(s);
			l.lon = DEG_TO_INT32(lon);

			//LOGINT32(l.lat); LOGINT32(l.lon);

			segStr[segCount] = 'l';
			*(LineSegmentType*)segPtr = l;
			segPtr += sizeof(LineSegmentType);

			blat1 = MAX(blat1, DEG_TO_RAD(lat));
			blon1 = MAX(blon1, DEG_TO_RAD(lon));
			blat2 = MIN(blat2, DEG_TO_RAD(lat));
			blon2 = MIN(blon2, DEG_TO_RAD(lon));

		} else if (line[0] == 'A') {

			double lat1, lon1;
			double lat2, lon2;
			double lat3, lon3;
			double radius,bearing;

			ArcSegmentType ar;

			s+=1;

			/*
			 * end points
			 *
			 */

			lat2 = DEG_TO_RAD(StrToDouble(s));
			SKIP_FIELD(s);
			lon2 = 0-DEG_TO_RAD(StrToDouble(s));
			SKIP_FIELD(s);
			lat3 = DEG_TO_RAD(StrToDouble(s));
			SKIP_FIELD(s);
			lon3 = 0-DEG_TO_RAD(StrToDouble(s));

			/*
			 * centre
			 *
			 */
			
			SKIP_FIELD(s);
			lat1 = DEG_TO_RAD(StrToDouble(s));
			SKIP_FIELD(s);
			lon1 = 0-DEG_TO_RAD(StrToDouble(s));

			ar.lat = RAD_TO_INT32(lat1);
			ar.lon = RAD_TO_INT32(lon1);

			/*
			 * note to self:
			 *
			 * Because 0 < bearing < 2*PI (check AvCalcs),
			 * RAD_TO_INT16(bearing) can return under/overflow Int16 but
			 * luckily it doesn't matter - only under PalmOS though! Under
			 * CygWin it definetely needs handling.
			 *
			 * During the porting process, this should be addressed
			 *
			 */

			bearing = AvCalcGreatCircleCourse(lat1, lon1, lat2, lon2, &radius);
			ar.radius = RAD_TO_INT32(radius); 
			ar.start  = RAD_TO_INT16(bearing); 
			bearing = AvCalcGreatCircleCourse(lat1, lon1, lat3, lon3, &radius); 
			ar.end    = RAD_TO_INT16(bearing);

			if (line[1] == 'L') ar.radius = -ar.radius;

			//LOGINT32(ar.radius);
			//LOGINT16(ar.start);
			//LOGINT16(ar.end);

			segStr[segCount] = 'a';
			*(ArcSegmentType*)segPtr = ar;
			segPtr += sizeof(ArcSegmentType);

			/*
			 * assume arc is a complete circle, and calculate
			 * bounding box accordingly
			 *
			 * latitude limits are easy - just add the
			 * radian-radius, and limit the max to +-90 degrees
			 *
			 */

			blat1 = MAX(blat1, MIN(PI/2, lat1+radius));
			blat2 = MIN(blat2, MAX(-PI/2,lat1-radius));

			/*
			 * longitude is more complicated, as it needs to wrap
			 * around.
			 *
			 * scale the radius according to our latitude
			 *
			 * remember west is +ve!!!
			 *
			 * Wrap around at 180deg W/E
			 *
			 */

			radius /= cos(lat1);

			lon1 += radius;
			if (lon1 > PI) lon1 -= 2*PI;
			blon1 = MAX(blon1, lon1);
			blon2 = MIN(blon2, lon1);

			lon1 -= 2*radius;
			if (lon1 < -PI) lon1 += 2*PI;
			blon1 = MAX(blon1, lon1);
			blon2 = MIN(blon2, lon1);

		} else if (line[0] == 'C') {

			double lat1, lon1;
			double radius;

			ArcSegmentType ar;

			/*
			 * centre
			 *
			 */
			
			lat1 = DEG_TO_RAD(StrToDouble(s));
			SKIP_FIELD(s);
			lon1 = 0-DEG_TO_RAD(StrToDouble(s));

			ar.lat = RAD_TO_INT32(lat1);
			ar.lon = RAD_TO_INT32(lon1);

			/*
			 * radius in nm
			 *
			 */

			SKIP_FIELD(s);
			radius = NM_TO_RAD(StrToDouble(s));
			ar.radius = RAD_TO_INT32(radius);
			ar.start  = 0;
			ar.end    = 0;

			//LOGINT32(ar.lat);LOGINT32(ar.lon);
			//LOGINT32(ar.radius);

			segStr[segCount] = 'a';
			*(ArcSegmentType*)segPtr = ar;
			segPtr += sizeof(ArcSegmentType);

			/*
			 * latitude limits are easy - just add the
			 * radian-radius, and limit the max to +-90 degrees
			 *
			 */

			blat1 = MAX(blat1, MIN(PI/2, lat1+radius));
			blat2 = MIN(blat2, MAX(-PI/2,lat1-radius));

			/*
			 * longitude is more complicated, as it needs to wrap
			 * around.
			 *
			 * scale the radius according to our latitude
			 *
			 * remember west is +ve!!!
			 *
			 * Wrap around at 180deg W/E
			 *
			 */

			radius /= cos(lat1);

			lon1 += radius;
			if (lon1 > PI) lon1 -= 2*PI;
			blon1 = MAX(blon1, lon1);
			blon2 = MIN(blon2, lon1);

			lon1 -= 2*radius;
			if (lon1 < -PI) lon1 += 2*PI;
			blon1 = MAX(blon1, lon1);
			blon2 = MIN(blon2, lon1);

		}

		segCount++;
		PFReadLine(f,line);

	}
	segStr[segCount] = 0;
	
	LOGINT16(segPtr - segStart);

	/*
	 * adjust length of strings so that the segment records
	 * start on an even byte boundary
	 *
	 */

	if ( (segCount+1 + StrLen(description) + 1) & 1 ) {

		LOGTAG("Adjusting length");
		StrCat(description,"\n");

	}
	
	/*
	 * add the segment string, name, auth and segment records to the
	 * airspace record
	 *
	 */

	LOGLINE;

	writePtr = (void*)&as->segmentCode;
	PFMemMove(writePtr, segStr, segCount+1);
	writePtr += segCount+1;
	PFMemMove(writePtr, description, StrLen(description)+1);
	writePtr += StrLen(description)+1;

	PFMemMove(writePtr, segStart, segPtr - segStart);
	writePtr += segPtr - segStart;

	LOGSTR(GetStringFromList(as->segmentCode,1));

	PFMallocResize(as, writePtr-(void*)as);

	LOGINT16(PFMallocSize(as));


	/*
	 * adjust the longitude limits if the span is > 180 degrees by
	 * swapping lon1 and lon2 around
	 *
	 */

	if (fabs(blon1 - blon2) > PI) {

		double tmp = blon1;

		blon1 = blon2;
		blon2 = tmp;

	}

	*lat1 = RAD_TO_INT16(blat1);
	*lon1 = RAD_TO_INT16(blon1);
	*lat3 = RAD_TO_INT16(blat2);
	*lon3 = RAD_TO_INT16(blon2);
	 
	PFMemFree(description);
	PFMemFree(segStart);
	PFMemFree(segStr);

	LOGEXIT;
	return as;

}

/*
 * function : AddIntToRecord
 *
 * Adds the specified integer to the end of the specified record in db.
 *
 */

static void AddIntToRecord(DmOpenRef db, UInt16 recNum, UInt16 val) {

	UInt16    oldSize;

	/*
	 * a size of 1 indicates this is the first entry in the cell
	 *
	 */

	oldSize = DBRecordGetSize(db, recNum);
	if (oldSize == 1) oldSize = 0;

	ModErrThrowIf(!DBRecordResize(db, recNum,oldSize+sizeof(val)));

	DBRecordUpdate(db, recNum, oldSize,&val, sizeof(val));

}
	
/*
 * function : AsImportDB
 *
 */

static Boolean AsImportDB(PFFileRef f, const char *dbname, UInt32 dbType, void (*cb)(const char *)) {

	DmOpenRef db;
	UInt16 j;
	UInt16 numRecords;

	LOGENTRY;
	LOGSTR(dbname);
	LOGINT32(dbType);

	db = DBOpen(dbname, false, true);

	ModErrThrowIf(!db);

	LOGLINE;

	/*
	 * purge any existing records
	 *
	 */

	numRecords = DBGetNumRecords(db);
	LOGINT16(numRecords);
	if (numRecords) {

		(*cb)(StrPurgingDatabase);
		DBClose(db);
		DBDelete(dbname);

		db = DBOpen(dbname, false, true);

	}

	/*
	 * dimension new database.
	 *
	 * 5 index records
	 *
	 */

	LOGLINE;

	(*cb)(StrCreatingdatabaseStructure);
	for (j=0;j<lastIdxRec;j++) {

		UInt16 recnum = DBNewRecord;

		(void)DBRecordCreate(db, recnum, NULL, 1);

	}

	/*
	 * import records from file
	 *
	 */

	ErrTry {

		LOGINT16(numRecords);

		for (j=0;;j++) {

			AirspaceType *as;
			UInt16 recNum = j+lastIdxRec; // skip past db headers

			Int16 lat1, lon1, lat3, lon3;

			LOGTAG("Adding:");
			LOGINT16(j);

			as = ImportRecord(f, &lat1, &lon1, &lat3, &lon3, cb);

			if (as == NULL) {
				
				LOGTAG("End of file mark");
				break;

			}

			LOGINT16(PFMallocSize(as));

			/*
			 * add record to the database
			 *
			 */

			(void)DBRecordCreate(db, recNum, as, PFMallocSize(as));
			
			LOGTAG("Bounds:");
			LOGINT16(lat1);
			LOGINT16(lon1);
			LOGINT16(lat3);
			LOGINT16(lon3);

			/*
			 * store bounding coordinates and airspace type
			 *
			 */

			AddIntToRecord(db, lat1IdxRec, lat1);
			AddIntToRecord(db, lon1IdxRec, lon1);
			AddIntToRecord(db, lat3IdxRec, lat3);
			AddIntToRecord(db, lon3IdxRec, lon3);
			AddIntToRecord(db, typeIdxRec, as->type);

			PFMemFree(as);

		}

	} ErrCatch(errNo) {

		(*cb)(StrErrorDetectedPurgingDatabase);
		DBClose(db);
		DBDelete(dbname);

		errNo = 0;
		return false;

	} ErrEndCatch;
	
	DBClose(db);

	LOGEXIT;
	return true;

}

/*
 * function : AsSearchDB
 *
 */

#define SETBIT(ptr, bit) ptr[bit / 8] |= ( 1 << ((bit) & 7) )
#define BITVAL(ptr, bit) ( ptr[bit / 8] & ( 1<< ((bit) & 7) ) )

static Int16 AsSearchDB(Int16 dbnum, Int16 lat1, Int16 lon1, Int16 lat3, Int16 lon3,
		AirspaceClassType filter, UInt16 *results, UInt16 maxResults) {

	AirspaceDBType *db = asdb[dbnum];
	Int16 dbMask = dbnum << 15;		// TODO - change to 14 for 4 databases
	Int16 j;
	Int16 numResults = 0;

	Int16 *lat1Idx, *lon1Idx;
	Int16 *lat3Idx, *lon3Idx;
	AirspaceClassType *typeIdx;
	UInt16 numAsRecords = db->numRecords;

	Int16 lonWidth = lon1 - lon3;	// see lonWidth usage below
	
	LOGENTRY;

	LOGTIMESTART;

	lat1Idx = db->lat1Idx;
	lon1Idx = db->lon1Idx;
	lat3Idx = db->lat3Idx;
	lon3Idx = db->lon3Idx;
	typeIdx = db->typeIdx;

	LOGINT16(lon1);LOGINT16(lon3);
	LOGINT16(INDEXROW(lat1));LOGINT16(INDEXCOL(lon1));
	LOGINT16(INDEXROW(lat3));LOGINT16(INDEXCOL(lon3));

	for (j = 0; numResults < maxResults && j<numAsRecords; j++) {

		Int16 asLon1, asLon2;

		if (!(typeIdx[j] & filter) || lat3Idx[j] > lat1 || lat1Idx[j] < lat3 || 
				lon1Idx[j] < lon3 || lon3Idx[j] > lon1 )
			continue;

		/*
		 * note:
		 *
		 * We're making (dangerous?) use of the overflow/underflow of Int16
		 * types here. Basically, this lets us gracefully handle the
		 * wrap-around situation of the globe where longitudes are close to
		 * -32768 or +32767
		 *
		 * If you do the maths, you'll see it works!!
		 *
		 */

		asLon1 = lon1 - lon1Idx[j];
		asLon2 = lon1 - lon3Idx[j];

		if (asLon1 > lonWidth || asLon2 < 0) continue;

		results[numResults++] = (j+1) | dbMask;

	}

	LOGINT16(numResults);
	
	LOGTIMESTOP;

	LOGEXIT;

	return numResults;

}

/*
 * function : OpenDatabase
 *
 */

static AirspaceDBType *OpenDatabase(char *intName) {

	AirspaceDBType *db = PFMalloc(sizeof(AirspaceDBType));
	char extName[64];

	ModErrThrowIf(!db);

	/*
	 * check for external database first
	 *
	 */

	StrPrintF(extName, "/PALM/Launcher/%s.pdb", intName);
	if ((db->extDB = PFOpenFile(extName, pfFileReadOnly))) {

		LOGTAG("External airspace");
		db->numRecords = DBFGetNumRecords(db->extDB) - lastIdxRec;

		if (db->numRecords > 0) {

			db->lat1Idx = DBFRecordGet(db->extDB, lat1IdxRec, false);
			db->lon1Idx = DBFRecordGet(db->extDB, lon1IdxRec, false);
			db->lat3Idx = DBFRecordGet(db->extDB, lat3IdxRec, false);
			db->lon3Idx = DBFRecordGet(db->extDB, lon3IdxRec, false);
			db->typeIdx = DBFRecordGet(db->extDB, typeIdxRec, false);

		} else {
			
			PFCloseFile(db->extDB);

		}

		db->external = true;

	}  else {

		/*
		 * check internal...
		 *
		 */

		LOGTAG("Internal airspace");
		db->external = false;
		db->intDB = DBOpen(intName, false, true);

		db->numRecords = DBGetNumRecords(db->intDB) - lastIdxRec;

		if (db->numRecords > 0) {
			
			db->lat1Idx = DBRecordGet(db->intDB, lat1IdxRec, false);
			db->lon1Idx = DBRecordGet(db->intDB, lon1IdxRec, false);
			db->lat3Idx = DBRecordGet(db->intDB, lat3IdxRec, false);
			db->lon3Idx = DBRecordGet(db->intDB, lon3IdxRec, false);
			db->typeIdx = DBRecordGet(db->intDB, typeIdxRec, false);

		} else {
			
			DBClose(db->intDB);

		}

	}

	LOGINT16(db->numRecords);

	if (db->numRecords < 1) {

		PFMemFree(db);
		db = NULL;

	}

	return db;

}


/*
 * function : CloseDatabase
 *
 */

static void CloseDatabase(AirspaceDBType *db) {

	LOGENTRY;

	if (db->external) {

		LOGTAG("Closing external");

		DBRecordFree(db->lat1Idx);
		DBRecordFree(db->lon1Idx);
		DBRecordFree(db->lat3Idx);
		DBRecordFree(db->lon3Idx);
		DBRecordFree(db->typeIdx);

		PFCloseFile(db->extDB);

	} else {

		LOGTAG("Closing internal");
		DBRecordFree(db->lat1Idx);
		DBRecordFree(db->lon1Idx);
		DBRecordFree(db->lat3Idx);
		DBRecordFree(db->lon3Idx);
		DBRecordFree(db->typeIdx);

		DBClose(db->intDB);

	}

	PFMemFree(db);

	LOGEXIT;

}

/****************************************************************************
 *
 * Public functions
 *
 */

/*
 * function : AsOpenDatabase
 *
 */

Boolean AsOpenDatabase(void) {

	Int16 j;

	for (j=0;j<MAX_AIRSPACE_DATABASES;j++) {

		asdb[j] = OpenDatabase(airspaceDBNames[j]);

	}

	return true;

}

/*
 * function : AsGetDatabaseInfo
 *
 */

void *AsGetDatabaseInfo(void) {

	if (!asdb[0]) return NULL;

	if (asdb[0]->external) {

		return NULL;

	} else {
		
		return DBGetInfo(asdb[0]->intDB);

	}

}

/*
 * function : AsSearchDatabase
 *
 */

Int16 AsSearchDatabase(Int16 lat1, Int16 lon1, Int16 lat3, Int16 lon3,
		AirspaceClassType filter, AirspaceIDType *results, UInt16 maxResults) {

	UInt16 numResults = 0;
	Int16 j;

	LOGENTRY;

	for (j=0;j<MAX_AIRSPACE_DATABASES && numResults < maxResults; j++) {

		if (asdb[j]) numResults += AsSearchDB(j, lat1, -lon1, lat3, -lon3, filter, &results[numResults], maxResults);
		LOGINT16(numResults);

	}

	LOGEXIT;

	return numResults;

}

/*
 * function : AsGetAirspace
 *
 */

AirspaceType *AsGetAirspace(AirspaceIDType id) {

	AirspaceType   *ptr;
	UInt16 recNum;
	AirspaceDBType *db;

	//LOGENTRY;
	//LOGINT16(id);

	db = asdb[(id & 0x8000) >> 15];	// TODO - change to 0xC000 >> 14 for 4 databases
	recNum = (id & 0x7FFF) - 1;		// 0x3fff

	recNum += lastIdxRec;	// skip past database headers

	if (db->external) {

		ptr = DBFRecordGet(db->extDB, recNum, false);

	} else {

		ptr = DBRecordGet(db->intDB, recNum, false);

	}

	//LOGEXIT;

	return ptr;

}

/*
 * function : AsDecodeAltitude
 *
 */

void AsDecodeAltitude(char *str, Int32 *altFeet,  UInt16 altUnits, AltReferenceType ref, UInt16 alt) {

	/*
	 * handle special cases first
	 *
	 */

	if (ref == altNotam) {

		StrPrintF(str, StrBYNOTAM);
		*altFeet=0;
		return;

	}

	if (ref == altFL && alt == 9999) {

		StrPrintF(str, StrUNLTD);
		*altFeet=999999;
		return;

	}

	if (ref == altAgl && alt == 0) {

		StrPrintF(str, StrSFC);
		*altFeet = 0;
		return;

	}

	/*
	 * convert from feet to metres
	 *
	 */

	if (altUnits == METRE_UNITS && ref != altFL) {

		float metres = (float) alt * METRES_PER_FOOT;

		alt = (UInt16)metres;

	}
		
	switch (ref) {

	case altFL:
		StrPrintF(str, "FL%d", alt);
		*altFeet = alt * 100;
		break;

	case altAgl:

		/*
		 * AGL special case - we set maximum altitude in *altFeet. This
		 * is because FlightMaster doesn't know the AMSL altitude of
		 * the airspace, so to play safe it should be highlighted as a
		 * possible affecting airspace (see MapAirspaceDraw)
		 *
		 */

		StrPrintF(str,"%uAGL",(alt+50)/100);
		*altFeet = 99999;

		break;

	case altAmsl:
		StrPrintF(str,"%u", (alt+50)/100);
		*altFeet = alt;
		break;

	default:

		ModErrThrow();

	}

}

/*
 * function : AsCloseDatabase
 *
 */

void AsCloseDatabase(void) {

	Int16 j;

	LOGENTRY;

	for (j=0;j<MAX_AIRSPACE_DATABASES;j++) {

		if (asdb[j]) CloseDatabase(asdb[j]);
		asdb[j] = NULL;

	}

	LOGEXIT;

}

/*
 * function : AsImport
 *
 */

Boolean AsImport(Int16 dbNumber, void(*cb)(const char *)) {

	PFFileRef f;

	const char *sdFilename[] = { FILEROOT "airspace.fma", FILEROOT "tfrs.fma" };
	const char *palmFilename[] = { APPNAME"-Airspace", APPNAME"-TFR" };
	const UInt32 dbType[] = { AirspaceDatabaseType, TFRDatabaseType };

	LOGENTRY;

	if ((f = PFOpenFile(sdFilename[dbNumber], pfFileReadOnly))) {

		Boolean success;

		success = AsImportDB(f, palmFilename[dbNumber], dbType[dbNumber], cb);
		PFCloseFile(f);

		LOGEXIT;

		return success;

	}

	LOGLINE;
	(*cb)(StrFilenotfound);

	LOGEXIT;
	return false;

}
