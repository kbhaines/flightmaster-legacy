/*
 * TrackLog.c
 *
 * (c) Blackhawk Systems 2005
 *
 */

#include "Platform.h"
#include "GlobalTypes.h"
#include "Constants.h"
#include "TrackLog.h"
#include "Gps.h"
#include "Utils.h"
#include "Modules.h"

/*******************************************************************************
 *
 * Module variables
 *
 */

#define MAX_TRACKSIZE 8000

static DmOpenRef intTrackDB = NULL;

/*
 * control variables for the internal log
 *
 * trackLogPtr remains locked by call to TLOpen until call
 * to TLClose
 *
 */

static TLInternalRecordType *trackLogPtr;

static Int16 logWritePtr;
static Int16 logSize;

static PFFileRef extLog;	// external log reference

static Boolean header2HasBeenOutput = false;

/*******************************************************************************
 *
 * Module functions
 *
 */

#define TLErrThrowIf(c) if (c) ErrThrow(TrackLogModuleID | __LINE__)

static void WriteInternal(Int32 lat, Int32 lon) ADT2_SECTION;


/*
 * WriteInternal
 *
 * Writes an entry to the internal track log
 *
 */

static void WriteInternal(Int32 lat, Int32 lon) {

	TLInternalRecordType new;
	Int16 vars[2];

	LOGENTRY;

	new.lat = lat;
	new.lon = lon;

	LOGINT16(logWritePtr);
	LOGINT16(logSize);

	LOGLINE;
	DBRecordUpdate(intTrackDB, 0, logWritePtr*sizeof(TLInternalRecordType), &new, sizeof(new));

	if (logSize < MAX_TRACKSIZE) {

		logSize++;

	}

	if ( ++logWritePtr == MAX_TRACKSIZE) {

		logWritePtr = 0;

	}
	
	LOGINT16(logSize);
	LOGINT16(logWritePtr);

	/*
	 * store the size & write pointers
	 *
	 */

	vars[0] = logSize;
	vars[1] = logWritePtr;
	DBRecordUpdate(intTrackDB, 0, sizeof(TLInternalRecordType) * MAX_TRACKSIZE, &vars, sizeof(vars));
	
	LOGEXIT;


}

const char *gpxHeader1 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
<gpx version=\"1.0\" creator=\"FlightMaster - http://www.flight-master.com\"\n\
xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n\
xmlns=\"http://www.topografix.com/GPX/1/0\"\n\
xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd\">\n";

const char *gpxHeader2 = "<trk><trkseg>\n";

const char *gpxFooter2 = "</trkseg></trk>\n";
const char *gpxFooter1 = "</gpx>\n";

/*******************************************************************************
 *
 * Global functions
 *
 */

/*
 * TLOpen
 *
 * open internal & external (if possible) track log for reading/writing
 *
 */

void TLOpen(void)  {

	UInt16    *ptr;

	TLErrThrowIf(intTrackDB);

	/*
	 * internal track log
	 *
	 */

	intTrackDB = DBOpen(APPNAME"-Track", false, true);

	TLErrThrowIf(!intTrackDB);

	if (DBGetNumRecords(intTrackDB) == 0 ) {

		/*
		 * initialise a new track log
		 *
		 */

		UInt16 index = DBNewRecord;
		UInt16 initial[2] = { 0, 0};

		LOGTAG("Newlog");

		/*
		 * MAX_TRACKSIZE records, 4 bytes for size and current write pointer
		 *
		 */

		(void)DBRecordCreate(intTrackDB, index, NULL, MAX_TRACKSIZE*sizeof(TLInternalRecordType) + 2*sizeof(UInt16));

		LOGLINE;

		/*
		 * set two UInt16s at end of record : 0 = logSize, 1 = logWritePtr
		 *
		 */

		DBRecordUpdate(intTrackDB, 0, sizeof(TLInternalRecordType)*MAX_TRACKSIZE, &initial, sizeof(UInt16)*2);
		
		LOGLINE;

	}

	trackLogPtr = (TLInternalRecordType*) DBRecordGet(intTrackDB, 0, false);
	ptr = (UInt16*)trackLogPtr;

	ptr = (void*)ptr + sizeof(TLInternalRecordType) * MAX_TRACKSIZE;
	logSize = ptr[0];
	logWritePtr= ptr[1];

	LOGINT16(logSize);
	LOGINT16(logWritePtr);
	
	/*
	 * external track log file
	 *
	 */

	if ( (extLog = PFOpenFile("/FM-Track.gpx", pfFileReadWrite)) ) {

		UInt32 size = PFFileSize(extLog);

		if (size)  {
			
			PFSeekFile(extLog, pfSeekEnd, -((Int32)StrLen(gpxFooter1)));

		} else {

			PFWriteFile(extLog, gpxHeader1, StrLen(gpxHeader1) );
			
		}
		
	}

}

/*
 * TLClose
 *
 * Well, duh!
 *
 */

void TLClose(void)  {

	TLErrThrowIf(!intTrackDB);

	/*
	 * write end-of-log marker
	 *
	 */

	WriteInternal(0,0);

	DBRecordFree(trackLogPtr);

	DBClose(intTrackDB);
	intTrackDB = NULL;

	if (extLog) {

		if (header2HasBeenOutput) {
			
			PFWriteFile(extLog, gpxFooter2, StrLen(gpxFooter2) );
			
		}
		
		PFWriteFile(extLog, gpxFooter1, StrLen(gpxFooter1) );

		PFCloseFile(extLog);
		extLog = NULL;

	}

}

/*
 * TLAddPoint
 *
 * adds a point to the track log
 *
 */

/*
 * in INT32 radians, set to 250 feet
 *
 * (32 = 1 foot in Int32 radian-speak)
 *
 */

#define LOG_DISTANCE (250 * 32)
		
void TLAddPoint(GPSPosnData *gps)  {

	static Int32 lastLat = 0, lastLon = 0;
	Int32 dLat = gps->lat32 - lastLat;
	Int32 dLon = gps->lon32 - lastLon;

	/*
	 * log to internal track log only if we've moved LOG_DISTANCE
	 *
	 */

	if (dLat < -LOG_DISTANCE || dLat > LOG_DISTANCE ||
			dLon < -LOG_DISTANCE || dLon > LOG_DISTANCE) {

		WriteInternal(gps->lat32, gps->lon32);

		lastLat = gps->lat32;
		lastLon = gps->lon32;

	}

	if (extLog) {

		char str[256], time[48];
		char lat[16], lon[16], alt[8], speed[8], course[8];

		UInt16 len;
		
		if (!header2HasBeenOutput) {
		
			PFWriteFile(extLog, gpxHeader2, StrLen(gpxHeader2) );
			header2HasBeenOutput = true;
			
		}
		
		//		Example output
		//		<trkpt lat="45.846710000" lon="-123.501760000">
		//		  <ele>1457.800000</ele>
		//		  <time>2008-02-24T01:30:10Z</time>
		//		  <course>135.300003</course>
		//		  <speed>34.466785</speed>
		//		</trkpt>

		StrCopy(lat, DoubleToStr(RAD_TO_DEG(gps->latitude), 5));
		StrCopy(lon, DoubleToStr(RAD_TO_DEG(gps->longitude), 5));
		StrCopy(alt, FloatToStr(METRES_PER_FOOT * gps->altitude,1));
		StrPrintF(time, "%04d-%02d-%02dT%02d:%02d:%02dZ",
				(Int16)gps->utc.year, (Int16)gps->utc.month, 
				(Int16)gps->utc.day,(Int16)gps->utc.hour,
				(Int16)gps->utc.minute, (Int16)gps->utc.second);
		StrCopy(course, FloatToStr(gps->trueHeading, 1) );
		StrCopy(speed, FloatToStr((KM_PER_NM*gps->speed)/3.6,1));

		StrPrintF(str, "<trkpt lat=\"%s\" lon=\"%s\">\n<ele>%s</ele><time>%s</time><course>%s</course><speed>%s</speed></trkpt>\n", 
				lat, lon, alt,time, course, speed);
		

		len = StrLen(str);
		if (PFWriteFile(extLog, str, len) != len) {

			PFCloseFile(extLog);
			extLog = NULL;

		}

	}

}

/*
 * TLClear
 *
 * Clears the track log
 *
 * The external track log file is emptied and re-initialised with headers
 *
 *
 */

void TLClear(void)  {

	logWritePtr = 0;
	logSize = 0;

	if (extLog) {

		PFCloseFile(extLog);
		if ( (extLog = PFOpenFile("/FM-Track.gpx", pfFileTruncate)) ) {

			LOGTAG("ExtLog Header");
			PFWriteFile(extLog, gpxHeader1, StrLen(gpxHeader1) );
			header2HasBeenOutput = false;
			
		}

	}

}

/*
 * TLInitGetPoint
 *
 */

TLRefType TLInitGetPoint(void) {

	TLRefType logReadPtr;

	if (logWritePtr == 0)

		logReadPtr = MAX_TRACKSIZE-1;

	else

		logReadPtr = logWritePtr - 1;

	return  logReadPtr;

}

/*
 * TLGetPoint
 *
 */

Boolean TLGetPoint(TLRefType *ref, TLInternalRecordType *result)  {

	TLRefType logReadPtr = *ref;

	if (!logSize) return false;

	//LOGINT16(logReadPtr);
	//LOGINT16(logWritePtr);

	if (logReadPtr == logWritePtr) {
		
		return false;

	}
		
	*result = trackLogPtr[logReadPtr];

	//LOGINT32(result->lat);
	//LOGINT32(result->lon);
	
	if (logReadPtr-- == 0) {
		
		if (logSize < MAX_TRACKSIZE)

			return false;

		else 

			logReadPtr = MAX_TRACKSIZE - 1;
	
	}

	*ref = logReadPtr;
	return true;

}

/*
 * TLGetForwardPoint
 *
 */

Boolean TLGetForwardPoint(TLRefType *ref, TLInternalRecordType *result) {

	if (!logSize) return false;

	LOGINT16(*ref);
	LOGINT16(logWritePtr);

	if (*ref == logWritePtr) {
		
		return false;

	}
		
	*result = trackLogPtr[*ref];

	LOGINT32(result->lat);
	LOGINT32(result->lon);
	
	(*ref)++;
	if (*ref == MAX_TRACKSIZE) {

		*ref = 0;

	}

	return true;

}

/*
 * TLGetNumNewEntries
 *
 */

Int16 TLGetNumNewEntries(TLRefType ref) {

	if (!logSize) return 0;

	LOGINT16(logWritePtr);
	LOGINT16(ref);

	if (logWritePtr >= ref) 

		return logWritePtr - ref;

	return MAX_TRACKSIZE - (ref - logWritePtr) ;

}

/*
 * TLWriteBreak
 *
 */

void TLWriteBreak() {

	WriteInternal(0,0);

}
