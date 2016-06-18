/*
 * gps.c
 *
 * GPS Input and decoding module 
 *
 */

#include "Platform.h"
#include "FMStrings.h"
#include <ExpansionMgr.h>
#include "Gps.h"
#include "MathLib.h"
#include "Utils.h"
#include "Modules.h"
#include "GlobalTypes.h"
#include "AvCalcs.h"

#include "GPSLib68K.h"
#include "Constants.h"

#include "ResourceDefines.h"
#include "FMPreferences.h"

#include "GarminGPS.h"

#define ModuleID GPSModuleID

/******************************************************************************
 *
 * Global data
 *
 *
 */

extern const FMPreferencesType Preferences;
extern const UInt32 DemoDateLimit;

extern const Boolean GPSState;
extern char ErrExtra[];
extern GPSType GPS;

/******************************************************************************
 *
 * module constants
 *
 */
#define ERR_GPS_MSG_NOT_DECODED -1
#define BAUD_SWITCH_TIMEOUT	3
#define GPS_COMMS_TIMEOUT	4
#define KM_PER_NM               1.852

#define SimRate 1
/******************************************************************************
 *
 * module data
 *
 */

static PFPortRefType 	gpsPortId;
static char 	gpsMessageBuff[4096];	/* buffer for GPS messages */
static Int16 	currBuffSize;		/* chars used in gpsMessageBuff */
static UInt32	currCycleTicks;		/* ticks read once at start of cycle */
static UInt32	lastCommsTime;		/* time in systicks of last comm 
					   from GPS */
static UInt32	lastFixTime = 0;	/* time in systicks of last GPRMC 
					   message from GPS */

static UInt16   cycleCounter = 0;

/*
 * current state of the GPS module
 *
 * InitReqd = not initialised
 * SerialLocking = reading from port, no GPS data
 * Ok = GPS data
 * FeedLost = input has stopped, attempting recovery
 *
 */

static enum {

	gpsOff, 
	gpsSyncing,
	gpsSync,
	gps2d,
	gps3d,
	gpsTimeout
		
}gpsModuleState = gpsOff;

static UInt32 gpsBaudRate = 4800;


/*
 * kinds of NMEA messages we care about
 *
 */

typedef enum {
	gpsUnknown,
	gpsRMC,
	gpsInvRMC,
	gpsGGA,
	gpsInvGGA,
	gpsGSA,
	gpsGSV,
	gpsST
} GPSMessageType;


static GPSSourceType gpsSource;

static float gpsAltCorrection = 0.0;

static Boolean magVarnCalculated = false;


/*
 * GPS Simulation parameters
 *
 */

static struct {

	/*
	 * position lat & lon are in radians
	 *
	 * +ve is north/east
	 * 
	 */

	double lat;
	double lon;
	
	/*
	 * heading in degrees true
	 *
	 */

	float targetHeading;
	float heading;

	/*
	 * altitude is in feet
	 *
	 */

	float targetAltitude;
	float altitude;
	
	/*
	 * Speed is in knots
	 *
	 */

	float targetSpeed;
	float speed;

} simulation;
	

/*
 * simulation parameters
 *
 */

static const float rateOfClimb = 800;
static const float rateOfTurn    = 3.0;
static const float maxAcceleration= 5.0;

static PFFileRef gpsLog = NULL;

#define GpsErrThrow ErrThrow(GPSModuleID | __LINE__)
#define GpsErrThrowIf(c) if (c) ErrThrow(GPSModuleID | __LINE__)
#define GpsErrThrowIfExt(c,x) if (c) { StrCopy(ErrExtra, x); ErrThrow(GPSModuleID | __LINE__); }

/*******************************************************************************
 *
 * private functions
 *
 */

static void RunSimulation(void) GPS_SECTION;
static UInt32 GetNum(char *str, UInt8 numChars) GPS_SECTION;
static char *GetField(char **ptr) GPS_SECTION;
static Err OpenGPSPort(UInt32 baudRate, Boolean resetTimers, PFBluetoothAddressType *btID) GPS_SECTION;
static void DecodePMGNST(char *stMsg) GPS_SECTION;
static void DecodeGSA(char *gsaMsg) GPS_SECTION;
static Boolean DecodeGSV(char *gsvMsg) GPS_SECTION;
static Boolean DecodeGGA(char *gga) GPS_SECTION;
static Boolean DecodeRMC(char *rmc) GPS_SECTION;
static GPSMessageType DecodeGPSMsg(char *startOfMsg) GPS_SECTION;
static void UpdateStateMachine(Boolean gpsMessageReceived) GPS_SECTION;

/*
 * function : GetNum
 *
 * Converts a specific number of characters from a string to a positive integer.
 *
 */
static UInt32 GetNum(char *str, UInt8 numChars) {
	Int8 	j;
	UInt16 	base = 1;
	UInt32	result = 0;

	for (j=numChars-1;j>=0;j--, base *=10 ) {
		result += (str[j] - '0') * base ;
	}
	return result;
}
		
/*
 * function : GetField
 *
 * Advances the passed-in pointer to the start of the next field in a 
 * comma-separated string of fields. 
 *
 * Returns a pointer to the value in the field *before* ptr is advanced to 
 * the next field.
 *
 */

static char *GetField(char **ptr) {
	static char 	fieldValue[40];
	char 		*nextComma;

	if (*ptr == NULL)
		return NULL;

	nextComma = StrChr(*ptr, ',');
	if (nextComma) {

		PFMemMove(fieldValue, *ptr, (nextComma-*ptr)); 
		fieldValue[nextComma-*ptr]=0;
		*ptr = nextComma + 1;

	} else {
		StrCopy(fieldValue, *ptr);
		*ptr = NULL;
	}
	return fieldValue;
}

/*
 * function : OpenGPSPort
 *
 * Does what it says :-)
 *
 */

static Err OpenGPSPort(UInt32 baudRate, Boolean resetTimers, PFBluetoothAddressType *btID) {

	LOGENTRY;
	
	if (gpsSource == gpsSerial || gpsSource == gpsCradle || gpsSource == gpsUSB || gpsSource == gpsClie){ 

		PFPortRefType portID;
		
		switch (gpsSource) {

		case gpsCradle:
			portID = portCradle;
			break;

		case gpsSerial:
			portID = portSerial;
			break;

		case gpsUSB:
			portID = portUSB;
			break;

		case gpsClie:
			portID = portClie;
			break;
	
		default:
			ModErrThrow();
			break;

		}
		
		gpsPortId = PFPortOpen(portID, gpsBaudRate);

	} else if (gpsSource == gpsBluetooth) {

		gpsPortId = PFPortOpenBluetooth(btID, true);
		
	} else if (gpsSource == gpsCard) {

		return ERR_GPS_NO_CARD;
		
#ifdef GPS_CARD_SUPPORT

		UInt32  cardPortRef;
		UInt16  slotRef;
		UInt32  slotIterator = expIteratorStart;
		Boolean foundSerial = false;
		UInt32 featureVersion;

		if (FtrGet(sysFileCExpansionMgr, expFtrIDVersion, &featureVersion) != errNone) {
		
			LOGEXIT;
			return ERR_GPS_NO_CARD;

		}

		/*
		 * check all the slots on the device. If we find a slot with
		 * a card in it then check if the card has serial capabilities
		 * and if it does use it. Otherwise, return an error
		 *
		 */

		while (slotIterator != expIteratorStop && !foundSerial) {
			Err error;

			error = ExpSlotEnumerate(&slotRef, &slotIterator);
			if (error == errNone) {
				if (ExpCardPresent(slotRef) == errNone) {
					ExpCardInfoType ci;

					ExpCardInfo(slotRef, &ci);
					if (ci.capabilityFlags == expCapabilitySerial)
						foundSerial=true;
				}
			}
		}
		if (!foundSerial) {

			LOGEXIT;
			return ERR_GPS_NO_CARD;

		}

		ExpCardGetSerialPort(slotRef, &cardPortRef);
		error = SrmOpen(cardPortRef, baud, &gpsPortId);
		if (error) {

			LOGEXIT;
			return error;

		}
		SrmSetReceiveBuffer(gpsPortId, serialBuffer, sizeof(serialBuffer));
#endif
		
	} else if (gpsSource == gpsGarmin) {

		if (!GarminGPSOpen()) return ERR_GPS_NO_GARMIN;

	} else
		GpsErrThrow;

	currBuffSize = 0;

	if (resetTimers) {
		lastCommsTime = PFGetTicks();
		lastFixTime = 0;
	}

	LOGEXIT;
	return 0;
}


/*
 * function : DecodePMGNST
 *
 * Decodes the proprietary Magellan status message. 
 *
 * Format of PMGNST message:
 *
 * 	Field	Data
 * 	-----	----
 * 	1	Version
 * 	2	1,2 or 3 for locktype (1=no lock)
 * 	3	T/F for position fix
 * 	4	??
 * 	5	Battery in hours 00.0 to 10.0 for Companion
 * 	6	??
 * 	7	PRN under consideration
 */
static void DecodePMGNST(char *stMsg) {
	char *fieldPtr;
	UInt16 j;

	/* skip to battery field */
	for (j=0;j<5;j++) {
		fieldPtr = GetField(&stMsg);
	}

	//batData->battLevel = (Int8)(StrToDouble(fieldPtr)*10);
}

/*
 * function : DecodeGSA
 *
 * Decodes a GPGSA Message. 
 *
 * Format of a GSA message:
 *	Field	Data
 *	-----	----
 *	1	Mode M - Manual, A-Automatic
 *	2	Fix type, 1=NA, 2=2d, 3=3d
 *	3-14	Satellite PRN number used in solution
 *	15      PDOP
 *	16	HDOP   (all 1.0 to 99.0)
 *	17	VDOP
 *
 */
static void DecodeGSA(char *gsaMsg) {
	char *fieldPtr;
	UInt16 j;
	
	GPS.sat.pdop = 100.0;
	GPS.sat.hdop = 100.0;
	GPS.sat.vdop = 100.0;

	/* skip field 1 */
	fieldPtr = GetField(&gsaMsg);

	/* get fix type */
	fieldPtr = GetField(&gsaMsg);
	if (!fieldPtr)
		return;

	switch (fieldPtr[0]) {
		case '2':
			GPS.sat.fixType=2;
			break;

		case '3':
			GPS.sat.fixType=3;
			break;
		default:
			GPS.sat.fixType=1;
			break;
	}

	/*
	 * skip 3-14
	 *
	 */

	for (j=2;j<14;j++) fieldPtr = GetField(&gsaMsg);
	
	/*
	 * P,H & V Dilution of precision
	 *
	 */

	fieldPtr = GetField(&gsaMsg);
	if (fieldPtr[0] != 0) GPS.sat.pdop = (float)StrToDouble(fieldPtr);
	
	fieldPtr = GetField(&gsaMsg);
	if (fieldPtr[0] != 0) GPS.sat.hdop = (float)StrToDouble(fieldPtr);
	
	fieldPtr = GetField(&gsaMsg);
	if (fieldPtr[0] != 0) GPS.sat.vdop = (float)StrToDouble(fieldPtr);
	
}

/*
 * function : DecodeGSV
 *
 * Decodes a GSV (satellites in view) message from the GPS, into the
 * satData structure.
 *
 * *satData is only updated when all GSV messages have been received i.e.
 * field 1 == field 2
 *
 * Format of a GSV message:
 *	Field	Data
 *	-----	----
 *	1	Total GSV messages (1-3)
 *	2	GSV Message Id (1-4)
 *	3	Satellites in view (0-12)
 *	4	Satellite number (1-32)
 *	5	Elevation (0-90)
 *	6	Azimuth (0-359)
 *	7	Signal-to-noise ratio 0-99 or NULL
 *	8	Checksum
 *
 * Fields 4-7 repeat up to a maximum of 4 satellites per message
 * 	
 * Returns true if the message is the last of the sequence
 *
 */
static Boolean DecodeGSV(char *gsvMsg) {
	char 			*fieldPtr;
	Boolean 		eom = false;
	UInt8			satId;
	UInt8 			totalMsg, msgId;
	static GPSSatData 	sdNew;

	/* 
	 * first two fields, total GSV messages and message number, 
	 * determine if this is the start of a new GSV sequence 
	 * (message number =1) or the last (message number = total)
	 *
	 */
	
	fieldPtr = GetField(&gsvMsg);
	if (!fieldPtr)
	    return false;

	totalMsg = StrAToI(fieldPtr);
	fieldPtr = GetField(&gsvMsg);
	msgId = (UInt8) StrAToI(fieldPtr);

	/* if start of GSV sequence, initialise working copy of satData 
	 * structure
	 */
	if (msgId == 1) {
		PFMemSet(sdNew.stnr, sizeof(sdNew.stnr), (Int8)-1);
		PFMemSet(sdNew.elevation, sizeof(sdNew.elevation), (Int16)-1);
		PFMemSet(sdNew.azimuth, sizeof(sdNew.azimuth), (Int16)-1);
		sdNew.viewSatCount = 0;
		sdNew.useSatCount = 0;
	}
	
	/* sat count field */
	fieldPtr = GetField(&gsvMsg);
	if (!fieldPtr) 
		return false;

	sdNew.viewSatCount = (UInt16) StrAToI(fieldPtr);

	do {

		Int16 elevation, azimuth;

		/* if we couldn't read the next field at this stage, then we 
		 * have reached the checksum 
		 */

		fieldPtr = GetField(&gsvMsg);
		if (!fieldPtr || fieldPtr[0]==0) {
			eom = true;
			break;
		}

		satId = (UInt8) StrAToI(fieldPtr);

		/* get elevation */
		fieldPtr = GetField(&gsvMsg);
		if (!fieldPtr) {
			eom = true;
			break;
		}

		elevation = -1;
		if (fieldPtr[0] != 0) elevation = StrAToI(fieldPtr);

		/* get azimuth */
		fieldPtr = GetField(&gsvMsg);
		azimuth = -1;
		if (fieldPtr[0] != 0) azimuth = StrAToI(fieldPtr);

		/* get sig-to-noise ratio */
		fieldPtr = GetField(&gsvMsg);
		if (fieldPtr && fieldPtr[0] != 0 && satId>0 && satId<=GPSNumSats){
			
			Int16 stnr = (Int16) StrAToI(fieldPtr);

			LOGINT16(satId);
			LOGINT16(stnr);
			LOGINT16(elevation);
			LOGINT16(azimuth);

			sdNew.stnr[satId] = (Int8) stnr;
			sdNew.elevation[satId] = elevation;
			sdNew.azimuth[satId] = azimuth;
			sdNew.useSatCount++;

			
		} 

	} while (!eom);

	/* if last message in GSV sequence then update *satData */
	if (msgId == totalMsg) {

		/*
		 * NOTE: (ref. note in header file)
		 * 
		 * copy fields from current satData that are not set as a
		 * result of the GSV message sequence
		 *
		 */

		sdNew.hdop    = GPS.sat.hdop;
		sdNew.vdop    = GPS.sat.vdop;
		sdNew.pdop    = GPS.sat.pdop;
		sdNew.fixType = GPS.sat.fixType;
		sdNew.waas    = GPS.sat.waas;

		GPS.sat = sdNew;

		return true;
	}

	return false;
	
}

/*
 * function : DecodeGGA
 *
 * Decodes the GGA type message. We are only interested in fields 9 and 10, 
 * relating to altitude. Returns true if a valid GGA message which indicates
 * altitude.
 *
 * Format of a GGA message:
 *	Field	Data
 *	-----	----
 *	1	UTC time
 *	2	Lat ddmm.mmmm
 *	3	Lat Hemisphere (N/S)
 *	4	Lon dddmm.mmmm
 *	5	Lon Hemisphere (E/W)
 *	6	Posn Fix Indicator 0 = NA, 1 = GPS SPS Mode, 2 = D-GPS, 
 *			3 = GPS PPS
 *	7	Satellites in use 0 - 12
 *	8	Horizontal precision 0.5 - 99.9
 *	9	MSL Altitude -9999.9 to 99999.9 metres
 *	10      M
 *	11	Geoidal Height -999.9 to 9999.9 metres
 *	12      M
 *	13	D-GPS data
 *	14	Checksum
 */
static Boolean DecodeGGA(char *gga) {
	char *fieldPtr;
	float altitude;
	UInt8 j;

	LOGENTRY;

	/* skip fields 1-5 */
	for (j=1;j<7;j++)
		fieldPtr = GetField(&gga);

	/*
	 * WAAS indication
	 *
	 */

	GPS.sat.waas = (fieldPtr != NULL && fieldPtr[0] == '2');

	/*
	 * skip 7 & 8
	 *
	 */

	for (j=7;j<10;j++)
		fieldPtr = GetField(&gga);

	/* field 9 should now be in fieldPtr */
	if (fieldPtr == NULL || fieldPtr[0] == 0 ) {

		LOGEXIT;

		return false;
	}

	altitude = (float)(StrToDouble(fieldPtr));

	LOGINT32((Int32)altitude);

	altitude *= FEET_PER_METRE;
	GPS.posn.deltaAltitude = (altitude - GPS.posn.rawAltitude) / GPS.posn.deltaTime;
	GPS.posn.rawAltitude = altitude;
	GPS.posn.altitude = altitude + gpsAltCorrection;

	LOGINT32((Int32)GPS.posn.deltaAltitude);
	LOGEXIT;

	return true;
}


/*
 * function : DecodeRMC
 *
 * Decodes the RMC data message.
 *
 * Returns true if the message indicates a position, false if
 * not.
 *
 * Format of an RMC message:
 *	Field	Data
 *	-----	----
 *	1	UTC time of fix
 *	2	Status A=valid, V=invalid
 *	3	Lat ddmm.mmmm
 *	4	Lat hemisphere N , S
 *	5	Lon dddmm.mmmm
 *	6	Lat Hemisphere E , W
 *	7	Speed 0.0 to 1851.8 knots
 *	8	Course 000.0 to 359.9 degrees true
 *	9	Date ddmmyy
 *	10	Mag Variation 000.0 to 180.0
 *	11	Degrees (?)
 *	12	Checksum
 */
static Boolean DecodeRMC(char *rmc) {
	char 	*fieldPtr;
	char 	*timePtr;
	Int32	tempi;
	double	tempd;
	float   heading, magVarn, magHeading;
	float   oldTime = GPS.posn.utc.second + GPS.posn.utc.milliSecond /1000;
	float   deltaTime;

	/* get UTC field */
	timePtr = GetField(&rmc);
	GPS.posn.utc.hour = (UInt8) GetNum(timePtr,2);
	GPS.posn.utc.minute = (UInt8) GetNum(&timePtr[2],2);
	GPS.posn.utc.second = (UInt8) GetNum(&timePtr[4],2);

	LOGLINE;

	/*
	 * check for decimal point in time field, if so
	 * we have more precision to deal with...
	 *
	 */

	if (!Preferences.oneSecUpdate && timePtr[6] == '.') {
		
		GPS.posn.utc.milliSecond = (timePtr[7]-'0') * 100;
		if (timePtr[8]!=0) GPS.posn.utc.milliSecond += 10*(timePtr[8]-'0');

		/*
		 * calculate delta time; i.e. time between RMC messages
		 *
		 * If this code changes, check the corresponding code in
		 * ReadFromGarminGPS.
		 *
		 */

		deltaTime = (GPS.posn.utc.second + GPS.posn.utc.milliSecond / 1000) - oldTime;
		if (deltaTime < 1.0) {
		
			if (deltaTime < 0.0) 
				deltaTime += 60.0;
			else 
				deltaTime = 1.0;
			
		}
		if (deltaTime > 10.0) deltaTime = 10.0;

	} else {

		GPS.posn.utc.milliSecond = 0;
		deltaTime = 1.0;
	
	}

	LOGINT32(deltaTime*100);
	GPS.posn.deltaTime = deltaTime;

	/* get status field */
	fieldPtr = GetField(&rmc);
	if (!fieldPtr || fieldPtr[0] != 'A') {
		/* rest of message is invalid, don't process */
		return false;
	}

	LOGLINE;
	/* read latitude, convert ddmm.mmmm to radians */
	fieldPtr = GetField(&rmc);
	if (!fieldPtr || !fieldPtr[0]) return false;

	tempi = GetNum(fieldPtr,4);
	tempd = (double) ( (double)(tempi/100) + (double)(tempi % 100) / 60) ;
	tempd += (double)GetNum(&fieldPtr[5],4) / 600000;

	LOGLINE;
	StrCopy(&GPS.posn.northStr[1], fieldPtr);

	/* get N/S indicator */
	fieldPtr = GetField(&rmc);
	if (!fieldPtr || !fieldPtr[0]) return false;
	if (fieldPtr[0] == 'S')
		tempd = -tempd ;

	GPS.posn.northStr[0] = fieldPtr[0];

	GPS.posn.latitude = (tempd/180)*PI;
	GPS.posn.lat32 = RAD_TO_INT32(GPS.posn.latitude);

	/* read longitude, convert dddmm.mmmm to radians */
	fieldPtr = GetField(&rmc);
	if (!fieldPtr || !fieldPtr[0]) return false;

	tempi = GetNum(fieldPtr,5);
	tempd = (double) ( (tempi/100) + (double)(tempi % 100) / 60) ;
	tempd += (double) GetNum(&fieldPtr[6],4) / 600000;

	LOGLINE;
	StrCopy(&GPS.posn.eastStr[1], fieldPtr);

	/* get E/W indicator */
	fieldPtr = GetField(&rmc);
	if (!fieldPtr || !fieldPtr[0]) return false;

	if (fieldPtr[0] == 'W')
		tempd = -tempd;

	GPS.posn.eastStr[0] = fieldPtr[0];

	GPS.posn.longitude = (tempd/180)*PI;
	GPS.posn.lon32 = RAD_TO_INT32(GPS.posn.longitude);

	LOGLINE;
	/* get groundspeed */
	fieldPtr = GetField(&rmc);
	//if (!fieldPtr || !fieldPtr[0]) return false;

	GPS.posn.speed = (float)StrToDouble(fieldPtr);

	/* get heading */
	fieldPtr = GetField(&rmc);
	//if (!fieldPtr || !fieldPtr[0]) return false;

	heading = (float)StrToDouble(fieldPtr);

	/*
	 * fix for FS2002
	 *
	 */

	WRAPMAX(heading,360.0);

	/* 
	 * date
	 *
	 */

	fieldPtr = GetField(&rmc);
	//if (!fieldPtr || !fieldPtr[0]) return false;

	if (fieldPtr[0] != 0) {

		//DUMP_STR(fieldPtr,0,20);
		GPS.posn.utc.day = GetNum(&fieldPtr[0], 2);
		GPS.posn.utc.month = GetNum(&fieldPtr[2], 2);

		if (StrLen(&fieldPtr[4]) == 2) {

			GPS.posn.utc.year = 2000 + GetNum(&fieldPtr[4], 2);

		} else {

			GPS.posn.utc.year = GetNum(&fieldPtr[4], 4);

		}


#ifdef DEMO
		//GpsErrThrowIfExt(GPS.posn.utc.year > 2019 || GPS.posn.utc.year < 2006,fieldPtr);
#endif

	}

	/*
	 * get mag variation (two parts, number and W/E indicator
	 *
	 */
	
	fieldPtr = GetField(&rmc);

	LOGLINE;
	
	if ( !magVarnCalculated || (cycleCounter & 0x4F) == 0) {

		LOGTAG("MagVarn");

		magVarnCalculated = true;
		magVarn = AvCalcMagVarn(GPS.posn.latitude, GPS.posn.longitude,
				GPS.posn.rawAltitude/FEET_PER_METRE,2008);

		LOGTAG("MagVarn");

	} else {

		magVarn = GPS.posn.magVarn;

	}

	LOGLINE;
	GPS.posn.magVarn = magVarn;
	magHeading = heading - magVarn;

	WRAPMAX(magHeading,360);

	if (GPS.posn.speed>1.0) {

		GPS.posn.deltaHeading = (heading - GPS.posn.trueHeading) / GPS.posn.deltaTime;

		if (GPS.posn.deltaHeading > 180) 
			GPS.posn.deltaHeading -= 360;
		else if (GPS.posn.deltaHeading < -180) 
			GPS.posn.deltaHeading += 360;

		GPS.posn.trueHeading = heading;
		GPS.posn.magHeading = magHeading;

	}
	
	return true;
}

	

/*
 * function : DecodeGPSMsg
 *
 * Decodes the following GPS Message types:
 * 	GPGGA into GPS.posn.altitude
 * 	GPRMC into GPS.posn.north, east, latitude, longitude, speed, trueHeading, 
 * 		magHeading
 * 	GPGSV into GPS.sat.stnr[xxx]
 * 	GPGSA
 * 	PMGNST
 *
 * Ignores all other messages
 *
 * Returns type of NMEA message that was decoded.
 *
 */

static GPSMessageType DecodeGPSMsg(char *startOfMsg) {

	GPSMessageType msgType = gpsUnknown;

	LOGENTRY;

	if (StrNCompare(&startOfMsg[1], "GPGSV", 5) == 0) {

		/*
		 * DecodeGSV only returns true when the last message
		 * of the sequence is received
		 *
		 */

		if (DecodeGSV(&startOfMsg[7])) {
			msgType = gpsGSV;
		}

	} else if (StrNCompare(&startOfMsg[1], "GPGSA",5) == 0) {
		msgType = gpsGSA;
		DecodeGSA(&startOfMsg[7]);
	} else if (StrNCompare(&startOfMsg[1], "GPGGA", 5) == 0) {
		if (DecodeGGA(&startOfMsg[7])) {
			msgType = gpsGGA;
		} else {
			msgType = gpsInvGGA;
		}

	} else if (StrNCompare(&startOfMsg[1], "GPRMC", 5) == 0) {
		
		if (DecodeRMC(&startOfMsg[7])) {
			msgType = gpsRMC;

		} else {
			msgType = gpsInvRMC;
		}

	} else if (StrNCompare(&startOfMsg[1], "PMGNST", 6) == 0) {
		msgType = gpsST;
		DecodePMGNST(&startOfMsg[8]);
	}

	LOGEXIT;

	return msgType;
}

/*
 * function : UpdateStateMachine
 *
 * Updates the gpsModuleState variable. Sends evtGPSXXX messages when the state
 * changes, and can send two or even three of these in a single call if the
 * state change is extreme enough. E.g. going from no comms to a 3-d fix
 * would produce evtGPSSync, evtGPSFix, evtGPSAltFix in a single pass.
 *
 * Should be called for *every* GPS message which is processed.
 * 
 * It is important to note that this state machine is driven by
 * changes to GPS.sat.fixtype.
 *
 */

static void UpdateStateMachine(Boolean gpsMessageReceived) {

	EventType e;
	Boolean   tumble;
	UInt16    tumbleTTL = 4;

	/*
	 * tumble/tumbleTTL control a kind of tumbling motion though the state
	 * machine.  E.g. it is possible for the state to tumble from 3d -->
	 * 2d--> Sync --> timeout in a single call of the function.
	 *
	 * tumbleTTL (time to live) is a guard against constant tumbling, if it
	 * hits 0, generate a module error.
	 *
	 */
	
	LOGENTRY;

	LOGINT16((Int16)gpsMessageReceived);

	/*
	 * state machine monitors covering timeouts, one for last fix time and
	 * one for last comms time (i.e. last valid GPS message). Either of
	 * these two timeouts can reduce the fix type...
	 *
	 */

	if (GPS.sat.fixType > 0) {
		if ((currCycleTicks - lastFixTime) > PFTicksPerSecond() * GPS_COMMS_TIMEOUT)
			GPS.sat.fixType = 1;
		if ((currCycleTicks - lastCommsTime) > PFTicksPerSecond() * GPS_COMMS_TIMEOUT )
			GPS.sat.fixType = 0;
	}

	GPS.sat.heartBeat = true;

	do {

	tumble = false;

	LOGINT16((Int16)gpsModuleState);
	LOGINT16((Int16)GPS.sat.fixType);

	switch (gpsModuleState) {

	case gpsOff:
		break;

	case gpsSyncing:
		if (gpsMessageReceived) {
			
			LOGLINE;
			gpsModuleState = gpsSync;
			PFSendSimpleEvent(evtGPSSync);

			/*
			 * might get into a 2-d or 3-d fix from here
			 *
			 */

			tumble = true;

		} else {

			LOGLINE;
			/*
			 * if using a serial port, then we may need to select
			 * the next baud rate for the auto-scanning
			 *
			 */

			if ((gpsSource == gpsCradle || gpsSource == gpsSerial) && (currCycleTicks - lastCommsTime) > 
				PFTicksPerSecond() * BAUD_SWITCH_TIMEOUT) {
				/* switch to the next baud rate */
				LOGTAG("Baud");
				switch (gpsBaudRate) {
				case 4800:
					gpsBaudRate = 19200;
					break;
					
				case 19200:
					gpsBaudRate = 57600;
					break;
					
				case 57600:
				default:
					gpsBaudRate = 4800;
					break;
				}
				
				PFPortClose(gpsPortId);
				OpenGPSPort(gpsBaudRate,false, NULL);
				lastCommsTime = currCycleTicks;
				
			}
			GPS.sat.heartBeat = false;
		}
		break;

	case gpsSync:

		/*
		 * check to see that we've got current comms from the GPS
		 *
		 */

		if (GPS.sat.fixType == 0) {

			gpsModuleState = gpsTimeout;

			PFSendSimpleEvent(evtGPSSyncLost);

			/*
			 * no point tumbling from timeout state
			 *
			 *
			 * tumble = false;
			 *
			 */


		} else if (GPS.sat.fixType > 1) {

			/*
			 * check the UTC date stamp against the demo date limit before
			 * allowing the 2-d fix to occur
			 *
			 */

			UInt32 utcDate = (UInt32) GPS.posn.utc.day + (UInt32) GPS.posn.utc.month * 100 +
				(UInt32)GPS.posn.utc.year * 10000;

			//DUMP_INT32(utcDate,0,20);
			//DUMP_INT32(dateLimit,0,40);
			
			if (utcDate < DemoDateLimit) {

				PFSendSimpleEvent(evtGPSFix);
				gpsModuleState = gps2d;
				tumble = true;

			} else {

				GPS.sat.fixType = 1;

			}

#ifdef OLDCODE

			e.eType = evtGPSFix;
			PFEventSend(&e);
			gpsModuleState = gps2d;

			/*
			 * got a 2d fix, could tumble into 3d
			 *
			 */

			tumble = true;

#endif

		}
		break;

	case gps2d:
		
		if (GPS.sat.fixType == 3) {

			PFSendSimpleEvent(evtGPSAltFix);
			gpsModuleState = gps3d;

			/*
			 * can't tumble beyond 3d!!
			 *
			 * 
			 * tumble = false;
			 */

		} else if (GPS.sat.fixType < 2 ) {

			PFSendSimpleEvent(evtGPSFixLost);
			gpsModuleState = gpsSync;

			/*
			 * fix changed, could tumble into timeout or sync state
			 *
			 */

			tumble = true;
		}
		
		break;

	case gps3d:

		if (GPS.sat.fixType != 3) {

			PFSendSimpleEvent(evtGPSAltFixLost);
			gpsModuleState = gps2d;

			/*
			 * fix changed, could tumble into 2-d, timeout or sync state
			 *
			 */

			tumble = true;

		}
		break;

	case gpsTimeout:

		if (gpsMessageReceived) {
			PFSendSimpleEvent(evtGPSSync);
			gpsModuleState = gpsSync;

			/*
			 * could tumble all the way to 3d
			 *
			 */

			tumble = true;
		} else {
			GPS.sat.heartBeat=false;
		}
		break;
	}

	} while (tumble && tumbleTTL--);

	LOGINT16((Int16)gpsModuleState);

	if (gpsModuleState == gps2d || gpsModuleState == gps3d) {

		if ( !magVarnCalculated || (cycleCounter & 0x4F) == 0) {

			LOGTAG("MagVarn");

			magVarnCalculated = true;
			GPS.posn.magVarn = AvCalcMagVarn(GPS.posn.latitude, GPS.posn.longitude,
					GPS.posn.rawAltitude/FEET_PER_METRE,2008);

			LOGTAG("MagVarn");

		}
		
	}

	if (!tumbleTTL) GpsErrThrow;

	LOGEXIT;
}


/*
 * function : ReadFromGPS
 *
 * Reads all available data from the GPS source onto end of gpsMessageBuff.
 * Tries to make sure that the first character of the buffer is a $ (which
 * indicates the start of a NMEA sentence), otherwise if no $ in the buffer
 * then the buffer is discarded.
 *
 * Returns true if some data was read from the GPS, false if not
 *
 */

static Boolean ReadFromGPS(void) {
	UInt32 length;
	char *dollar;

	LOGENTRY;

	/*
	 * check the input for errors and availability of data. If there are
	 * errors, then reset the input lines.
	 *
	 * If the new serial manager is available, then Bluetooth and serial
	 * comms can use the same API.
	 *
	 */

	//StrCopy(gpsMessageBuff,"$GPVTG,262$GPGGA,172838.000,3409.3401,N,11828.7720,W,1,04,3.0,209.4,M,-33.5,M,,0000*61\r\n");
	//currBuffSize = StrLen(gpsMessageBuff);
	//LOGEXIT;
	//return true;



	length = sizeof(gpsMessageBuff)-currBuffSize - 1;

	if (length > 0) {
		
		length = PFPortRead(gpsPortId, &gpsMessageBuff[currBuffSize], length);

	}
	
	if (gpsLog) {

		PFWriteFile(gpsLog, &gpsMessageBuff[currBuffSize], length);

	}

	currBuffSize+=length;
	gpsMessageBuff[currBuffSize]=0;	/* NULL-terminate the string */


	/* 
	 * if first char in message buffer != $ then find $ 
	 * and shift it and the following characters to the start of 
	 * GPS message buffer 
	 * 
	 */

	if (gpsMessageBuff[0] != '$') {
		dollar = StrChr(gpsMessageBuff, '$');
		if (dollar) {
			currBuffSize -= (dollar-gpsMessageBuff);
			PFMemMove(gpsMessageBuff, dollar, currBuffSize+1); 
					/* +1 to get terminating 0 */
		} else {

			/* 
			 * No $ in the buffer: may as well discard the
			 * data
			 *
			 */

			currBuffSize = 0;
			gpsMessageBuff[0] = 0;
		}
	}
	
	LOGEXIT; 
	return length > 0;

}


/*
 * function : RunSimulation
 *
 * Updates the simulation data
 *
 */

static void RunSimulation(void) {

	UInt32         utcTime; 
	float		headingDiff;
	static UInt32   lastTicks;
	float lapse = MIN((float) (currCycleTicks - lastTicks) / (float)PFTicksPerSecond(), 2.0);


	LOGENTRY;

	LOGINT32(currCycleTicks);
	LOGINT32(lastTicks);

	utcTime = PFGetSeconds() - (UInt32)Preferences.localTimeZone*30*60;
	
	LOGINT16((Int16)(lapse*10));
	lastTicks = currCycleTicks;

	lastCommsTime = currCycleTicks;
	lastFixTime = currCycleTicks;

	GPS.sat.viewSatCount=12;
	GPS.sat.useSatCount=12;

	/*
	 * time
	 *
	 */

	PFSecondsToTimeStamp(utcTime, &GPS.posn.utc);

	GPS.sat.fixType = 3;

	/*
	 * update speed
	 *
	 */

	if (simulation.speed < simulation.targetSpeed)  {

		simulation.speed = MIN(simulation.speed+maxAcceleration,simulation.targetSpeed);

	} else if (simulation.speed > simulation.targetSpeed) {

		simulation.speed = MAX(simulation.speed-maxAcceleration, simulation.targetSpeed);

	}

	/*
	 * update heading
	 *
	 */

	headingDiff = simulation.targetHeading - simulation.heading;
	LOGINT16((Int16)headingDiff);
	LOGINT16((Int16)simulation.targetHeading);
	LOGINT16((Int16)simulation.heading);

	if (headingDiff > 180) headingDiff -=360;
	else if (headingDiff < -180) headingDiff += 360;
		
	if (headingDiff > 0) {

		headingDiff = MIN(rateOfTurn*lapse, headingDiff);
		simulation.heading += headingDiff;
		if (simulation.heading >= 360) simulation.heading -= 360;

	} else if (headingDiff < 0) {

		headingDiff = MAX(headingDiff, -rateOfTurn*lapse);

		LOGINT16((Int16)headingDiff);
		simulation.heading += headingDiff;
		if (simulation.heading < 0) simulation.heading += 360;

	}
		

	/*
	 * update position, altitude & climbrate
	 *
	 */
	
	AvCalcShiftPoint(&simulation.lat, &simulation.lon,
			DEG_TO_RAD((double)simulation.heading),
			NM_TO_RAD((double)simulation.speed/3600)*lapse);

	if (simulation.altitude < simulation.targetAltitude) {

		simulation.altitude = MIN(simulation.altitude+rateOfClimb*lapse/60,
				simulation.targetAltitude);

	} else if (simulation.altitude > simulation.targetAltitude) {

		simulation.altitude = MAX(simulation.altitude-rateOfClimb*lapse/60,
				simulation.targetAltitude);

	}
	
	/*
	 * deltaTime:
	 * if the corresponding code in DecodeRMC changes, don't forget to change
	 * this code also!!
	 *
	 */
	
	GPS.posn.deltaTime = lapse;

	GPS.posn.speed = (float)simulation.speed;
	GPS.posn.latitude = simulation.lat;
	GPS.posn.longitude = simulation.lon;

	GPS.posn.lat32    = RAD_TO_INT32(simulation.lat);
	GPS.posn.lon32    = RAD_TO_INT32(simulation.lon);

	GPS.posn.deltaAltitude = (simulation.altitude - GPS.posn.rawAltitude) / GPS.posn.deltaTime;
	GPS.posn.rawAltitude = simulation.altitude;
	GPS.posn.altitude = simulation.altitude + gpsAltCorrection;
	
	LOGINT32(GPS.posn.altitude);
	LOGINT32(simulation.altitude);

	/*
	 * position to text format, e.g. N5140.3780 W00203.3800
	 *
	 */

	GPSCoordsToText(GPS.posn.latitude, GPS.posn.longitude, GPS.posn.northStr, GPS.posn.eastStr);

	/*
	 * periodically calculate Magnetic Variation
	 *
	 */

	LOGLINE;

	if (GPS.posn.speed > 0) {
		
		float newHeading = simulation.heading;
		float magHeading;

		LOGLINE;

		GPS.posn.deltaHeading = (newHeading - GPS.posn.trueHeading) / GPS.posn.deltaTime;

		if (GPS.posn.deltaHeading > 180) GPS.posn.deltaHeading -= 360;
		else if (GPS.posn.deltaHeading < -180) GPS.posn.deltaHeading += 360;

		GPS.posn.trueHeading = newHeading;

		magHeading = newHeading - GPS.posn.magVarn;

		if (magHeading>=360) magHeading -= 360;
		else if (magHeading<0) magHeading += 360;

		GPS.posn.magHeading  = magHeading;
	}
	
	LOGEXIT; 
}

/*******************************************************************************
 * 
 * public functions
 *
 */

/*
 * function : GPSInit
 *
 */

Err GPSInit(GPSSourceType source, PFBluetoothAddressType *bluetoothID) {

	Err 	error;

	LOGENTRY;

	cycleCounter = 0;

	/*
	 * must set gpsSource here, OpenGPSPort will pick it up
	 *
	 */

	gpsSource = source;

	GPS.sat.fixType = 0;
	gpsModuleState = gpsSyncing;
	magVarnCalculated = false;

	/*
	 * simulation mode doesn't need ports opening etc.
	 *
	 */

	if (gpsSource == gpsSimulate) {
		
		simulation.heading = simulation.targetHeading;
		simulation.altitude = simulation.targetAltitude;
		simulation.speed = simulation.targetSpeed;
		return errNone;

	}

	error = OpenGPSPort(gpsBaudRate,true, bluetoothID) ;
	if (error!=errNone) {

		LOGEXIT;
		return error;

	}

#ifdef GPSLOG
	if (gpsLog = PFOpenFile("/FM-GPS.txt", pfFileReadWrite)) {

		PFSeekFile(gpsLog, pfSeekEnd, 0);

	}
#endif

	LOGEXIT;
	
	return errNone;
}

/*
 * function : GPSSourceSupported
 *
 */

Boolean GPSSourceSupported(GPSSourceType source) {

	UInt32 featureVersion;

	Boolean supported = true;

	switch (source) {

	case gpsBluetooth:
		if (FtrGet(btLibFeatureCreator, btLibFeatureVersion, &featureVersion) != errNone)
			supported = false;
		break;

	case gpsCard:
		if (FtrGet(sysFileCExpansionMgr, expFtrIDVersion, &featureVersion) != errNone)
			supported = false;
		break;

	case gpsGarmin:

		if (GarminGPSOpen()) {
			
			GarminGPSClose();
			
		} else {
			
			supported = false;
			
		}
		break;

	default:
		supported = true;
		break;

	}
	return supported;
}

/*
 * function : GPSProcess
 *
 */

Err GPSProcess(GPSDutyType dutyCycle, EventPtr event) {

	char 	*crlf;
	UInt16  sleep;
	GPSMessageType msgType;
	Boolean gpsMessageReceived;
	EventType e;
	UInt32  listenCycles;

	LOGENTRY;

	if (gpsModuleState == gpsOff)
		GpsErrThrow;

	cycleCounter ++;

	/*
	 * Garmin iQue & simulation mode
	 *
	 * Simulator should never send a nilEvent - it's not
	 * needed.
	 *
	 */

	if (gpsSource == gpsGarmin || gpsSource == gpsSimulate) {

		static UInt32 lastRead = 0;
		
		LOGTAG("Garmin/Sim");

		currCycleTicks = PFGetTicks();

		while (!PFGetEvent(event,PFTimerTicksLeft(lastRead, PFTicksPerSecond()/SimRate))) {
			
			if (PFTimerHasExpired(lastRead, PFTicksPerSecond()/SimRate)) {
				
				EventType e;
				Boolean success;

				LOGTAG("Reading");

				if (gpsSource == gpsGarmin) {
					
					success = GarminGPSRead(&GPS);
					if (success) lastCommsTime = currCycleTicks;
					if (GPS.sat.fixType >= 2) lastFixTime = currCycleTicks;
					GPS.posn.altitude = GPS.posn.rawAltitude + gpsAltCorrection;

				}  else {
					
					LOGTAG(StrSim);
					success = true;
					RunSimulation();

				}

				if (success) {

					PFSendSimpleEvent(evtGPSSatUpdate);
					PFSendSimpleEvent(evtGPSPositionUpdate);
				
					UpdateStateMachine(true);

				} else {

					UpdateStateMachine(false);

				}

				lastRead = PFGetTicks();
				
			}

		}

		LOGEXIT;
		
		return errNone;

	}

	LOGTAG("Non-Garmin/Sim");

	if (dutyCycle == dutyNormal)
		listenCycles = 16;
	else
		listenCycles = 32;

	sleep = PFTicksPerSecond() / listenCycles;

	do {

		currCycleTicks = PFGetTicks();
	
		LOGTAG("Reading");
		if (!ReadFromGPS()) {

			/*
			 * call UpdateStateMachine to allow timeouts to generate state
			 * changes
			 *
			 */

			UpdateStateMachine(false);
			PFGetEvent(event, sleep);
			continue;
		}

		/*
		 * GPS messages start with '$' and end with '\r\n'. So we can
		 * process the message buffer as long as the first char and
		 * end chars exist in the buffer
		 *
		 */

		crlf = StrChr(gpsMessageBuff, 13);
		gpsMessageReceived = false;

		while (currBuffSize && gpsMessageBuff[0] == '$' && crlf && crlf[1] == 10) {

			char *asterisk;

			/* 
			 * found CR/LF, extract GPS message & decode it.
			 * Safe to mark the end of the string with \0
			 *
			 */

			*crlf = 0;

			LOGSTR(gpsMessageBuff);

			asterisk = StrChr(&gpsMessageBuff[1], '*');

			/*
			 * discard if no checksum, or if message >100 chars -
			 * this fixes bug with Holux where the truncated RMC
			 * message arrives with another GPS message appended,
			 * and the checksum of the other message just happens
			 * to be right for the entire string!
			 *
			 */

			if (!asterisk || asterisk - gpsMessageBuff > 100) {

				/*
				 * corrupted - discard message
				 *
				 */

				LOGTAG("Discarded");
				msgType = gpsUnknown;

			} else {

				/*
				 * perform checksum function on message, before attempting to
				 * decode it
				 *
				 * The GPS checksum is two characters after the '*' e.g. *5F
				 *
				 * It is calculated by XOR'ing all characters between the '$'
				 * and the '*'
				 *
				 */

				const char hex[17] = "0123456789ABCDEF";
				UInt8 checksum = 0;
				char *c = gpsMessageBuff + 1;

				while ( c < asterisk) checksum ^= *c++;

				if (hex[checksum>>4] == asterisk[1] && hex[checksum & 15] == asterisk[2]) {

					msgType = DecodeGPSMsg(gpsMessageBuff);

				} else {

					/*
					 * checksum failed
					 *
					 */

					msgType = gpsUnknown;

				}

			}

			if (msgType != gpsUnknown) {

				/*
				 * set fixType = 1, to indicate incoming GPS messages
				 *
				 */

				LOGLINE;

				LOGINT16((Int16)msgType);

				if (GPS.sat.fixType == 0) 
					GPS.sat.fixType = 1;

				lastCommsTime = currCycleTicks;

				switch (msgType) {
				case gpsInvGGA:

					/*
					 * No altitude? Reduce fixtype to 2d
					 *
					 */

					if (GPS.sat.fixType == 3)
						GPS.sat.fixType = 2;
					break;

				case gpsGGA:

					/*
					 * with valid altitude indication,
					 * we've definetely got a 3d fix...
					 *
					 */

					if (GPS.sat.fixType == 2)
						GPS.sat.fixType = 3;
					break;

				case gpsRMC:

					lastFixTime = currCycleTicks;

					/*
					 * Fix for GPS295 - if the sat fix type
					 * is < 2 then we can set it to 2
					 * because with this message we've
					 * obviously got at least a 2d fix!!
					 * 
					 */

					if (GPS.sat.fixType < 2)
						GPS.sat.fixType = 2;

					/*
					 * Send a position update event, only
					 * if we're in 2d or 3d state;
					 * otherwise position event would come
					 * *before* a fix event (generated by
					 * UpdateStateMachine), which isn't
					 * what we want
					 *
					 */

					if (gpsModuleState == gps2d || gpsModuleState == gps3d) {

						PFSendSimpleEvent(evtGPSPositionUpdate);
						
					}
					break;

				case gpsInvRMC:

					/*
					 * invalid position fix, reduce fixType...
					 *
					 */

					if (GPS.sat.fixType > 1)
						GPS.sat.fixType = 1;

					break;

				case gpsGSV:
					PFSendSimpleEvent(evtGPSSatUpdate);
					break;

				default:
					break;

				}

				LOGLINE;
				gpsMessageReceived = true;
			}

			/* 
			 * Remove message from buffer, moving the following
			 * messages to the start of the buffer.
			 */

			currBuffSize -= (crlf+2) - gpsMessageBuff;
			if(currBuffSize<0) GpsErrThrow;
			PFMemMove(gpsMessageBuff,
				&gpsMessageBuff[(crlf+2)-gpsMessageBuff],
				currBuffSize+1); 
			/* NB currBufSize + 1 to get terminating 0 */


			/*
			 * generate state changes and messages as a result of
			 * the GPS message receipt.
			 *
			 */

			UpdateStateMachine(gpsMessageReceived);

			/*
			 * should have a '$' at the start of the buffer again, look
			 * for the cr/lf and repeat the loop
			 *
			 */

			crlf = StrChr(gpsMessageBuff, 13);

		}


		if (gpsMessageBuff[0]=='$' && currBuffSize > 100){

			/* 
			 * first char is a dollar, yet no cr/lf detected - if the buffer is
			 * 100 chars full then chuck it, because it's probably filled with
			 * crap
			 * 
			 */

			currBuffSize = 0;
			gpsMessageBuff[0] = 0;

		}

		PFGetEvent(event, sleep);

	} while (PFEventGetType(event) == nilEvent && (gpsModuleState == gps2d || gpsModuleState == gps3d));

	LOGEXIT;
	return errNone;
}

/*
 * function : GPSClose
 *
 */
void GPSClose(void){

	LOGENTRY;
	
	if (gpsModuleState != gpsOff) {

		gpsModuleState = gpsOff;
		currBuffSize = 0;
		GPS.sat.fixType = 0;

		if (gpsSource == gpsGarmin) {

			GarminGPSClose();
			
		} else if (gpsSource != gpsSimulate) {
			
			PFPortClose(gpsPortId);
			
		}
	}

	if (gpsLog) PFCloseFile(gpsLog);

	LOGEXIT;
	
}

/*
 * function : GPSSetSimParams
 *
 */

void GPSSetSimParams(const double *lat, const double *lon, float heading, 
		float altitude, float speed) {

	if (lat != NULL) simulation.lat = *lat;
	if (lon != NULL) simulation.lon = *lon;

	simulation.targetHeading = heading ;
	simulation.targetAltitude= altitude;
	simulation.targetSpeed   = speed;

}

/*
 * function : GPSSimulating
 *
 */

Boolean GPSSimulating(void) {

	return gpsSource == gpsSimulate;

}

/*
 * function : GPSSetAMSLAltitude
 *
 */

void GPSSetAMSLAltitude(float altitude) {

	if (altitude < 0.0) 

		gpsAltCorrection = 0.0;

	else 

		gpsAltCorrection = (altitude - GPS.posn.rawAltitude);

}

/*
 * function : GPSSetAltitudeCorrection
 *
 *
 */

void GPSSetAltitudeCorrection(float correction) {

	correction = MIN(1000.0, correction);
	correction = MAX(-1000.0, correction);

	gpsAltCorrection = correction;

	GPS.posn.altitude = GPS.posn.rawAltitude + gpsAltCorrection;

}

/*
 * function : GPSSetLocation
 *
 */


void GPSSetLocation(Int32 lat, Int32 lon) {

	GPS.posn.latitude = INT32_TO_RAD(lat);
	GPS.posn.longitude = INT32_TO_RAD(lon);

	GPS.posn.lat32 = lat;
	GPS.posn.lon32 = lon;

	magVarnCalculated = true;
	GPS.posn.magVarn = AvCalcMagVarn(GPS.posn.latitude, GPS.posn.longitude, 0.0, 2008);

}

/*
 * function : GPSCoordsToText
 *
 */

void GPSCoordsToText(double lat, double lon, char *north, char *east) {

	float degrees, mins, millimins;

	degrees = (float) RAD_TO_DEG(lat);
	if (degrees < 0) degrees = -degrees;
	mins    = (degrees - (Int16)(degrees))*60;
	millimins = (mins - (Int16)(mins)) * 10000;
	StrPrintF(north, "%c%02d%02d.%04d",
			lat>=0 ? (char)'N':(char)'S',
			(Int16)degrees, (Int16)mins, (Int16)millimins);

	degrees = (float) RAD_TO_DEG(lon);
	if (degrees < 0) degrees = -degrees;
	mins    = (degrees - (Int16)(degrees))*60;
	millimins = (mins - (Int16)(mins)) * 10000;
	StrPrintF(east, "%c%03d%02d.%04d",
			lon>=0 ? (char)'E':(char)'W',
			(Int16)degrees, (Int16)mins, (Int16)millimins);

}

UInt32 GPSGetDaySeconds(void) {
	
	return (UInt32)GPS.posn.utc.hour*3600+
	+ (UInt32)GPS.posn.utc.minute*60 
	+ (UInt32)GPS.posn.utc.second;

}
