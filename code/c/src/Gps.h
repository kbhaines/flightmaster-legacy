/*
 * gps.h
 *
 * GPS Input and decoding module header file
 *
 * This module manages connection (via the serial port) to a GPS unit 
 * and decoding of standard NMEA GPS messages from the GPS into C structures.
 *
 */

#ifndef GPS_H_INCLUDED
#define GPS_H_INCLUDED

#include "Platform.h"
#include <BtLib.h>
#include "Constants.h"

#define GPSNumSats 		32
#define GPS_OK			0
#define ERR_GPS_INVALID_MSGID	-1
#define ERR_GPS_PORT_LOCKED	-2
#define ERR_GPS_NOT_INIT	-3
#define ERR_GPS_RETRY_REQD	-4
#define ERR_GPS_TIMEOUT		-5
#define ERR_GPS_OUT_OF_DATE	-6
#define ERR_GPS_NO_SUCH_SOURCE  -7
#define ERR_GPS_NO_BLUETOOTH    -8
#define ERR_GPS_NO_CARD         -9
#define ERR_GPS_NO_GARMIN       -10

#define DEMO_LIMIT 'DATE'

/* 
 * GPS Position Data Structure
 *
 */

typedef struct {

	/*
	 * strings for north and east, as read directly from the GPS
	 *
	 */
	
	char		northStr[13];
	char		eastStr[13];

	/*
	 * radians, +ve = North/East -ve= South/West
	 *
	 */
	
	double		latitude;
	double 		longitude;

	Int32		lat32;
	Int32		lon32;

	float		altitude;	// corrected altitude
	float		rawAltitude;// raw (GPS) altitude

	float 		speed;		/* in kts */
	float		trueHeading; 	/* degrees true */
	float		magHeading;	/* degress magnetic */
	float           magVarn;        /* degrees, -ve east */
	
	PFTimeStampType	utc;		/* time of data receipt */
	float		deltaTime;	/* secs between RMC messages. 0=invalid */
	float		deltaHeading;   /* heading change in degrees between RMC messages */
	float		deltaAltitude;  /* altitude change in feet between RMC messages */

} GPSPosnData;

typedef struct {

	/*
	 * NOTE:
	 * 
	 * if adding fields to this structure remember that the DecodeGSV
	 * function stamps all over it when it has recieved the last GSV in the
	 * sequence; you need to copy out any data from the old structure into
	 * the new one created by that function. See DecodeGSV note for more
	 * details
	 *
	 */

	/* 
	 * signal-to-noise ratio, per satellite. -1=no data.
	 *
	 */

	Int8 		stnr[GPSNumSats+1];

	/*
	 * azimuth & elevation of satellites. Only valid if
	 * corresponding element in stnr is not -1
	 *
	 */

	Int16           azimuth[GPSNumSats+1];
	Int16           elevation[GPSNumSats+1];

	/*
	 * number of satellites in view, and number of satellites used
	 * in the position fix (if any)
	 *
	 */

	UInt8		viewSatCount;
	UInt8		useSatCount;

	/*
	 * 0 = nothing, 1 = acquiring, 2=2d, 3=3d
	 *
	 */

	UInt8		fixType;

	/*
	 * set to true if GPS is sending messages
	 * 
	 */

	Boolean		heartBeat;

	/*
	 * precision of position fix
	 *
	 */

	float pdop;
	float vdop;
	float hdop;

	/*
	 * wide-area augmentation system
	 *
	 */

	Boolean waas;

} GPSSatData;

typedef struct {
	
	/*
	 * 0 - 100 %
	 *
	 */

	Int8 		battLevel;
	
	PFTimeStampType	timeStamp;

} GPSBattLevelData;

/*
 * types of sources that we can receive GPS information from
 *
 * Note: this list should be in the same order as the items in the list on the
 * preferences dialog
 *
 */

typedef enum { gpsCradle = 0, 
	gpsSerial, 
	gpsBluetooth, 
	gpsCard, 
	gpsGarmin,
	gpsClie,
	gpsUSB,
	gpsSimulate
} GPSSourceType;

/*
 * GPSDutyType is a parameter passed to GPSProcess, used to determine
 * what sort of duty cycle the function should use
 *
 */

typedef enum {

	/*
	 * normal duty cycle, the function reads from the GPS once per
	 * second and returns if a PalmOS event is available
	 *
	 */
	
	dutyNormal = 0,

	/*
	 * fast duty cycle, the function reads from the GPS 4 times per second
	 * and returns if a PalmOS event is available
	 *
	 */

	dutyFast

} GPSDutyType;
	

/*
 * Combined structure including position, satellite and battery
 * data all in one for convenience.
 *
 */

typedef struct {
	GPSPosnData		posn;
	GPSSatData		sat;
	GPSBattLevelData 	bat;
} GPSType;

/********************************************************************************
 * 
 * public functions
 *
 */

/*
 * GPSInit
 *
 * Initialises communications to the GPS unit (prodding it as necessary to get
 * it to start transmitting data)
 *
 * bluetoothID points to array of 6 bytes the id of the bluetooth device
 * to connect to first. If not found, GPSInit opens a discovery dialog
 *
 * Returns GPS_OK if successful, or one of the following error codes:
 * 
 * 	ERR_GPS_PORT_LOCKED	failed to open serial port
 *
 */

extern Err GPSInit(GPSSourceType source, BtLibDeviceAddressType *bluetoothID) GPS_SECTION;

/*
 * GPSSourceSupported
 *
 * Returns a boolean value indicating whether or not the specified GPS input
 * source is valid on this device
 *
 */

extern Boolean GPSSourceSupported(GPSSourceType source) GPS_SECTION;

/*
 * GPSProcess
 *
 * Processes incoming GPS messages, updating the information structures
 * provided to GPSInit with new GPS information. 'dutyCycle' controls how
 * agressively the function reads from the GPS, normal is 1/s, fast is 4/s.
 *
 * It may send the following events:
 *
 * - evtGPSSync
 * - evtGPSSyncLost
 * - evtGPSPositionUpdate
 * - evtGPSFixLost
 * - evtGPSFix
 * - evtGPSAltFix
 * - evtGPSAltFixLost
 *
 * Function only exits when it detects a PalmOS event in the event queue,
 * returns the event in 'event'.
 *
 * Returns errNone
 * 
 */

extern Err GPSProcess(GPSDutyType dutyType, EventPtr event) GPS_SECTION;

/*
 * GPSClose
 *
 * Shuts down the GPS Module, and closes the serial port. The module can
 * be restarted by calling GPSInit again.
 */

extern void GPSClose(void) GPS_SECTION;

/*
 * GPSSetSimParams
 *
 * Allows caller to set up the simulator parameters. If Lat & Lon are
 * null then position is *not* set.
 *
 */

extern void GPSSetSimParams(const double *lat, const double *lon, float heading, 
		float altitude, float speed) GPS_SECTION;

/*
 * GPSSimulating
 *
 * Returns true if the signal is being simulated
 *
 */

extern Boolean GPSSimulating(void) GPS_SECTION;
 
/*
 * GPSSetAMSLAltitude
 *
 * Tells the GPS module what altitude correction to feed into the system, so
 * that GPS vs. barometric errors can be reduced
 *
 * Accepts current altitude ABOVE MEAN SEA LEVEL. The correction is based on
 * this value vs. the current GPS altitude. The input is in feet.
 *
 * Set to -1 to cancel the correction
 *
 */

extern void GPSSetAMSLAltitude(float altitude) GPS_SECTION;

/*
 * GPSSetAltitudeCorrection
 *
 * Tells the GPS module what value to add to GPS altitude to compute final
 * altitude.
 *
 */

extern void GPSSetAltitudeCorrection(float correction) GPS_SECTION;

/*
 * GPSSetLocation
 *
 * Allows caller to set a false GPS position, as long as the GPS
 * is not running
 *
 */

extern void GPSSetLocation(Int32 lat, Int32 lon) GPS_SECTION;

/*
 * GPSCoordsToText
 *
 * Converts (double) lat/lon to text representation e.g.
 * N5140.3780
 *
 */

extern void GPSCoordsToText(double lat, double lon, /*@out@*/ char *north, /*@out@*/ char *east) GPS_SECTION;

extern UInt32 GPSGetDaySeconds() GPS_SECTION;


#endif
