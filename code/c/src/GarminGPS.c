/*
 * function : ReadFromGarminGPS
 *
 * This functions translates from the Garmin GPS library data structures into
 * our FlightMaster GPS structure formats.
 *
 * Returns false if the GPS input isn't valid.
 *
 */

#include <PalmOS.h>
#include "Platform.h"
#include "GarminGPS.h"
#include "GPSLib68K.h"
#include "GlobalTypes.h"
#include "Modules.h"

static UInt16 garminLibRef;

#define ModuleID GarminModuleID

Boolean GarminGPSOpen(void) {

	Err error;

	error = SysLibFind(gpsLibName, &garminLibRef);
	if (error != errNone) {
		error = SysLibLoad(gpsLibType, gpsLibCreator, &garminLibRef);
	}

	if (error != errNone) {

		LOGEXIT;
		return false;

	}

	if (GPSOpen(garminLibRef) != gpsErrNone) {

		LOGEXIT;
		return false;

	}

	return true;
	
}


Boolean GarminGPSRead(GPSType *GPS) {

	UInt8          maxSats = GPSGetMaxSatellites(garminLibRef);
	GPSSatDataType *garminSat = PFMalloc(maxSats * sizeof(GPSSatDataType));
	GPSPVTDataType garminPos;
	Err            error;
	UInt16         j;
	float 		   altitude;
	
	LOGENTRY;

	error = GPSGetPVT(garminLibRef, &garminPos);

	if (error == gpsErrNoData) {

		LOGTAG("No Garmin Data");
		PFMemFree(garminSat);
		return false;

	}

	ModErrThrowIf(garminSat==NULL);

	if (garminPos.status.mode != gpsModeNormal && garminPos.status.mode != gpsModeBatSaver) {

		LOGEXIT;
		return false;

	}

	error = GPSGetSatellites(garminLibRef, garminSat);

	if (error != gpsErrNone) {

		LOGTAG("Invalid Sat Data");
		PFMemFree(garminSat);
		LOGEXIT;
		return false;

	}


	/*garminPos.position.lat = 503737965;
	garminPos.position.lon = 999015050;
	garminPos.status.fix = gpsFix3D;*/
	

	/*
	 * satellite counters
	 *
	 */

	GPS->sat.viewSatCount=0;
	GPS->sat.useSatCount=0;

	MemSet(GPS->sat.stnr, sizeof(GPS->sat.stnr), (Int8)-1);
	MemSet(GPS->sat.elevation, sizeof(GPS->sat.elevation), (Int16)-1);
	MemSet(GPS->sat.azimuth, sizeof(GPS->sat.azimuth), (Int16)-1);

	for (j=0; j<maxSats; j++) {

		if (garminSat[j].svid > 0 && garminSat[j].svid < GPSNumSats+1) {
			
			Int16 satId = garminSat[j].svid;

			GPS->sat.viewSatCount++;

			if (garminSat[j].status & gpsSatUsedMask) {

				GPS->sat.useSatCount++;

			}

			GPS->sat.stnr[satId] = (Int8)(garminSat[j].snr/100);
			GPS->sat.azimuth[satId] = (Int16) RAD_TO_DEG(garminSat[j].azimuth);
			GPS->sat.elevation[satId] = (Int16) RAD_TO_DEG(garminSat[j].elevation);

		} else {

			LOGTAG("Invalid SVID:");
			LOGINT16(j);

		}
	}

	GPS->sat.pdop = garminPos.status.epe;
	GPS->sat.hdop = garminPos.status.eph;
	GPS->sat.vdop = garminPos.status.epv;

	/*
	 * time
	 *
	 */

	GPS->posn.utc.hour = garminPos.time.seconds / 3600;
	GPS->posn.utc.minute  = (garminPos.time.seconds % 3600) / 60;
	GPS->posn.utc.second  = garminPos.time.seconds % 60;

	LOGUINT32(garminPos.time.seconds);
	LOGINT16((Int16)GPS->posn.utc.hour);
	LOGINT16((Int16)GPS->posn.utc.minute);
	LOGINT16((Int16)GPS->posn.utc.second);

	/*
	 * fix-type 
	 *
	 */

	LOGINT16((Int16)garminPos.status.fix);

	switch (garminPos.status.fix){
	case gpsFixUnusable:
	case gpsFixInvalid:
		if (garminPos.status.mode > gpsModeOff)
			GPS->sat.fixType = 1;
		else
			GPS->sat.fixType = 0;
		break;
	case gpsFix2D:
	case gpsFix2DDiff:
		GPS->sat.fixType = 2;
		break;
	case gpsFix3D:
	case gpsFix3DDiff:
		GPS->sat.fixType = 3;
		break;
	}

	GPS->sat.waas = (garminPos.status.fix == gpsFix2DDiff || garminPos.status.fix == gpsFix3DDiff);

	/*
	 * position, heading & velocity
	 *
	 */
	
	LOGLINE;

	LOGINT16((Int16)GPS->sat.fixType);
	LOGINT32((Int32)garminPos.position.altMSL);
	LOGINT32(garminPos.position.lat);
	LOGINT32(garminPos.position.lon);

	LOGLINE;

	GPS->posn.latitude = PI*((double)garminPos.position.lat / 2147483648.0 );
	GPS->posn.longitude = PI*((double)garminPos.position.lon / 2147483648.0 );

	GPS->posn.lat32 = RAD_TO_INT32(GPS->posn.latitude);
	GPS->posn.lon32 = RAD_TO_INT32(GPS->posn.longitude);

	LOGLINE;

	/*
	 * deltaTime:
	 * if the corresponding code in DecodeRMC changes, don't forget to change
	 * this code also!!
	 *
	 */
	
	GPS->posn.deltaTime = 1.0;

	altitude = garminPos.position.altMSL * FEET_PER_METRE;
	GPS->posn.deltaAltitude = (altitude - GPS->posn.rawAltitude) / GPS->posn.deltaTime;
	GPS->posn.rawAltitude = altitude;
	
	/*
	 * position to text format, e.g. N5140.3780 W00203.3800
	 *
	 */


	GPSCoordsToText(GPS->posn.latitude, GPS->posn.longitude, GPS->posn.northStr, GPS->posn.eastStr);

	/*
	 *
	 * speed. Convert from metres per second into Kts:
	 *
	 * (mps * 3.6) / KM_PER_NM
	 *
	 */

	LOGLINE;
	GPS->posn.speed = ((garminPos.velocity.speed * 3.6) / KM_PER_NM);

	if (GPS->posn.speed > 0) {
		
		float newHeading = RAD_TO_DEG(garminPos.velocity.track);
		float magHeading;

		LOGLINE;

		if (newHeading < 0) newHeading += 360;

		GPS->posn.deltaHeading = (newHeading - GPS->posn.trueHeading) / GPS->posn.deltaTime;

		if (GPS->posn.deltaHeading > 180) GPS->posn.deltaHeading -= 360;
		else if (GPS->posn.deltaHeading < -180) GPS->posn.deltaHeading += 360;

		GPS->posn.trueHeading = newHeading;

		magHeading = newHeading - GPS->posn.magVarn;

		if (magHeading>=360) magHeading -= 360;
		else if (magHeading<0) magHeading += 360;

		GPS->posn.magHeading  = magHeading;
	}
	
	LOGLINE;

	PFMemFree(garminSat);
	LOGEXIT; 
	return true;
}

void GarminGPSClose(void) {
	
	Err err = GGPSClose(garminLibRef);
	
	if (err != gpsErrStillOpen) {
	
		SysLibRemove(garminLibRef);
	
	}

}
