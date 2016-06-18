/*
 * FMPreferences.h
 *
 */

#ifndef PREFERENCES_H
#define PREFERENCES_H

#include "Platform.h"
#include "GlobalTypes.h"
#include "Constants.h"
#include "FlightPlan.h"
#include "MapDialog.h"
#include "TimerForm.h"

#include <BtLib.h>


typedef struct {

	/*
	 * a value from -23 to +24, representing -11.5 to +12 hours offset from
	 * UTC
	 *
	 */
	
	Int8 localTimeZone;

	UInt16 registrationCode;
	UInt8 units;	/* 0=Nm, 1=Mi, 2=Km, 3=Copilot */

	Boolean autoStartGPS;

	UInt16            runwayLength;
	UInt16            runwayWidth;
	RunwaySurfaceType runwaySurface;

	GPSSourceType gpsSource;

	/*
	 * ATZ Radius, in map units (8 = 1 nautical mile)
	 *
	 */

	UInt16 atzRadius;

	/*
	 * magnetic/true headings
	 *
	 */

	Boolean useMagnetic;

	/*
	 * save last form used, restore when re-loading
	 *
	 */

	UInt16 lastForm;

	/*
	 * Simulation parameters
	 *
	 */

	float simHeading;
	float simAltitude;
	float simSpeed;

	/*
	 * GPS update rate override
	 *
	 */

	Boolean oneSecUpdate;

	/*
	 * Flight Plan page toggles
	 *
	 */

	Boolean showCumulative;
	Boolean showETA;

	/*
	 * Bluetooth device Id
	 *
	 */

	BtLibDeviceAddressType bluetoothID;

	/*
	 * one setting for each of the 4 map screens
	 *
	 */

	Int16 mapNumber;
	MapSettingType mapSetting[4];

	UInt32 registrationCode32;

	Boolean nightMode;

	TrackLogSettingType trackLog;

	Boolean turnAnticipation;
	Boolean obstacleWarnings;

	Int32 lastMapLat, lastMapLon;

	TimerType timer[NUMTIMERS];

	char FlightPlanName[36];

	Int32 voiceVolume;
	
	// Added at v7_2
	Int16 gpsAltOffset;

	// Added at v7_3
	
	Int16 hsiZoom;
	Int16 hsiTimeField;
	Int16 mapTimeField;
	
	Int32 globalAltitudeFilter;		// feet
	
	VNAVDataType vnavEnroute;
	VNAVDataType vnavApproach;
	
} FMPreferencesType_v7_3;

typedef FMPreferencesType_v7_3  FMPreferencesType;


/*
 * PrfReadPreferences
 *
 * Returns true if the preferences existed, false if they were
 * created.
 *
 */

extern Boolean PrfReadPreferences(FMPreferencesType *prefs) STARTUP_SECTION;

/*
 * PrfSavePreferences
 *
 */

extern void PrfSavePreferences(const FMPreferencesType *prefs) STARTUP_SECTION;


#endif
