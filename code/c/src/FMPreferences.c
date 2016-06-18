/*
 * FMPreferences.c
 *
 */

#include "Platform.h"
#include "GlobalTypes.h"
#include "Constants.h"
#include "ResourceDefines.h"
#include "FMPreferences.h"

#include "FlightDatabase.h"

/*******************************************************************************
 *
 * Module variables
 *
 */

#define PREFSET1   0
#define PREF_V5_01 13
#define PREF_V6_0  14
#define PREF_V7_0  15
//NB Skipped 16 - used in X4 release
#define PREF_V7_1  17
#define PREF_V7_2  18
#define PREF_V7_3  19

#define LATEST_PREFS_VERSION PREF_V7_3

static const char *RegCodeFileName = "/FM-RegCode.dat";

/*******************************************************************************
 * 
 * Internal/Helper Functions
 *
 */

/*******************************************************************************
 *
 * Public functions
 *
 */

/*
 * PrfReadPreferences
 *
 * Returns true if the preferences existed, false if they were
 * created.
 *
 */

Boolean PrfReadPreferences(FMPreferencesType *prefs) {

	Int16 version;
	UInt16 size;
	Boolean newPrefs = false;

	/* 
	 * Get the preferences. If they're old preferences, then upgrade them
	 */

	size = sizeof(*prefs);
	version = PrefGetAppPreferences(FlightMasterCreatorId, PREFSET1, prefs, &size, true);

	if (version == noPreferenceFound || version < PREF_V7_0) {

		/* 
		 * new installation, start from scratch 
		 *
		 */

		UInt16 j;
		PFFileRef regCodeFile;

		newPrefs = true;
		
		PFMemSet(prefs, sizeof(*prefs),0);

		prefs->localTimeZone = 0;
		prefs->registrationCode = 0;
		prefs->units = NM_UNITS;
		prefs->useMagnetic = true;
		prefs->autoStartGPS = false;
		prefs->runwayLength = 0;
		prefs->runwayWidth = 0;
		prefs->runwaySurface=surfHard;
		prefs->gpsSource = gpsCradle;
		prefs->atzRadius = 16; /* 2nm */
		prefs->lastForm  = FlightPlanForm;
		prefs->simHeading = 0.0;
		prefs->simAltitude= 1000;
		prefs->simSpeed   = 120;
		prefs->oneSecUpdate = false;

		prefs->showCumulative = false;
		prefs->showETA = false;

		for (j=0;j<6;j++) prefs->bluetoothID.address[j] = 0;

		prefs->mapNumber = 0;
		for (j=0;j<4;j++) {

			prefs->mapSetting[j].scale = 6;
			prefs->mapSetting[j].icons = wpAllAirfields | wpVOR;
			prefs->mapSetting[j].labels = vorFilter;

			prefs->mapSetting[j].airspace = asTypeClassA | asTypeClassBG;
			prefs->mapSetting[j].airspaceLabels = 0;

			prefs->mapSetting[j].lowerFilter = 0;
			prefs->mapSetting[j].upperFilter = 0;

			prefs->mapSetting[j].showTrack = false;
			prefs->mapSetting[j].showZones = false;
			prefs->mapSetting[j].trackUp = false;
			prefs->mapSetting[j].showHeadingArc = false;
			prefs->mapSetting[j].route = true;
			prefs->mapSetting[j].trackLog = false;
			prefs->mapSetting[j].terrain = terrainNormal;
			prefs->mapSetting[j].terrainRefAlt = 0.0;

		}

		/*
		 * try to recover the registration code from the SD card
		 *
		 */

		prefs->registrationCode32 = 0;
		if ((regCodeFile = PFOpenFile(RegCodeFileName, pfFileReadOnly))) {

				PFReadFile(regCodeFile, &(prefs->registrationCode32), sizeof(prefs->registrationCode32));
				PFCloseFile(regCodeFile);

		}

		prefs->nightMode = false;
		prefs->trackLog = trackLogOff;
		prefs->turnAnticipation = false;
		prefs->obstacleWarnings = false;

		prefs->lastMapLat = 1;
		prefs->lastMapLon = 1;

		for (j=0;j < NUMTIMERS;j++) {

			prefs->timer[j].countdown = false;
			prefs->timer[j].autoReset = false;
			prefs->timer[j].seconds = 0;
			prefs->timer[j].initialValue = 0;
			prefs->timer[j].state  = timerStopped;
			StrPrintF(prefs->timer[j].label,"TMR %d", j+1);

		}

		StrCopy(prefs->FlightPlanName, "My Flight");
		prefs->voiceVolume = 768;

		prefs->gpsAltOffset = 0;
		prefs->hsiZoom = 2;
		prefs->mapTimeField = 0;
		prefs->hsiTimeField = 0;

	}  else {
	   
		/*
		 * upgrade to v7_1
		 *
		 */

		if (version < PREF_V7_1) {

			Int16 j;

			for (j=0;j < NUMTIMERS;j++) {

				prefs->timer[j].countdown = false;
				prefs->timer[j].autoReset = false;
				prefs->timer[j].seconds = 0;
				prefs->timer[j].initialValue = 0;
				prefs->timer[j].state  = timerStopped;
				StrPrintF(prefs->timer[j].label,"TMR %d", j+1);

			}

			StrCopy(prefs->FlightPlanName, "My Flight");
			prefs->voiceVolume = 768;
			version = PREF_V7_1;

		}

		/*
		 * upgrade to v7_2
		 *
		 */

		if (version < PREF_V7_2) {

			prefs->gpsAltOffset = 0;
			version = PREF_V7_2;

		}

		/*
		 *  Upgrade to v7_3
		 * 
		 */
		
		if (version < PREF_V7_3) {

			prefs->hsiZoom = 2;
			prefs->mapTimeField = 0;
			prefs->hsiTimeField = 0;
			prefs->globalAltitudeFilter = 99999;
			
			prefs->vnavApproach.altType = vnavAltAboveWP;
			prefs->vnavApproach.distanceBefore = NM_TO_RAD(3.0);
			prefs->vnavApproach.rateType = vnavRatePerMin;
			prefs->vnavApproach.targetAltitude = 2000.0;
			prefs->vnavApproach.targetRate = 500.0;

			prefs->vnavEnroute.altType = vnavAltAMSL;
			prefs->vnavEnroute.distanceBefore = 0.0;
			prefs->vnavEnroute.rateType = vnavRatePerMin;
			prefs->vnavEnroute.targetAltitude = 2500.0;
			prefs->vnavEnroute.targetRate = 500.0;
			
			version = PREF_V7_3;

			FlightDBUpgrade();
			
		}

	} 

	/*
	 * Clean up previous version files
	 * 
	 */
	
	DBDelete("FlightMaster7-Index0");
	DBDelete("FlightMaster7-Index1");
	DBDelete("FlightMaster7-Index2");
	DBDelete("FlightMaster7-Index3");
	DBDelete("FlightMaster7-Index4");
	DBDelete("FlightMaster7-Index5");
	DBDelete("FlightMaster7-Index6");
	DBDelete("FlightMaster7-Index7");
	DBDelete("FlightMaster7-Index8");
	DBDelete("FlightMaster7-Index9");
	DBDelete("FlightMaster7-Index10");
	DBDelete("FlightMaster7-Index11");

	PrefSetAppPreferences(FlightMasterCreatorId, PREFSET1, LATEST_PREFS_VERSION, 
			prefs, sizeof(*prefs),true);

	return !newPrefs;

}

/*
 * PrfSavePreferences
 *
 */

void PrfSavePreferences(const FMPreferencesType *prefs) {

	PFFileRef regCodeFile;

	PrefSetAppPreferences(FlightMasterCreatorId, PREFSET1, LATEST_PREFS_VERSION, 
			prefs, sizeof(*prefs),true);

	/*
	 * save the registration code to the SD card
	 *
	 */

	if ((regCodeFile = PFOpenFile(RegCodeFileName, pfFileTruncate))) {

			PFWriteFile(regCodeFile, & (prefs->registrationCode32), sizeof(prefs->registrationCode32));
			PFCloseFile(regCodeFile);

	}

}



