/******************************************************************************
 *
 * Main.c
 *
 * FlightMaster Loader main application module
 *
 */

#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif

#include <PalmOS.h>
#include <PalmCompatibility.h>
#include <VFSMgr.h>

/* 
 * include registration code c file, to allow inlining of the registration
 * code generator
 *
 */

#define LOGLINE

/*
 * these files are included from the original FlightMaster source
 * code
 *
 */

#include "Constants.h"
#include "GlobalTypes.h"
#include "RegistrationCode.c"
#include "ResourceDefines.h"

#include "GPSLib68K.h"

/*******************************************************************************
 *
 * Global Variables
 *
 */

UInt16 volRefNum;

/*
 * User preferences, set on the Preferences dialog and in the Waypoint Scanning
 * dialog. Stored in the application preferences section when application exits
 *
 */

MNPreferencesType  Preferences;

/*******************************************************************************
 *
 * module variables
 *
 */

#define PREFSET1   0
#define PREF_V4_0  8
#define PREF_V4_1  9
#define PREF_V4_6  10
#define PREF_V4_7  11
#define PREF_V5_0  12
#define PREF_V5_01 13

/*
 * function : Print
 *
 * Prints the string to the screen at current cursor position
 *
 */

static void Print(char *s) {

	static Int16 y = 0;

	WinDrawChars(s, StrLen(s), 0, y);
	y+=FntLineHeight();

}

/*
 * function : SecurityCheck
 *
 * Verifies that an SD card in the slot has
 * a valid device ID
 *
 */

static Boolean SecurityCheck(void) {

	UInt16 slotRefNum;
	UInt32 slotIterator = expIteratorStart;

	while (slotIterator != expIteratorStop) {

		Err err;

		err = ExpSlotEnumerate(&slotRefNum, &slotIterator);

		if (err == errNone) {

			ExpCardInfoType ci;
			UInt32  regCode;
			char s[80];
			Int16 j;

			const UInt32 validCode[] = {
				
				//1572832U,	// emulator (COMMENT OUT!!)
				397800397U, 	// Kingston 256MB
				3575865445U,	// Sandisk 64MB

				3767031331U,	// card 1 (Garmin 3600)
				3944313473U,	// card 2 (Garmin 3600)

				430578148U,	// card 1 (T-E2)
				692105149U,	// card 2 (T-E2)

				0
			
			};

			/*
			 * check the serial number
			 *
			 */

			ExpCardInfo(slotRefNum, &ci);

			regCode = GenerateRegistrationCode(0, REGCODEPRIME, 
					ci.deviceUniqueIDStr);
			
			/*
			 * remove this for the release version
			 *
			 */

			//StrPrintF(s, "%s = %ld", ci.deviceUniqueIDStr, regCode);
			//Print(s);

			for (j=0;validCode[j] != 0;j++)

				if (regCode == validCode[j]) return true;

		}

	}

	return false;

}

/*
 * function : CopyFiles
 *
 * Copy the installation files from the SD card 
 * to the internal memory
 *
 */

static Boolean CopyFiles(void) {

	const char *filename[] = {

		"/AeroPalmNav.prc",
		"/AeroPalmNav-Airspace.pdb",
		"/AeroPalmNav-Index.pdb",

		"/AeroPalmPlan.prc",
		"/AeroPalmPlan_Aircraft.pdb",
		"/AeroPalmPlan_Pilot.pdb",
		"/AeroPalmPlan-Flight.pdb",
		"/AeroPalmPlan-Prefs.pdb",
		"/AeroPalmPlan-UserWpt.pdb",
		"/AeroPalmPlan-Waypoint.pdb",

		"/MathLib.prc",

		NULL

	};

	Int16 j;
	UInt32 volIterator = vfsIteratorStart;
	Boolean found = false;

	Err    err;

	FileRef dirRef;
	FileInfoType fileInfo;
	UInt32  fileIterator;
	char    *fileName;


	/*
	 * find the card with the files on it
	 *
	 */

	while (volIterator != vfsIteratorStop) {

		err = VFSVolumeEnumerate(&volRefNum, &volIterator);

		if (err == errNone && VFSFileOpen(volRefNum, "/MWSA",
					vfsModeRead,
					&dirRef) == errNone) {

			found = true;

			break;
		}

	}

	if (!found) {

		Print("Directory not found");
		return false;

	}
	fileName = MemPtrNew(256);
	fileInfo.nameP = fileName;
	fileInfo.nameBufLen = 256;
	fileIterator = vfsIteratorStart;

	while (fileIterator != vfsIteratorStop) {

		char s[100];
		UInt16 cardNo;
		LocalID lid;

		err = VFSDirEntryEnumerate(dirRef, &fileIterator, &fileInfo);

		if (err == errNone) {

			StrPrintF(s, "Copying %s", fileName);
			Print(s);

			StrPrintF(s, "/MWSA/%s", fileName);

			if ( VFSImportDatabaseFromFile(volRefNum, s, &cardNo, &lid) != errNone) {

				Print("failed");
				MemPtrFree(fileName);
				return false;

			}

		} else {

			StrPrintF(s,"Dir: %d", err);
			Print(s);
			MemPtrFree(fileName);
			return false;

		}

	}

	MemPtrFree(fileName);
	return true;

}

/*
 * function : LoadCode
 *
 * Load the registration code & preferences into the
 * FlightMaster preference database
 *
 */

static void LoadCode(void) {

	UInt16 j;
	MNPreferencesType Preferences;

	UInt8 *serialNo;
	UInt16 serialNoSize;
	char  *serialNoString;
	char s[120];
	FileRef fileRef;

	UInt16 libRef;
	Err error;

	Preferences.localTimeZone = 0;
	Preferences.registrationCode = 0;
	Preferences.units = CP_UNITS;
	Preferences.useMagnetic = true;
	Preferences.autoStartGPS = false;
	Preferences.waypointFilter = wpAirfield;
	Preferences.runwayLength = 0;
	Preferences.runwayWidth = 0;
	Preferences.runwaySurface = surfHard;
	Preferences.gpsSource = gpsBluetooth;

	/*
	 * Check for Garmin iQue support. The iQue has a built in library for
	 * managing the GPS input, so load that (if not already loaded)
	 * and open it.
	 *
	 */

	error = SysLibFind(gpsLibName, &libRef);
	if (error != errNone) {

		UInt16 garminLibRef;

		error = SysLibLoad(gpsLibType, gpsLibCreator, &garminLibRef);
		SysLibRemove(garminLibRef);

	}

	if (error == errNone) Preferences.gpsSource = gpsGarmin;


	Preferences.afZoom = 0x21;
	Preferences.VORZoom = 0x22;
	Preferences.NDBZoom = 0x11;
	Preferences.otherZoom = 0x20;
	Preferences.atzRadius = 16; /* 2nm */
	Preferences.lastForm      = FlightPlanForm;
	Preferences.simHeading = 0.0;
	Preferences.simAltitude= 1000;
	Preferences.simSpeed   = 120;
	Preferences.oneSecUpdate = false;

	Preferences.showCumulative = false;
	Preferences.showETA = false;

	for (j=0;j<6;j++) Preferences.bluetoothID.address[j] = 0;

	Preferences.mapNumber = 0;
	for (j=0;j<4;j++) {

		Preferences.mapSetting[j].scale = 3;
		Preferences.mapSetting[j].icons = wpAirfield | vorFilter;
		Preferences.mapSetting[j].labels = vorFilter;

		Preferences.mapSetting[j].airspace = asTypeClassA | asTypeClassBG;
		Preferences.mapSetting[j].airspaceLabels = 0;

		Preferences.mapSetting[j].lowerFilter = 0;
		Preferences.mapSetting[j].upperFilter = 0;

		Preferences.mapSetting[j].showTrack = false;
		Preferences.mapSetting[j].showZones = false;
		Preferences.mapSetting[j].trackUp = false;
		Preferences.mapSetting[j].showHeadingArc = false;

	}

	if ( SysGetROMToken(0, sysROMTokenSnum, &serialNo, &serialNoSize) != errNone) {
		
		Print("Loader error 1");

		/*
		 * comment out during development
		 *
		 */

		return;

	}

	serialNoString = MemPtrNew(serialNoSize+1);
	MemMove(serialNoString, serialNo, serialNoSize);
	serialNoString[serialNoSize] = 0;

	Preferences.registrationCode32 = GenerateRegistrationCode(
			0, REGCODEPRIME, serialNoString);

	StrPrintF(s,"%s %ld\n", serialNoString, Preferences.registrationCode32);
	Print(s);

	VFSFileOpen(volRefNum, "/codes.dat", vfsModeCreate | vfsModeReadWrite, &fileRef);
	VFSFileSeek(fileRef, vfsOriginEnd, 0);
	VFSFileWrite(fileRef, StrLen(s), (void*)s, NULL);
	VFSFileClose(fileRef);

	PrefSetAppPreferences(MiniNavCreatorId, PREFSET1, PREF_V5_01, 
			&Preferences, sizeof(Preferences),true);

	MemPtrFree(serialNoString);

}


/*
 * function : PilotMain
 *
 * Main function called by the PalmOS when Loader is launched.
 */

UInt32 PilotMain(UInt16 launchCode, MemPtr launchParameters, 
	UInt16 launchFlags)
{
#pragma unused(launchParameters)
	Err error;
	UInt32 vfsMgrVersion;
	
	switch (launchCode) {
	
	case sysAppLaunchCmdSyncNotify:
		break;

	case sysAppLaunchCmdExgReceiveData:
		break;

	case sysAppLaunchCmdNormalLaunch:

		if (SecurityCheck()) {

			if (CopyFiles()) 
				LoadCode();
			else
				Print("Can't copy files");

		} 

		SysTaskDelay(SysTicksPerSecond()*2);

		break;

	case sysAppLaunchCmdNotify:
		break;

	default:
		break;

	}

	return errNone;
}
