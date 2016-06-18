/******************************************************************************
 *
 * Main.c
 *
 * FlightMaster stub.
 *
 * Depends on source code from FlightMaster, see Makefile for paths.
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
#include "MathLib.h"

#define LOGLINE

/*******************************************************************************
 *
 * Global Variables
 *
 */

UInt16 volRefNum;

/*******************************************************************************
 *
 * module variables
 *
 */

#define FlightMasterApp "FlightMaster-BHMN"
#define CoPilotApp "CoPilot"
#define CoPilotUserWaypoint "CoPilot User Waypoint"
#define FlightMasterSDName "/PALM/Launcher/FlightMaster.prc"
#define CoPilotSDName "/PALM/Launcher/CoPilot.prc"

#define PfErrThrowIf(c)

/*******************************************************************************
 * 
 * Database functions
 *
 */


/*
 * PFOpenDatabase
 *
 */

DmOpenRef PFOpenDatabase(const char *dbname, Boolean readOnly, Boolean createIfMissing) {

	const UInt32 FMCreatorID = 'GPFM';
	const UInt32 FMDBType = 'FMDB';

	DmOpenRef ref;
	UInt16 attr;
	LocalID lid = DmFindDatabase(0, dbname);

	if (!lid) {

		if (createIfMissing) {

			Err error; 

			error = DmCreateDatabase(0, dbname, FMCreatorID, FMDBType, false);
			PfErrThrowIf(error);

			lid = DmFindDatabase(0, dbname);

			DmDatabaseInfo(0,lid,NULL,&attr,NULL,NULL,NULL,NULL,NULL,
					NULL,NULL,NULL,NULL);

			attr |= dmHdrAttrBackup;

			DmSetDatabaseInfo(0,lid,NULL,&attr,NULL,NULL,NULL,NULL,
				NULL,NULL,NULL,NULL,NULL);

			return PFOpenDatabase(dbname, readOnly, false);

		} else {

			return NULL;

		}

	}

	ref = DmOpenDatabase(0, lid, readOnly ? dmModeReadOnly:dmModeReadWrite);
	if (!ref) return NULL;

	return ref;

}

/*
 * PFCloseDatabase
 *
 *
 */

void PFCloseDatabase(DmOpenRef ref) {

	DmCloseDatabase(ref);

}

/*
 * PFDeleteDatabase
 *
 */

void PFDeleteDatabase(const char *dbname) {

	LocalID lid = DmFindDatabase(0, dbname);

	if (lid) DmDeleteDatabase(0, lid);

}

/*
 * function : Print
 *
 * Prints the string to the screen at current cursor position
 *
 */

static void Print(const char *s) {

	static Int16 y = 0;

	WinDrawChars(s, StrLen(s), 0, y);
	y+=FntLineHeight();

}

/*
 * function : CopyFiles
 *
 * Copy the installation files from the SD card 
 * to the internal memory
 *
 */

static Boolean CopyFile(const char *filename) {

	Int16 j;
	UInt32 volIterator = vfsIteratorStart;
	Boolean found = false;
	UInt16 cardNo;
	LocalID lid;

	Err    err;

	FileRef fileRef;

	/*
	 * find the card with the file on it
	 *
	 */

	while (volIterator != vfsIteratorStop) {

		err = VFSVolumeEnumerate(&volRefNum, &volIterator);

		if (err == errNone && VFSFileOpen(volRefNum, filename, vfsModeRead, &fileRef) == errNone) {

			found = true;

			break;
		}

	}

	if (!found) {

		Print("File not found");
		return false;

	}


	if ( VFSImportDatabaseFromFile(volRefNum, filename, &cardNo, &lid) != errNone) {

		Print("failed");
		return false;

	}

	return true;

}

/*
 * PFLoadApplication
 *
 */

void PFLoadApplication(const char *appName,  Boolean run) {

	FileRef ref;
	Boolean found = false;
	UInt32 vfsMgrVersion;
	Err err;

	err=FtrGet(sysFileCVFSMgr, vfsFtrIDVersion, &vfsMgrVersion);

	if (!err) {

		UInt16 volRefNum;
		UInt32 volIterator = vfsIteratorStart;

		while (volIterator != vfsIteratorStop) {

			err = VFSVolumeEnumerate(&volRefNum, &volIterator);
			if (err == errNone) {

				err = VFSFileOpen(volRefNum, appName, vfsModeRead , &ref);

				if (err == errNone) {

					LocalID lid;
					UInt16 card;

					VFSFileClose(ref);
					if (VFSImportDatabaseFromFile(volRefNum, appName, &card, &lid)== errNone)  {

						if (run) {

							SysUIAppSwitch(card, lid, 0, NULL);
							
						}

					} else {

						Print("Error");
						Print(appName);
						SysTaskDelay(400);

					}

					return;

				}

			}

		}

	}

	Print(appName);
	Print("Application not found");

}
/*
 * function : DoMyStuff
 *
 */

static void DoMyStuff(void) {

	Err error;
	UInt16 MathLibRef;
	DmOpenRef db;
	BitmapType *logo;

	EventType event;

	logo=MemHandleLock(DmGetResource(bitmapRsc, 1000));
	WinPaintBitmap(logo, 0,0);
	MemPtrUnlock(logo);

	/*
	 * check Mathlib
	 *
	 */

	error = SysLibFind(MathLibName, &MathLibRef);
	if (error)
		error = SysLibLoad(LibType, MathLibCreator, &MathLibRef);

	if (error) {

	    error = MathLibOpen(MathLibRef, MathLibVersion);

	}

	if (error) {

		Print("MathLib not installed, installing...");

		PFLoadApplication("/PALM/MathLib.prc", false);

	}
	
	/*
	 * Check for old copy of FlightMaster/CoPilot left in main memory. This
	 * can happen if the PDA crashes while running one of the programs, and
	 * will stop updated software on the SD card from running
	 *
	 */

	if (db = PFOpenDatabase(FlightMasterApp, true, false)) {

		//Print("Purging old FlightMaster-SD left in memory");
		PFCloseDatabase(db);
		PFDeleteDatabase(FlightMasterApp);

	}

	if (db = PFOpenDatabase(CoPilotApp, true, false)) {

		//Print("Purging old CoPilot-SD left in memory");
		PFCloseDatabase(db);
		PFDeleteDatabase(CoPilotApp);

	}

	/*
	 * Check for CoPilot configuration...
	 *
	 */

	db = PFOpenDatabase(CoPilotUserWaypoint, true, false);
	if (db) {

		PFCloseDatabase(db);

		/*
		 * Configured, now load FlightMaster and CoPilot program files and
		 * execute FlightMaster
		 *
		 */

		PFLoadApplication(CoPilotSDName,false);
		PFLoadApplication(FlightMasterSDName, true);	// run flightmaster

	} else {

		/*
		 * Not configured, load FlightMaster and CoPilot program files 
		 * and execute CoPilot
		 *
		 */

		PFLoadApplication(FlightMasterSDName, false);
		PFLoadApplication(CoPilotSDName, true);// run CoPilot

	}

#ifdef XXXX
	event.eType = keyDownEvent; 
	event.data.keyDown.chr = launchChr; 
	event.data.keyDown.modifiers = commandKeyMask; 
	EvtAddEventToQueue (&event); 
#endif

}
 

/*
 * function : PilotMain
 *
 * Main function called by the PalmOS when Loader is launched.
 *
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

		DoMyStuff();
		break;

	case sysAppLaunchCmdNotify:
		break;

	default:
		break;

	}

	return errNone;
}
