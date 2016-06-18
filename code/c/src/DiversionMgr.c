/*
 * DiversionMgr.c
 *
 * Manages diversion setup and clear-down, and the diversion keypresses
 * 
 * (c)2003 Blackhawk Systems Ltd.
 *
 */
#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif

#include "Platform.h"
#include "DiversionMgr.h"
#include "GlobalTypes.h"
#include "CpInterface.h"
#include "Utils.h"
#include "ResourceDefines.h"
#include "DiversionMgr.h"
#include "MathLib.h"
#include "AvCalcs.h"
#include "FlightPlan.h"
#include "WDManager.h"
#include "OBSManager.h"
#include "NavManager.h"
#include "FMStrings.h"
#include "MessageDialog.h"
#include "Modules.h"
#include "FMPreferences.h"

#define ModuleID DiversionMgrModuleID

/*****************************************************************************
 * 
 * Global variables
 *
 */

extern const WDMHandle WPDataset;
extern const FMDataset FMDS;

/*
 * Diversions modify the flightplan (we keep a backup - see below)
 *
 */

extern FlightPlanType FlightPlan;
extern FlightPlanStackType FlightPlanStack;

/*
 * Used to get present position for start of diversion
 *
 */

extern GPSType       GPS;

/*
 * need preferences for airfield minimums during emergency diversions
 *
 */

extern FMPreferencesType Preferences;

/******************************************************************************
 * 
 * module variables
 *
 */

/* 
 * function DvEmergencyDiversion 
 *
 * find the nearest airfield that meets minimum landing requirements.
 *
 */

Boolean DvEmergencyDiversion(void) {

	WaypointIDType    foundId = wpNotFound;
	CpRunwayInfoType *runways;
	UInt16            numRunways;
	double            wpRange;
	double            maxRange = 1000/NM_PER_RADIAN;
	Waypoint         *wp;
	WDMSearchHandle   sh = WDMInitProxScan( WPDataset,
			GPS.posn.lat32, GPS.posn.lon32,
			FMDS.noObstacles, wpAirfield|wpLargeAirfield);
	ShortWaypointType *swp;

	LOGENTRY;

	swp = WDMGetProxWaypoint(sh, RAD_TO_INT32(maxRange));

	while ( swp ) {

		UInt16 k;
		Boolean foundRunway;

		wp = WDMGetWaypoint(WPDataset, swp-> wpId);

		runways = CpGetRunwayInfo(GetStringFromList(wp->ident,2),&numRunways);

		if (!runways) {

			PFMemFree(wp);
			swp=WDMGetProxWaypoint(sh, RAD_TO_INT32(maxRange));
			continue;

		}
			
		/*
		 * runways are stored longest-first, so
		 * stop looking as soon as length is
		 * not adequate
		 *
		 */

		foundRunway = false;
		for (k=0;k<numRunways && runways[k].length >= Preferences.runwayLength;k++) {

			/*
			 * check if this runway's width
			 * and surface are appropriate.
			 *
			 */

			if (runways[k].width >= Preferences.runwayWidth &&
			  ( Preferences.runwaySurface == surfEither ||
			  ((Preferences.runwaySurface == surfHard && runways[k].hardSurface) || 
			  (Preferences.runwaySurface == surfGrass && !runways[k].hardSurface)))) {

				foundRunway = true;
				break;
			}
		}
		PFMemFree(runways);

		if (!foundRunway) {

			PFMemFree(wp);
			swp=WDMGetProxWaypoint(sh, RAD_TO_INT32(maxRange));
			continue;

		}

		wpRange = AvCalcGreatCircleRange( GPS.posn.latitude,
				GPS.posn.longitude,
				wp->latitude, wp->longitude);

		if (wpRange >= maxRange) {

			PFMemFree(wp);
			swp=WDMGetProxWaypoint(sh, RAD_TO_INT32(maxRange));
			continue;

		}

		/*
		 * by this point, the waypoint is a valid candidate for a diversion
		 *
		 * Make a note of its ID, and reduce maximum range so we don't look
		 * beyond it
		 *
		 */

		maxRange = wpRange;
		foundId  = swp->wpId;

		PFMemFree(wp);

		swp=WDMGetProxWaypoint(sh, RAD_TO_INT32(maxRange));

	}

	WDMFreeSearch(sh);

	if (foundId != wpNotFound) {

		Waypoint *wp = WDMGetWaypoint(WPDataset, foundId);
		
		OBSClear();
		if (!FpStackPush(FlightPlanStack, FlightPlan)) {

			/*
			 * out of stack space
			 *
			 */

			LOGEXIT;
			return false;

		}

		FlightPlan = FpNew();

		FpSetNewFirstLeg(FlightPlan, GPS.posn.lat32, GPS.posn.lon32,"Here",DEG_TO_RAD(GPS.posn.magVarn),
				RAD_TO_INT32(wp->latitude), RAD_TO_INT32(wp->longitude), wp->ident, DEG_TO_RAD(wp->magVar));
		
		PFMemFree(wp);
		
		LOGEXIT;

		return true;

	} else {

		GUIAlertShow(EmergencyDivertFailedAlert);

		LOGEXIT;

		return false;

	}

}

