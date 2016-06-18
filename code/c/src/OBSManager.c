/*
 * OBS Manager Module
 *
 */

#include "OBSManager.h"
#include "AvCalcs.h"
#include "Modules.h"

#define ModuleID OBSManagerModuleID

/*******************************************************************************
 *
 * module variables
 *
 */

static Boolean obsActive = false;
static float obsHeading;

static FlightPlanLegWaypointType *wp0, *wp1;

/*
 * function : OBSSet
 *
 */

void OBSSet(const FlightPlanLegWaypointType *wp, float newOBSHeading, Boolean magnetic) {

	double   bearing = DEG_TO_RAD(newOBSHeading);
	double lat,lon;

	if (obsActive) OBSClear();

	if (magnetic) bearing += (double)(wp->magVar);

	obsActive = true;
	obsHeading = newOBSHeading;

	//fp->magnetic = magnetic;?????
	
	lat = INT32_TO_RAD(wp->lat);
	lon = INT32_TO_RAD(wp->lon);
	AvCalcShiftPoint(&lat, &lon, bearing-PI, NM_TO_RAD(3000));
	wp0 = FpLegWaypointNew(RAD_TO_INT32(lat),RAD_TO_INT32(lon),"O1",0.0);

	lat = INT32_TO_RAD(wp->lat);
	lon = INT32_TO_RAD(wp->lon);
	AvCalcShiftPoint(&lat, &lon, bearing, NM_TO_RAD(3000));
	wp1 = FpLegWaypointNew(RAD_TO_INT32(lat),RAD_TO_INT32(lon),"O2",0.0);

	ModErrThrowIf(!wp0 || !wp1);

}

/*
 * function : OBSGetBearing
 *
 */

float OBSGetBearing(void) {

	return obsHeading;

}

/*
 * function : OBSGetWaypoints
 *
 */

const FlightPlanLegWaypointType *OBSGetWaypoint(Int16 num) {

	ModErrThrowIf(!obsActive);

	switch(num) {
	case 0: return wp0;
			break;
			
	case 1: return wp1;
			break;

	}

	ModErrThrowIf(1);
	return NULL;

}

/*
 * function : OBSClear
 *
 * Clear the OBS
 *
 */

void OBSClear(void) {

	if (obsActive) {

		obsActive = false;
		PFMemFree(wp0);
		PFMemFree(wp1);

	}

}

/*
 * function: OBSActive
 *
 */

Boolean OBSActive(void) { return obsActive; }

