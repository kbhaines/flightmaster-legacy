/*
 * OBS Manager Module
 *
 */

#include "Platform.h"
#include "GlobalTypes.h"
#include "FlightPlan.h"

/*******************************************************************************
 *
 * public functions
 *
 */

/*
 * function : OBSSet
 *
 * Set the OBS relative to the specified waypoint and using
 * the specified inbound heading which is in degrees. If magnetic
 * is true then waypoint's mag variation is taken into consideration
 *
 */

extern void OBSSet(const FlightPlanLegWaypointType *wp, float newOBSHeading, Boolean magnetic) ADT_SECTION;

/*
 * function : OBSGetBearing
 *
 * Return the current OBS bearing in degrees, as passed in last call to OBSSet
 *
 */

extern float OBSGetBearing(void) ADT_SECTION;

/*
 * function : OBSGetWaypoint
 *
 * Returns pointer to OBS waypoint 0 or 1. Waypoint 1 is on
 * the far side of the target waypoint specified in OBSSet
 * (i.e. the line between target and wp1 is the back
 * course).
 *
 * Raises an error if OBS is not active, i.e. call OBSActive to determine
 * if it's safe to call this function
 *
 * DO NOT DEALLOCATE the returned pointer!
 *
 */

extern const FlightPlanLegWaypointType *OBSGetWaypoint(Int16 num) ADT_SECTION;


/*
 * function : OBSClear
 *
 * Clear the current OBS. Safe to call even if OBS is not already set.
 *
 */

extern void OBSClear(void) ADT_SECTION;


/*
 * function: OBSActive
 *
 * Returns true if OBS is active
 *
 */

extern Boolean OBSActive(void) ADT_SECTION;
