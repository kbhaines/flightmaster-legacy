/*
 * NavManager.h
 *
 * Navigation Management Module.
 * 
 *
 */

#ifndef NAVMAN_H_INCLUDED
#define NAVMAN_H_INCLUDED

#include "Platform.h"
#include "FlightPlan.h"
#include "Gps.h"

typedef enum {

	/*
	 * these values are represented in user-units, as specified
	 * in Preferences
	 *
	 */

	navBearingTo, navDistanceTo, 
	navDestDistance, navDestETE, navDestETA,

	navCompassFrom,

	navBearingFrom, 

	navTrack, navGS, navXTRK,
	navCourse,

	navETE, navETA,
	navLocal, navUTC, 
	
	navMV, 
	navBackcourse,
	
	navFromIdent, navToIdent,
	navAltitude, navVSI,

	navPitch, navBankAngle,

	navFlightDirectorRoll,	// angle to turn to get back onto course (degrees)

	navTurnETE,				// time (secs) to the turn
	navTurnDistanceTo,

	navVNAVETE,				// time (secs) to VNAV point of descent
	navVNAVDistanceTo,		// distance to VNAV Point of Descent

	navVNAVRequiredVS,		// required rate of descent (user units)
	navVNAVAltError,		// altitude error during descent

	/*
	 * these values are the 'raw' data, un-converted.
	 *
	 */

	navRadBearingTo,
	navVNAVradDistanceToVNAV,		// radian VNAV distance
	navVNAVradDistanceToDescend,	// radian descent distance
	navEnd

} NavDataType;

	
/*
 * NavUpdatePosition
 *
 * Tells the navigation manager the latest GPS data
 *
 * If the NM notices that the current leg of the flightplan has changed, then it
 * recalculates its leg information
 *
 */

extern void NavUpdatePosition(GPSPosnData *gps) ADT_SECTION;

/*
 * NavGetFloatValue
 *
 * Returns the specified value, last calculated during call to NavUpdatePosition
 *
 */

extern float NavGetFloatValue(NavDataType field) ADT_SECTION;

extern double NavGetDoubleValue(NavDataType field) ADT_SECTION;

/*
 * NavGetIntValue
 *
 * Returns the specified value, last calculated during call to NavUpdatePosition
 *
 */

extern Int32 NavGetIntValue(NavDataType field) ADT_SECTION;

/*
 * NavGetStrValue
 *
 * Returns specified value as a formatted string. The string is volatile, the
 * next call to NavGet* function will overwrite its contents.
 *
 */

extern /*@shared@*/ const char *NavGetStrValue(NavDataType field) ADT_SECTION;

/*
 * NavDrawField
 *
 * Draws the specified navigation value at the specified location with
 * alignment, using the DrawLargeNumberSmallText function from Utils.c
 *
 * Uses DisplayUnits to determine which units to display
 *
 * Returns the value returned by DrawLargeNumberSmallText
 *
 */

extern Coord NavDrawField(NavDataType field, Coord x, Coord y, UInt8 alignment, FontID font) ADT_SECTION;


extern WaypointIDType NavGetIDOfPlanWaypoint(Int16 wpNum) ADT_SECTION;

extern void NavCalculateETE(char *result, Int32 secondsToGo) ADT_SECTION;

#endif

