/*
 * AvCalcs module
 *
 * A set of aviation and navigation calculations & alogrithms
 *
 * (c) 2003 Blackhawk Systems Ltd.
 * 
 */

#include "Platform.h"
#include "Constants.h"

/*
 * function : AvCalcRhumbLine
 *
 * Calculates the rhumb-line course and distance between two points.
 * Points are passed as radians.
 * 
 * Returns : 	bearing (radians)
 * 		distance (nm)
 *
 */

extern void AvCalcRhumbLine(double lat1, double lon1,
		double lat2, double lon2, double *bearing, double *distance)
		AVCALCS_SECTION;

/*
 * function : AvCalcGreatCircleRange
 *
 * Calculates the great circle distance between two points
 *
 * Returns the range in radians
 *
 */

#define IntCalcGreatCircleRange(lat1,lon1,lat2,lon2) \
	AvCalcGreatCircleRange(INT32_TO_RAD(lat1), INT32_TO_RAD(lon1),\
			INT32_TO_RAD(lat2), INT32_TO_RAD(lon2))

extern double AvCalcGreatCircleRange(double lat1, double lon1, 
		double lat2, double lon2) AVCALCS_SECTION;

/*
 * function : AvCalcGreatCircleCourse
 *
 * Calculates the great circle range & course between two points. 
 *
 */

#define IntCalcGreatCircleCourse(lat1,lon1,lat2,lon2, r) \
	AvCalcGreatCircleCourse(INT32_TO_RAD(lat1), INT32_TO_RAD(lon1),\
			INT32_TO_RAD(lat2), INT32_TO_RAD(lon2), r)

extern double AvCalcGreatCircleCourse(double lat1, double lon1,
		double lat2, double lon2, /*@out@*/ double *range) AVCALCS_SECTION;


/*
 * function : AvCalcOffTrackError
 *
 * Calculates the off-track error between of the present position D relative to
 * the course AB. crsAD is course from A to D, crsAB is course from A to B, d
 * is distance AD in radians. 
 *
 * Error is returned in radians, +ve is to the right of course.
 *
 */

extern double AvCalcOffTrackError(double crsAB, double crsAD, double d)
	AVCALCS_SECTION;


/*
 * function : AvCalcMagVarn
 *
 * Calculates magnetic variation
 *
 */

extern float AvCalcMagVarn(double lat, double lon, float altitude, Int16 year) AVCALCS_SECTION;

/*
 * function : AvCalcShiftPoint
 *
 * relocates a point by the specified range and bearing
 *
 */

extern void AvCalcShiftPoint(double *lat, double *lon, double bearing, double range) AVCALCS_SECTION;

