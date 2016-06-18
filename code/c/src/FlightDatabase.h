/*
 * FlightDatabase
 *
 */

#ifndef FLIGHTDATABASE_H_INCLUDED
#define FLIGHTDATABASE_H_INCLUDED

#include "Platform.h"

#include "FlightPlan.h"

/*******************************************************************************
 *
 * functions
 *
 */

/*
 * FlightDBOpen
 *
 */

extern void FlightDBOpen(void) ADT3_SECTION;
extern void FlightDBClose(void) ADT3_SECTION;


/*
 * FlightDBGetNumRecords
 *
 */

extern Int16 FlightDBGetNumRecords(void) ADT3_SECTION;

/*
 * FlightDBGetDescription
 *
 */

extern char *FlightDBGetName(Int16 flightNum) ADT3_SECTION;

/*
 * FlightDBGetFlight
 *
 * Returns NULL if the specified flight is not available
 *
 */

extern FlightPlanType FlightDBGetFlight(Int16 flightNum, Boolean alternate) ADT3_SECTION;

/*
 * FlightDBSaveFlight
 *
 * Save the specified flight. If the named flight already exists and 'overwrite' is false, 
 * returns False and does not save the flight.
 *
 */

extern Boolean FlightDBSaveFlight(const char *name, FlightPlanType flight, FlightPlanType alternate, Boolean overwrite) ADT3_SECTION;

/*
 * FlightDBDeleteFlight
 *
 */

extern Boolean FlightDBDeleteFlight(Int16 flightNum) ADT3_SECTION;


extern void FlightDBUpgrade(void) ADT3_SECTION;

#endif
