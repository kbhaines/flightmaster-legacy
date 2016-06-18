/*
 * CpInterface.h
 *
 * This module interfaces between FlightMaster and Copilot, providing functions
 * for interrogating the Copilot flights and converting them to FlightMaster
 * databases.
 *
 */

#ifndef CPINTERFACE_H_INCLUDED
#define CPINTERFACE_H_INCLUDED

#include "Platform.h"
#include "Constants.h"
#include "GlobalTypes.h"
#include "FlightPlan.h"

#ifdef AEROPALM
#define CoPilotCreatorID        'AP-P'
#else
#define CoPilotCreatorID        'GXBU'
#endif

#define CoPilotWaypointType     'wayp'
#define CoPilotUserWaypointType 'wayu'

#define CoPilotExternalDBName   "/Palm/Programs/CoPilot/CoPilot_Waypoint.pdb"

/*
 * CpRunwayInfoType
 *
 * Information about a single runway of an airfield waypoint,
 * given in the notes field of a waypoint.
 *
 */

typedef struct {

	UInt8 heading;
	UInt16 length;
	UInt16 width;
	Boolean hardSurface;

} CpRunwayInfoType;

/*
 * function : CpIsIntalled
 *
 * Returns true if the necessary CoPilot databases are available:
 *
 * 	flight, preferences, system waypoints, user waypoints
 *
 */

extern Boolean CpIsInstalled(void) CPINTERFACE_SECTION;

/*
 * function : CpInitialise
 *
 * Initialise the connection to the CoPilot databases. Must be called
 * before calling any of the Cp functions (with the exception of
 * CpIsIntalled). Call CpClose when done with the CoPilot module.
 *
 */

extern void CpInitialise(void) CPINTERFACE_SECTION;

/*
 * function : CpClose
 *
 * Call this when finished interrogating Copilot data. If any waypoints were
 * added by CpSaveWaypoint then this function will sort the CoPilot user
 * database into ident-order.
 *
 */

extern void CpClose(void) CPINTERFACE_SECTION;

/*
 * function : CpGetFlightDescription
 *
 * Returns the description text for the specified Copilot flight number. The
 * string is overwritten between successive calls to this function, so a copy
 * must be made by the caller if the value is to be retained.
 *
 * Returns NULL if flightNum is out of range.
 *
 */

extern char *CpGetFlightDescription(UInt16 flightNum) CPINTERFACE_SECTION;

/*
 * function : CpImportFlight
 *
 * Imports the specified Copilot (flightNum) into the FlightPlanType
 * structure. If there is a problem with the flight, returns false 
 * otherwise returns true. In both cases, *fp will get stomped on.
 * 
 * segNo and alternate control which section of the flight plan is loaded,
 * supporting the segmentation capabilities of CoPilot
 *
 */

extern Boolean CpImportFlight(UInt16 flightNum, FlightPlanType fp, UInt16 segNo, Boolean alternate) CPINTERFACE_SECTION;

/*
 * function : CpGetActiveFlightNum
 *
 * Reads the Copilot preferences and determines what flight is the active one
 * in CoPilot.
 *
 */

extern UInt16 CpGetActiveFlightNum(void) CPINTERFACE_SECTION;

/*
 * function : CpGetUnitPreferences
 *
 * Reads the Copilot preferences and stores them into the passed in
 * DisplayUnits type structure.
 *
 */

extern void CpGetUnitPreferences(DisplayUnitsType *du) CPINTERFACE_SECTION;

/*
 * function : CpGetValidFlights
 *
 * Returns an array of UInt16 for the flight numbers, and the total 
 * number of valid flights. 
 *
 */

extern void CpGetValidFlights(UInt16 maxFlights, UInt16 *total, UInt16 *flights) CPINTERFACE_SECTION;

/*
 * function : CpLaunchCoPilot
 *
 * Does what it says on the tin ;-)
 *
 */

extern void CpLaunchCoPilot(void) CPINTERFACE_SECTION;

/*
 * function : CpGetWaypoint
 *
 * Returns pointer to the specified CoPilot waypoint.
 *
 * Caller must free the memory. 
 *
 */

extern CpRunwayInfoType *CpGetRunwayInfo(/*@out@*/ const char *notes, /*@out@*/ Int16 *count) CPINTERFACE_SECTION;

/*
 * function : CpGetNavFrequency
 *
 * Returns a pointer to a newly allocated string, containing the frequency
 * associated with the navaid passed in wp. Returns NULL if no frequency
 * information is found.
 * 
 * Caller must deallocate storage.
 *
 */

extern char *CpGetNavFrequency(const Waypoint *wp) CPINTERFACE_SECTION;

/*
 * function : CpGetAirportFrequencies
 *
 * Returns a pointer to the airport frequencies in the waypoint
 *
 */

extern const char *CpGetAirportFrequencies(const Waypoint *wp) CPINTERFACE_SECTION;


/*
 * function : CpGetNumSegments
 *
 * Returns number of segments in last loaded CoPilot plan
 *
 */

extern Int16 CpGetNumSegments(void) CPINTERFACE_SECTION;

/*
 * function : CpGetSegmentNum
 *
 * Returns number of last loaded segment from CoPilot Plan
 *
 */

extern Int16 CpGetSegmentNum(void) CPINTERFACE_SECTION;

/*
 * function : CpGetPlanID
 *
 * Returns the ID of the last loaded CoPilot flight plan
 *
 */

extern UInt16 CpGetPlanID(void) CPINTERFACE_SECTION;

/*
 * function : CpIsAlternateLoaded
 *
 * Returns true if an alternate Flight Plan was loaded last
 *
 */

extern Boolean CpIsAlternateLoaded(void) CPINTERFACE_SECTION;

/*
 * function : CpIsAlternateAvailable
 *
 * Returns true if an alternate destination is available in
 * the last loaded CoPilot Flight Plan
 *
 */

extern Boolean CpIsAlternateAvailable(void) CPINTERFACE_SECTION;

/*
 * function : GetObstacleAltitude
 *
 */

extern Int32 GetObstacleAltitude(const Waypoint *wp) CPINTERFACE_SECTION;

#endif
