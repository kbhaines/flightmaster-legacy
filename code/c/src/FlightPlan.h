/*
 * FlightPlan.h
 *
 * Flight Plan ADT
 *
 */


#ifndef FLIGHTPLAN_H_INCLUDED
#define FLIGHTPLAN_H_INCLUDED

#include "Platform.h"
#include "Constants.h"
#include "GlobalTypes.h"

#define MAX_FP_LEGS 45
#define MAX_IDENT_CHARS 10

/*
 * Vertical navigation
 *
 * NB VNAVRateType is aligned with VNRateButtons in FMStrings
 *
 */

typedef enum { vnavAltAMSL, vnavAltAboveWP } VNAVAltType;
typedef enum { vnavRatePerMin, vnavRatePerMile, vnavDegrees } VNAVRateType;

/*
 * IF CHANGING THIS STRUCTURE REMEMBER TO CHANGE FILEMAGIC!!
 *
 */

typedef struct {

	VNAVAltType altType;
	float       targetAltitude;	// feet
	
	VNAVRateType rateType;
	float		targetRate;		// feet/nm or radians

	float distanceBefore; // in radians

} VNAVDataType;


/*
 * LegWaypoint
 *
 * IF CHANGING THIS STRUCTURE REMEMBER TO CHANGE FILEMAGIC!!
 *
 */

typedef struct {

	WaypointIDType id_deprecated; // deprecated!!!
	char ident[11];
	Int32 lat, lon;
	float magVar;	// magVar is in radians, east is -ve

} FlightPlanLegWaypointType;


/*
 * Flight Plan Leg
 *
 * IF CHANGING THIS STRUCTURE REMEMBER TO CHANGE FILEMAGIC!!
 *
 */

typedef struct {

	/*
	 * waypoint leg information
	 *
	 */

	FlightPlanLegWaypointType a;
	FlightPlanLegWaypointType b;

	/*
	 * leg information
	 *
	 */

	float altitude; // in feet
	float distance; // in radians
	float track; 	// in radians

	/*
	 * data in vnav only valid if vnavSet is true
	 *
	 */

	Boolean vnavSet;
	VNAVDataType vnav;

} FlightPlanLegType;

typedef struct FlightPlanType *FlightPlanType;

typedef enum { minorVersion, majorVersion } FlightPlanVersionType;

typedef struct FlightPlanStackType *FlightPlanStackType;


/*
 *  function: FpLegWaypointNew
 *
 */

extern FlightPlanLegWaypointType *FpLegWaypointNew(Int32 lat, Int32 lon, const char *ident, float magvar) ADT2_SECTION;

/*
 * function : FpNew
 *
 * Create a new flightplan and initialise it
 *
 */

extern FlightPlanType FpNew(void) ADT2_SECTION;

extern void FpFree(/*@out@*/ /*@only@*/ FlightPlanType fp) ADT2_SECTION;

/*
 * function : FpSizeOf
 *
 */

extern const UInt32 FpSizeOf(void) ADT2_SECTION;

/*
 * function : FpInit
 *
 * Initialise a flight plan
 *
 */

extern void FpInit(FlightPlanType fp) ADT2_SECTION;

/*
 * function : FpCopy
 *
 */

extern void FpCopy(FlightPlanType dest, FlightPlanType src) ADT2_SECTION;

/*
 * functions to manage current flightplan leg
 *
 * Returns false if there was no change
 *
 */

extern Boolean FpNextLeg(FlightPlanType fp) ADT2_SECTION;

extern Boolean FpPreviousLeg(FlightPlanType fp) ADT2_SECTION;

extern void FpSetCurrentLeg(FlightPlanType fp, Int16 leg) ADT2_SECTION;


/*
 * function : FpGetFrom/ToWaypoint
 *
 * Returns pointer to waypoint at start or end of specified leg
 *
 */

extern const FlightPlanLegWaypointType *FpGetFromWaypoint(const FlightPlanType fp, Int16 leg) ADT2_SECTION;

extern const FlightPlanLegWaypointType *FpGetToWaypoint(const FlightPlanType fp, Int16 leg) ADT2_SECTION;

/*
 * function : FpInsertWaypoint
 *
 * Insert the waypoint between the end points of the specified leg, creating a
 * new leg in the process
 *
 * Returns true if successful, false if failed - (because MAX_FP_LEGS was
 * reached or because the waypoint is a duplicate of the previous or next
 * waypoint after insert position)
 *
 */

extern Boolean FpInsertWaypoint(FlightPlanType fp, Int16 leg, Int32 lat, Int32 lon, const char *ident, float magVarn, float altitude) ADT2_SECTION;

/*
 * function : FpAppendWaypoint
 *
 */

extern Boolean FpAppendWaypoint(FlightPlanType fp, Int32 lat, Int32 lon, const char *ident, float magVarn, float altitude) ADT2_SECTION;

/*
 * function : FpDeleteWaypoint
 *
 * Delete the specified waypoint from the plan modifying the other legs as
 * required to keep a contiguous plan.
 *
 */

extern Boolean FpDeleteWaypoint(FlightPlanType fp, Int16 wpNum) ADT2_SECTION;

/*
 * function : FpFindWaypoint
 *
 * Looks for the specified waypoint in the flight plan, and returns
 * the waypoint number.
 *
 * Returns fpWaypointNotFound if not found
 *
 */

#define fpWaypointNotFound -1
extern Int16 FpFindWaypoint(const FlightPlanType fp, Int32 lat, Int32 lon) ADT2_SECTION;

/*
 *  function: FpSetNewFirstLeg
 *
 */

extern void FpSetNewFirstLeg(FlightPlanType fp, Int32 fromLat, Int32 fromLon, const char *fromIdent, float fromMagVar,
		Int32 toLat, Int32 toLon, const char *toIdent, float toMagVar) ADT2_SECTION;

/*
 * function : FpDeleteLeg
 *
 * Delete the specified leg from the plan
 *
 */

extern Boolean FpDeleteLeg(FlightPlanType fp, Int16 legNum) ADT2_SECTION;

extern Int16 FpGetCurrentLeg(const FlightPlanType fp) ADT2_SECTION;
#define FpGetCurrentWaypoint(fp) FpGetWaypoint(fp, FpGetCurrentLeg(fp)+1)

extern Int16 FpGetNumLegs(const FlightPlanType fp) ADT2_SECTION;
#define FpGetNumWaypoints(fp) (FpGetNumLegs(fp)+1)

extern Boolean FpIsBlank(const FlightPlanType fp) ADT2_SECTION;

extern const FlightPlanLegType *FpGetLeg(const FlightPlanType fp, Int16 leg,
		Boolean obs) ADT2_SECTION;

/*
 * function : FpGetWaypoint
 *
 * Returns the specified waypoint of the flightplan, where waypoint 0 
 * is the start of the flightplan
 *
 */

extern const FlightPlanLegWaypointType *FpGetWaypoint(const FlightPlanType fp, Int16 wpNum) ADT2_SECTION;

/*
 * function : FpSetVNAV
 *
 * Sets the VNAV data for the specified leg of the flightplan.
 *
 */

extern void FpSetVNAV(FlightPlanType fp, Int16 leg, const VNAVDataType *vd) ADT2_SECTION;

/*
 * function : FpVNAVIsSet
 *
 * Returns true if VNAV is active for the leg
 *
 */

extern Boolean FpVNAVIsSet(FlightPlanType fp, Int16 leg) ADT2_SECTION;

/*
 * function : FpGetVNAV
 *
 * Returns the VNAV data for the current leg
 *
 */

extern const VNAVDataType *FpGetVNAV(FlightPlanType fp, Int16 leg) ADT2_SECTION;

/*
 * function : FpClearVNAV
 *
 * Disables VNAV for the specified leg
 *
 */

extern void FpClearVNAV(const FlightPlanType fp, Int16 leg) ADT2_SECTION;

/*
 * function : FpGetVersion
 *
 * Returns current version number of flight plan; this changes everytime the
 * flight plan is modified
 *
 */

extern Int16 FpGetVersion(const FlightPlanType fp, FlightPlanVersionType fpv) ADT2_SECTION;

/*
 * function : FpSaveToFile
 *
 */

extern Int32 FpSaveToFile(FlightPlanType f, PFFileRef file) ADT2_SECTION;

/*
 * function : FpLoadFromFile
 *
 */

extern Int32 FpLoadFromFile(FlightPlanType f, PFFileRef file) ADT2_SECTION;

/*
 * function : FPStackNew, FPStackPush, FPStackPop
 *
 * These three functions manage a simple stack of flight plans.
 *
 * FpStackPeek allows caller to peek at the entries under the top of
 * the stack - 0 is the top entry, 1 is second from top etc. NULL is returned
 * when the end of the stack is reached
 *
 */

extern FlightPlanStackType FpStackNew(void) ADT2_SECTION;
extern Boolean FpStackPush(FlightPlanStackType fs, FlightPlanType fp) ADT2_SECTION;
extern FlightPlanType FpStackPop(FlightPlanStackType fs) ADT2_SECTION;

/*
 * Safely overwrite an existing flight plan (fp) with the plan
 * from the top of the stack
 * 
 */
extern void FpStackPopOverExisting(FlightPlanStackType fs, FlightPlanType *fp) ADT2_SECTION;

extern const FlightPlanType FpStackPeek(FlightPlanStackType fs, Int16 num) ADT2_SECTION;
extern void FpStackFree(FlightPlanStackType fs) ADT2_SECTION;

/*
 * function : FpStackSave
 *
 */

extern Boolean FpStackSave(FlightPlanStackType fs, const char *filename) ADT2_SECTION;

/*
 * function : FpStackLoad
 *
 */

extern Boolean FpStackLoad(FlightPlanStackType fs, const char *filename) ADT2_SECTION;

/*
 * function : FpSwapLongitudes
 *
 * This function needed to keep compatibility with FM7.5 waypoint 
 * database format (during flight load/save operations, see
 * FlightDatabase.c)
 * 
 */

extern void FpSwapLongitudes(FlightPlanType fp) ADT2_SECTION;

#endif

