#ifndef PANMANAGER_H_
#define PANMANAGER_H_

#include "Platform.h"
#include "Constants.h"
#include "GlobalTypes.h"
#include "FlightPlan.h"
#include "MapManager.h"


/*
 * PanGetModeString
 * 
 */

extern const char *PanGetModeString(void) MAP_SECTION;

/*
 * PanHandleKeyEvent
 * 
 */

extern Boolean PanHandleKeyEvent(WChar key, const FlightPlanType fp, const GPSType gps,
		Int16 planModeWaypointNum, const MapType map, Boolean inPlanMode, ScreenInfoType lscreen) MAP_SECTION;



/*
 * PanGetPosition
 * 
 */

extern void PanGetPosition(Int32 *lat, Int32 *lon) MAP_SECTION;

/*
 * PanForceActivate
 * 
 */

extern void PanForceActivate(const Waypoint *wp) MAP_SECTION;

/*
 *  function: PanForceDeactivate
 *
 */

extern void PanForceDeactivate(void) MAP_SECTION;

#endif /*PANMANAGER_H_*/
