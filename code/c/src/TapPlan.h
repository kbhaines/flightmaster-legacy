/********************************************************************************
 * 
 * TapPlan state machine functions
 *
 * The user selects a waypoint and presses 'goto' to activate a diversion -
 * standard so far.  However, if he presses goto again within 5 seconds,
 * without de-activating the selected waypoint, then the system interprets this
 * as a request to edit the plan.
 *
 * In this mode, called TapPlan(tm!), each press of the goto button moves the
 * selection waypoint along to the next leg, basically the user keeps pressing
 * the goto button until the waypoint is where he wants it.
 *
 * The machine implements different behaviours between planning mode and navigation mode
 * 
 */

#ifndef TAPPLAN_H_
#define TAPPLAN_H_

#include "Platform.h"
#include "Constants.h"
#include "WDManager.h"
#include "GlobalTypes.h"
#include "MapManager.h"
#include "FlightPlan.h"

#define TP_SECTION MAP_SECTION


typedef enum { tpInitial, tpInsert, tpDelete, tpGotoPlan, tpGotoAndTerminate, tpGotoNoPlan } TapPlanState;

/*
 * TapPlanStateMachineUpdate
 * 
 * Calling with keyPressed = false means that only the state timeout is checked, so the
 * rest of the parameters can be garbage.
 *  
 */

extern void TapPlanStateMachineUpdate(
		Boolean keyPressed,
		Boolean inPlanMode, 
		FlightPlanType *currentFlightPlan, 
		FlightPlanStackType fpStack, 
		MapSelection *sel, 
		const GPSType gps, 
		const WDMHandle wpDataset) TP_SECTION;

/*
 * TapPlanCancel
 * 
 * Return the tap plan state machine to an initial
 * state
 * 
 */

extern void TapPlanCancel(void) TP_SECTION;

/*
 * TapPlanGetState
 * 
 */

extern TapPlanState TapPlanGetState(void) TP_SECTION;

/*
 * TapPlanGetStatusString
 *
 */

extern const char *TapPlanGetStatusString(void) TP_SECTION;

#endif /*TAPPLAN_H_*/
