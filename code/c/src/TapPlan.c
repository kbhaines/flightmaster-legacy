/*
 * TapPlan.c
 *
 */

#include "Platform.h"
#include "Modules.h"
#include "GlobalTypes.h"
#include "TapPlan.h"

#include "OBSManager.h"

#define ModuleID TapPlanModuleID


/*******************************************************************************
 *
 * Module variables
 *
 */

static TapPlanState tapPlanState = tpInitial;
static UInt32 tapPlanTimeout;
static Int16  tapPlanInsertPos = 0;
static char tapPlanString[50];

static const char *myPositionIdent = "(gps)";

/*******************************************************************************
 * 
 * Internal/Helper Functions
 *
 */

static void TapPlanInsertSelection(const WDMHandle wpDataset, 
		FlightPlanType *flightPlan,
		FlightPlanStackType flightPlanStack,
		const MapSelection sel, 
		Boolean rollback) TP_SECTION;

/*
 * function : TapPlanInsertSelection
 * 
 */

static void TapPlanInsertSelection(const WDMHandle wpDataset, 
		FlightPlanType *currentFlightPlan,
		FlightPlanStackType flightPlanStack,
		const MapSelection sel, 
		Boolean rollback) {

	Waypoint *wp; 
	
	OBSClear();
	
	if (MapSelectionGetType(sel) == msWaypoint) {
		
		wp = WDMGetWaypoint(wpDataset, MapSelectionGetWaypointID(sel));
		
	} else if (MapSelectionGetType(sel) == msFreePoint) {
		
		const WorldCoords *wc = MapSelectionGetCoords(sel);
		ModErrThrowIf(!wc);

		wp = WDMNewWaypoint(MapSelectionGetIdent(sel),"Point", wc->lat, wc->lon, MapSelectionGetMagVar(sel));
		
	} else {
		
		const WorldCoords *wc = MapSelectionGetCoords(sel);
		
		ModErrThrowIf(!wc);
		
		wp = WDMNewWaypoint(MapSelectionGetIdent(sel), NULL, wc->lat, wc->lon, 
				MapSelectionGetMagVar(sel));
		
	}
	
	LOGSTR(wp->ident);
	
	if (rollback) FpStackPopOverExisting(flightPlanStack, currentFlightPlan);
	
	LOGSTR(wp->ident);

	FpStackPush(flightPlanStack, *currentFlightPlan);
	
	if (FpGetNumWaypoints(*currentFlightPlan) == 1) {
		
		tapPlanInsertPos = 1;
		StrPrintF(tapPlanString, "Append %s", wp->ident);
		
	}else if (tapPlanInsertPos == 0 || tapPlanInsertPos > FpGetNumWaypoints(*currentFlightPlan)) {
		
		tapPlanInsertPos = 0;
		StrPrintF(tapPlanString, "Start at %s",	wp->ident);

	} else if (tapPlanInsertPos == FpGetNumWaypoints(*currentFlightPlan)) {
		
		StrPrintF(tapPlanString, "Append %s", wp->ident);
		
	} else {

		ModErrThrowIf(tapPlanInsertPos == 0);
		
		StrPrintF(tapPlanString, "%s >> %s >> %s",
				FpGetWaypoint(*currentFlightPlan, tapPlanInsertPos-1)->ident,
				wp->ident,
				FpGetWaypoint(*currentFlightPlan, tapPlanInsertPos)->ident);
		
	}
	LOGSTR(wp->ident);

	FpInsertWaypoint(*currentFlightPlan, tapPlanInsertPos++, RAD_TO_INT32(wp->latitude), RAD_TO_INT32(wp->longitude),
			wp->ident, wp->magVar, 0.0);
	
	tapPlanState = tpInsert;
	
	
	LOGSTR(wp->ident);

	PFMemFree(wp);
	
}


/*******************************************************************************
 *
 * Public functions
 *
 */



/*
 * function: TapPlanCancel
 *
 * Reset tapplan edit
 *
 */

void TapPlanCancel(void) {

	if (tapPlanState != tpInitial) {
		
		tapPlanState = tpInitial;
		tapPlanString[0] = 0;

	}

}

/*
 * TapPlanGetState
 * 
 */

 TapPlanState TapPlanGetState(void) {
	 
	 return tapPlanState;
	 
 }


/*
 * function : TapPlanStateMachineUpdate
 *
 *
 */

void TapPlanStateMachineUpdate(
		Boolean keyPressed,
		Boolean inPlanMode, 
		FlightPlanType *currentFlightPlan, 
		FlightPlanStackType fpStack, 
		MapSelection *sel, 
		const GPSType gps, 
		const WDMHandle wpDataset) {

	LOGENTRY;

	//TODO - something wrong with inserting waypoint as first in plan, try 
	// building plan from blank, you'll see
	
	if (!keyPressed) {
		
		if (TapPlanGetState()!= tpInitial && PFTimerHasExpired(tapPlanTimeout, SysTicksPerSecond()*8)) {
	
			TapPlanCancel();
	
		}

		LOGEXIT;
		return;

	}
	
	tapPlanTimeout = PFGetTicks();

	switch (tapPlanState) {
	
	case tpInitial:
		if (inPlanMode) {
			
			Int16 selectionRouteWaypoint;
			const WorldCoords *selectionPosition;
		
			if (!*sel) {
				
				LOGEXIT;
				return;
			}
			
			selectionPosition = MapSelectionGetCoords(*sel);
			selectionRouteWaypoint = FpFindWaypoint(*currentFlightPlan, selectionPosition->lat, selectionPosition->lon);
			
			if (selectionRouteWaypoint == fpWaypointNotFound) {
				
				TapPlanInsertSelection(wpDataset, currentFlightPlan, fpStack, *sel, false);
				
			} else { 
				
				OBSClear();
				
				StrPrintF(tapPlanString, "Delete %s", FpGetWaypoint(*currentFlightPlan, selectionRouteWaypoint)->ident);
				FpStackPush(fpStack, *currentFlightPlan);
				FpDeleteWaypoint(*currentFlightPlan, selectionRouteWaypoint);
			
				if (MapSelectionGetType(*sel) == msRouteWaypoint) {

					// TODO - zz8 convert selection to msFreepoint, improve usability
					
					MapSelectionFree(sel);
					TapPlanCancel();
					
				} else {
				
					tapPlanState = tpDelete;
					
				}
				
				break;
				
			} 
				
		} else {
			
			char gotoIdent[11];
			
			if (*sel) {
				
				const WorldCoords *wc = MapSelectionGetCoords(*sel);
				const Boolean isBlankFlight = FpIsBlank(*currentFlightPlan);
				Boolean isAirport = false;
				
				if (MapSelectionGetType(*sel) == msWaypoint && !isBlankFlight) {
					
					Waypoint *wp = WDMGetWaypoint(wpDataset, MapSelectionGetWaypointID(*sel));
					
					if (WDMGetWaypointType(wp) & wpAllAirfields) {

						isAirport = true;
						
					}
					
					PFMemFree(wp);
					
				}

				FpStackPush(fpStack, *currentFlightPlan);
				
				if (isAirport) FpInit(*currentFlightPlan);
					
				FpSetNewFirstLeg(*currentFlightPlan, gps.posn.lat32, gps.posn.lon32, myPositionIdent, DEG_TO_RAD(gps.posn.magVarn), 
						wc->lat, wc->lon, MapSelectionGetIdent(*sel), MapSelectionGetMagVar(*sel));

				if (isAirport) {
					
					tapPlanState = tpGotoAndTerminate;
					
				} else {
					
					tapPlanState = isBlankFlight ? tpGotoNoPlan : tpGotoPlan;
					
				}

				StrNCopy(gotoIdent, MapSelectionGetIdent(*sel), sizeof(gotoIdent));
				OBSClear();

			} else if (!FpIsBlank(*currentFlightPlan)) {
				
				const FlightPlanLegWaypointType *wp = FpGetCurrentWaypoint(*currentFlightPlan);

				FpStackPush(fpStack, *currentFlightPlan);
				FpSetNewFirstLeg(*currentFlightPlan, gps.posn.lat32, gps.posn.lon32, myPositionIdent, DEG_TO_RAD(gps.posn.magVarn), 
						wp->lat, wp->lon, wp->ident, wp->magVar); 

				StrNCopy(gotoIdent, wp->ident, sizeof(gotoIdent));

				//TODO - zz8 consider adding a delaying state to prevent multiple gotos of this sort
				TapPlanCancel();
				
				OBSClear();

			} else {
				
				break;
				
			}
			
			StrPrintF(tapPlanString, "Goto %s", gotoIdent);

		}
		
		break;
		
	case tpDelete:
		TapPlanInsertSelection(wpDataset, currentFlightPlan, fpStack, *sel, true);

		break;
		
	case tpInsert:
		TapPlanInsertSelection(wpDataset, currentFlightPlan, fpStack, *sel, true);
		break;
		
	case tpGotoAndTerminate:
		FpStackPopOverExisting(fpStack, currentFlightPlan);
		FpStackPush(fpStack, *currentFlightPlan);
		FpSetNewFirstLeg(*currentFlightPlan, gps.posn.lat32, gps.posn.lon32, myPositionIdent, DEG_TO_RAD(gps.posn.magVarn), 
				MapSelectionGetCoords(*sel)->lat, MapSelectionGetCoords(*sel)->lon, MapSelectionGetIdent(*sel), MapSelectionGetMagVar(*sel));
		tapPlanState = tpGotoPlan;
		break;
		
	case tpGotoNoPlan:
		TapPlanInsertSelection(wpDataset, currentFlightPlan, fpStack, *sel, true);
		tapPlanInsertPos = 0;
		break;
		
	case tpGotoPlan:
		if (FpFindWaypoint(FpStackPeek(fpStack,0), 
				MapSelectionGetCoords(*sel)->lat, MapSelectionGetCoords(*sel)->lon) 
				== fpWaypointNotFound) {
			
			TapPlanInsertSelection(wpDataset, currentFlightPlan, fpStack, *sel, true);
			tapPlanState = tpInsert;
			
		} else {
			
			Int16 routeWaypointToDelete;
			FpStackPopOverExisting(fpStack, currentFlightPlan);
			
			routeWaypointToDelete = FpFindWaypoint(*currentFlightPlan, MapSelectionGetCoords(*sel)->lat, MapSelectionGetCoords(*sel)->lon);
			StrPrintF(tapPlanString, "Delete %s", FpGetWaypoint(*currentFlightPlan, routeWaypointToDelete)->ident);

			FpStackPush(fpStack, *currentFlightPlan);
			FpDeleteWaypoint(*currentFlightPlan, routeWaypointToDelete);
		
			if (MapSelectionGetType(*sel) == msRouteWaypoint) {
				
				// TODO - zz8 convert selection to msFreepoint
				MapSelectionFree(sel);
				TapPlanCancel();
				
			} else {
			
				tapPlanState = tpDelete;
				
			}
			
			OBSClear();
			
		}
		break;
		
	}
}

/*
 * TapPlanGetStatusString
 * 
 */

const char *TapPlanGetStatusString(void) {
	
	if (tapPlanState == tpInitial) return NULL;
	
	return tapPlanString;
	
}

