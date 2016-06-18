/******************************************************************************
 *
 * FlightPlanForm.c
 *
 * (c) Blackhawk Systems Ltd. 2002
 *
 */


#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif
#include "Platform.h"

#include "GlobalTypes.h"
#include "Constants.h"
#include "ResourceDefines.h"
#include "FlightPlanForm.h"
#include "Utils.h"
#include "CpInterface.h"
#include "WPInfoDialog.h"
#include "EditWaypointForm.h"
#include "DiversionMgr.h" 
#include "Gps.h"
#include "AvCalcs.h"

#include "WDManager.h"
#include "FMStrings.h"
#include "Instruments.h"
#include "OBSManager.h"
#include "Modules.h"
#include "FMPreferences.h"
#include "HeadingIndicator.h"
#include "NavManager.h"

#define ModuleID FlightPlanFormModuleID

/*******************************************************************************
 *
 * global variables
 *
 */
extern const Boolean            GPSState;
extern const GPSType        GPS;
extern FMPreferencesType  	Preferences;
extern FlightPlanType           FlightPlan;
extern FlightPlanStackType      FlightPlanStack;
extern const char              *Blanks[];
extern const char              *Dashes[];
extern const char              *Stars[];
extern const WDMHandle         WPDataset;
extern const UserConversionType UC;
extern const Int32 CycleCounter;

extern const HSIMiniPanelType HSIMiniPanel;

/*******************************************************************************
 *
 * module variables
 *
 */

/*
 * flightPlanData is a structure holding data about each leg of the flight
 * plan. altitude and track are simply copies from the FlightPlan structure,
 * but time & range are calculated according to current GPS speed
 *
 */

static struct {

	/*
	 * altitude in display units
	 *
	 */

	Int32 altitude;

	/*
	 * track in degrees
	 *
	 */

	Int16 track;

	/*
	 * distance in radians
	 *
	 */
	
	float distance;

	/*
	 * time in seconds
	 *
	 */

	Int32 time;

} flightPlanData[MAX_FP_LEGS];
	

/*
 * source for the totaliser fields
 *
 */

static float totalDistance; 	/* radians */
static Int32 totalTime;		/* seconds */
	
/*******************************************************************************
 *
 * private functions
 *
 * are declared here to allow easy relocation to other code sections
 *
 */

static void SetupFlightPlanData(void) FLIGHTPLAN_SECTION;
static void DrawLegListItem(Int16 itemNum, PFScreenRectType *bounds) FLIGHTPLAN_SECTION;
static void FlightPlanFormInit(void) FLIGHTPLAN_SECTION;
static void FlightPlanFormDeinit(void)FLIGHTPLAN_SECTION;
static void SetupList(Boolean redraw) FLIGHTPLAN_SECTION;
static void UpdateDisplay(Boolean drawlist) FLIGHTPLAN_SECTION;
static Boolean HandleListSelectEvent(EventPtr event) FLIGHTPLAN_SECTION;
static Boolean HandleCtlSelectEvent(EventPtr event) FLIGHTPLAN_SECTION;

/*
 * function : SetupFlightPlanData
 *
 *
 */

static void SetupFlightPlanData(void) {

	Int16 j;
	Int16  currentLeg = FpGetCurrentLeg(FlightPlan);
	float  gpsSpeed   = NM_TO_RAD(GPS.posn.speed);
	
	LOGENTRY;


	totalTime = 0;
	totalDistance = 0.0;

	if (FpIsBlank(FlightPlan)) {

		LOGEXIT;
		return;

	}

	/*
	 * if the GPS isn't running then just show the data from
	 * the flight plan
	 *
	 */

	if (!GPSState || GPS.sat.fixType < 2) {

		currentLeg = -1;
		gpsSpeed   = 0.0;
		totalTime  = -1;

	}

	/*
	 * all legs before the current leg should have invalid
	 * range and time fields. 
	 *
	 */

	LOGLINE;

	if (currentLeg > 0) {

		for (j=0;j<currentLeg;j++) {

			flightPlanData[j].distance = -1;
			flightPlanData[j].time = -1;

		}

	}

	/*
	 * The range & time of the current leg are determined using the GPS
	 * position and speed, not from the range in the flight plan
	 *
	 */

	LOGLINE;

	if (currentLeg > -1) {

		const FlightPlanLegWaypointType *waypoint = FpGetCurrentWaypoint(FlightPlan);
		
		totalDistance = IntCalcGreatCircleRange(GPS.posn.lat32, GPS.posn.lon32, waypoint->lat, waypoint->lon);

		flightPlanData[currentLeg].distance = totalDistance;

		if (gpsSpeed > 0.0) {

			totalTime = (Int32) (flightPlanData[currentLeg].distance * 3600 / gpsSpeed);
			flightPlanData[currentLeg].time = totalTime;

			if (Preferences.showETA) {

				flightPlanData[currentLeg].time += GPSGetDaySeconds()
					+ (Int32)Preferences.localTimeZone*30*60;

				WRAPMAX(flightPlanData[currentLeg].time, 86400);

			}


		} else { 

			totalTime = -1;
			flightPlanData[currentLeg].time = -1;

		}

	}

	/*
	 * range and time of all remaining legs come from the flight plan
	 *
	 */

	LOGLINE;

	for (j=currentLeg+1; j < FpGetNumLegs(FlightPlan); j++) {

		const FlightPlanLegType *fpl = FpGetLeg(FlightPlan, j, false);

		flightPlanData[j].distance = fpl->distance;
		totalDistance += fpl->distance;

		if (gpsSpeed > 0.0) {

			flightPlanData[j].time  = (UInt32) (flightPlanData[j].distance * 3600 / gpsSpeed);
			totalTime += flightPlanData[j].time;

		}  else {

			flightPlanData[j].time  = -1;

		}

		if (Preferences.showCumulative) {

			flightPlanData[j].distance = totalDistance;
			flightPlanData[j].time = totalTime;

		}

		if (Preferences.showETA && totalTime > 0) {

			flightPlanData[j].time = totalTime + GPSGetDaySeconds() 
				+ (Int32)Preferences.localTimeZone*30*60;
			WRAPMAX(flightPlanData[j].time, 86400);
		
		}

	}

	LOGLINE;

	/*
	 * set track & altitude
	 *
	 */

	for (j=0;j<FpGetNumLegs(FlightPlan);j++) {

		const FlightPlanLegType *fpl = FpGetLeg(FlightPlan, j, false);

		DEG_MAGVAR_WP(flightPlanData[j].track, fpl->track, fpl->a.magVar);
		flightPlanData[j].altitude = (Int32)(fpl->altitude * UC.altitudeConv);

	}

	LOGEXIT;

}


/*
 * function : DrawLegListItem
 *
 * Callback - draws a single Leg List Entry
 *
 */

static void DrawLegListItem(Int16 itemNum, PFScreenRectType *bounds) {

	Coord x = bounds->x1;
	Coord y = bounds->y1;
	
	char  tmp[25];
	char  secsString[3];
	
	const FlightPlanLegWaypointType *wp1,*wp2;
	PFScreenRectType clipRect;
	const FlightPlanLegType *fpl;
	FontID oldFont;

	LOGENTRY;
	
	/*
	 * If a new plan was loaded by FlightSelector dialog, it might have fewer
	 * legs than the current one we have set up the list for. 
	 *
	 * We haven't had a chance to update the number of list items, therefore we
	 * need to check we're not running off the end of the flight plan
	 *
	 */

	if (itemNum >= FpGetNumLegs(FlightPlan)) return;
	
	fpl = FpGetLeg(FlightPlan, itemNum, false);

	oldFont = FntSetFont(stdFont);

	/*
	 * reset the clipping region, because the waypoint Ids spill into the
	 * lower table row
	 *
	 */

	PFGetClippingRectangle(&clipRect);
	PFSetClippingRectangle(bounds);

	/*
	 * Waypoint idents
	 *
	 */
	
	wp1 = FpGetWaypoint(FlightPlan, itemNum);
	wp2 = FpGetWaypoint(FlightPlan, itemNum+1);
	if (itemNum != FpGetCurrentLeg(FlightPlan)) {
		
		FntSetFont(largeBoldFont);
		PFPaintChars(wp2->ident,StrLen(wp2->ident), x, bounds->y1);
		
	} else {
		
		PFDrawCharsTruncated(wp1->ident,StrLen(wp1->ident), x, bounds->y1,FPLegWidth);
		FntSetFont(boldFont);
		PFDrawCharsTruncated(wp2->ident,StrLen(wp2->ident), x+2, bounds->y1+FntCharHeight()-2,FPLegWidth-2);

	}
	
	x += FPLegWidth;

	FntSetFont(largeBoldFont);

	y += 2;

	/*
	 * alt & track
	 *
	 */

	if (flightPlanData[itemNum].altitude > 0)
		StrPrintF(tmp, "%ld",flightPlanData[itemNum].altitude);
	else
		StrCopy(tmp, Stars[3]);

	PFDrawChars(tmp, StrLen(tmp), x,y);
	x += FPAltWidth;

	StrPrintF(tmp, "%03d", flightPlanData[itemNum].track);
	PFDrawChars(tmp, StrLen(tmp),x,y);
	x += FPTrkWidth;

	/*
	 * range
	 *
	 */

	if (flightPlanData[itemNum].distance > 0.0) {

		/*
		 * rng is in *tenths* of a unit e.g. 105 = 10.5 
		 *
		 */

		UInt32 rng = (UInt32) ((flightPlanData[itemNum].distance*UC.distanceConv+0.05)*10);

		if (rng < 1000)
			StrPrintF(tmp, "%ld.%ld", rng/10, rng % 10);
		else if (rng < 100000)
			StrPrintF(tmp, "%ld", (rng+5)/10);
		else 
			StrPrintF(tmp, Stars[3]);

	} else {

		StrCopy(tmp,Dashes[3]);

	}

	x += FPDistWidth;

	DrawAlignedChars(tmp, ALIGNRIGHT, x-4,y);
//	len = StrLen(tmp);
//	PFDrawChars(tmp, len, x - FntCharsWidth(tmp,len) - 3, y);

	/*
	 * time
	 *
	 */

	secsString[0] = 0;
	
	if (flightPlanData[itemNum].time >= 0) {
		
		if (flightPlanData[itemNum].time < 86400) {
	
			StrPrintF(secsString,":%02ld", flightPlanData[itemNum].time % 60);
			StrPrintF(tmp,"%02ld:%02ld", flightPlanData[itemNum].time / 3600,
					((flightPlanData[itemNum].time) / 60)%60);
	
		} else {
			
			StrPrintF(tmp, "%s:%s", Stars[2],Stars[2]);
			
		}

	} else {

		StrPrintF(tmp, "%s:%s", Dashes[2],Dashes[2]);

	}

	PFDrawChars(tmp, StrLen(tmp),x,y);
	if (secsString[0]) {

		x += FntCharsWidth(tmp,StrLen(tmp));
		
		FntSetFont(boldFont);
		DrawAlignedChars(secsString,ALIGNLEFT, x,y+3);
		
	}

	/*
	 * reset font, otherwise PalmOS gets confused
	 *
	 */

	FntSetFont(oldFont);

	PFSetClippingRectangle(&clipRect);

	LOGEXIT;

}
	
/* 
 * function : FlightPlanFormInit
 *
 * Called as the form is initialised.
 *
 */

static void FlightPlanFormInit(void) {
	
	/*  warning-- don't do any drawing in this routine. */
	/*  Also, don't call FrmSetFocus from here (it must be called *after* */
	/*  FrmDrawForm) */

	GUIListSetDrawFunction(FPLegList, DrawLegListItem);

	SetupList(false);

	GUIObjectGroupSetValue(FPLegPushButtonGroup,Preferences.showCumulative ? FPCumulativePushButton : FPLegPushButton);
	GUIObjectGroupSetValue(FPTimePushButtonGroup, Preferences.showETA ? FPETAPushButton : FPETEPushButton);

	if (!CpIsInstalled()) GUIObjectHide(FPCoPilotButton);

}

/*
 * function : FlightPlanFormDeinit
 *
 * Called as the form is closing
 */
static void FlightPlanFormDeinit(void) {

	GUIListClearDrawFunction(FPLegList);
	
}

/*
 * function : SetupList
 *
 */
static void SetupList(Boolean redraw) {

	LOGENTRY;

	LOGINT16(FpGetNumLegs(FlightPlan));

	SetupFlightPlanData();

	if (!FpIsBlank(FlightPlan)) {

		GUIListSetNumItems(FPLegList, FpGetNumLegs(FlightPlan));

		if (FpGetCurrentLeg(FlightPlan) > -1) {

			GUIListSetSelection(FPLegList, FpGetCurrentLeg(FlightPlan));

		} else {

			GUIListSetSelection(FPLegList, noListSelection);

		}

	} else {

		GUIListSetNumItems(FPLegList, 0);

	}

	if (redraw) GUIListDraw(FPLegList);

	LOGEXIT;

}


/*
 * function : UpdateDisplay
 *
 */

static void UpdateDisplay(Boolean drawlist) {

	UInt32 distance;
	char tmp[48];
	Int32  time;

	LOGENTRY;

	SetupList(drawlist);

	PFScreenLock(true);
	
	GUIFormDraw();	
	time = totalTime;
 	distance = (UInt32) ((totalDistance*UC.distanceConv+0.05)*10);

	/*
	 * update the totaliser fields
	 *
	 */

	if (distance < 1000)
		StrPrintF(tmp, "%ld.%ld", distance/10, distance % 10);
	else if (distance < 100000)
		StrPrintF(tmp, "%ld", (distance+5)/10);
	else 
		StrPrintF(tmp, Stars[3]);
	GUIFieldSetText(FPTotalDistance, tmp, true);

	if (Preferences.showETA) { 

		/*
		 * can only show ETA when the GPS is running
		 *
		 */

		if (GPSState && GPS.sat.fixType > 0) {
			
			time += GPSGetDaySeconds() + (Int32)Preferences.localTimeZone*30*60+30;

			WRAPMAX(time, 86400);

		} else {

			time = -1;

		}

	}

	if (time > 0 && time < 86400 ) {

		StrPrintF(tmp,"%02ld:%02ld", time / 3600, 
				((time+30) / 60)%60);

	} else if (time <=0){

		StrPrintF(tmp,"%s:%s", Dashes[2],Dashes[2]);

	} else {
		
		StrPrintF(tmp,"%s:%s", Stars[2],Stars[2]);

	}
	GUIFieldSetText(FPTotalTime, tmp, true);

	if (HSIMiniPanel) HSIMiniPanelDraw(HSIMiniPanel);

	PFScreenUnlock();
	
	PFSendSimpleEvent(evtScreenRedrawn);
	LOGEXIT;
}

/*
 * function : HandleListSelectEvent
 *
 */
static Boolean HandleListSelectEvent(EventPtr event) {
	UInt16   oldWp = FpGetCurrentLeg(FlightPlan);

	if (oldWp != PFEventGetListSelection(event)) {
		
		OBSClear();
		FpSetCurrentLeg(FlightPlan, PFEventGetListSelection(event));

	}

	UpdateDisplay(true);

	return true;
}

/*
 * function : HandleCtlSelectEvent
 *
 * Handles control selections on the form.
 */

static Boolean HandleCtlSelectEvent(EventPtr event) {

	Boolean handled = false;

	switch (PFEventGetControlSelectID(event)) {

	case FPAlternateButton:
		PFEventSendMenu(MnFlightFlipFlop);
		handled = true;
		break;

	case FPCoPilotButton:
		CpLaunchCoPilot();
		handled = true;
		break;

	case FPLegPushButton:
		Preferences.showCumulative = false;
		handled = true;
		UpdateDisplay(true);
		break;

	case FPCumulativePushButton:
		Preferences.showCumulative = true;
		handled = true;
		UpdateDisplay(true);
		break;

	case FPETEPushButton:
		Preferences.showETA = false;
		handled = true;
		UpdateDisplay(true);
		break;

	case FPETAPushButton:
		Preferences.showETA = true;
		handled = true;
		UpdateDisplay(true);
		break;

	}
	return handled;
}


/*******************************************************************************
 *
 * public functions
 *
 */

/* 
 * function : FlightPlanFormHandleEvent
 *
 * Handles all events directed at the form.
 *
 * Returns true if the function handled the event
 */
Boolean FlightPlanFormHandleEvent(EventPtr event)
{
	Boolean handled = false;

	LOGLINE;

	switch (PFEventGetType(event)) 
	{
	case frmOpenEvent:
		FlightPlanFormInit();
		GUIFormResize(false, false);
		UpdateDisplay(false);
		handled = true;
		break;

	case winDisplayChangedEvent:
		if (GUIFormResize(false, false)) UpdateDisplay(true);
		handled = true;
		break;
		
	case ctlSelectEvent:
		handled = HandleCtlSelectEvent(event);
		break;

	case menuEvent:
		if (PFEventGetMenuID(event) == MnInformation && !FpIsBlank(FlightPlan)) {

			WaypointIDType wpID = NavGetIDOfPlanWaypoint(FpGetCurrentLeg(FlightPlan)+1);

			if (wpID != wpNotFound) {

				WPInfoSetWaypointInfo(wpID);
				GUIFormPopup(WPInfoDialog);

			}

			handled = true;

		}

		if (PFEventGetMenuID(event) == MnNextLeg || PFEventGetMenuID(event) == MnPreviousLeg || PFEventGetMenuID(event) == MnFlightFlipFlop) {

			UpdateDisplay(true);
			handled = true;

		}
		
		if (PFEventGetMenuID(event) == MnUndoEdit ||
				PFEventGetMenuID(event) == MnEditPlan) {

			if (GPSState && VALIDGPSFIX && PFEventGetMenuID(event) == MnEditPlan && !FpIsBlank(FlightPlan)) {

				const FlightPlanLegWaypointType *wp = FpGetCurrentWaypoint(FlightPlan);

				FpStackPush(FlightPlanStack, FlightPlan);
				FpSetNewFirstLeg(FlightPlan, GPS.posn.lat32, GPS.posn.lon32, "GPS", GPS.posn.magVarn,
						wp->lat, wp->lon, wp->ident, wp->magVar);

			}

			/*
			 * force form redraw
			 *
			 */

			GUIFormGoto(FlightPlanForm);

		}

		break;


	case evtFlightPlanLoaded:
		GUIFormGoto(FlightPlanForm);
		break;

	case lstSelectEvent:
		handled = HandleListSelectEvent(event);
		break;

	case evtGPSPositionUpdate:
	case evtGPSFixLost:

		LOGLINE;
		if (!GUIMenuIsDisplayed()) {
		   
			if (HSIMiniPanel || ((CycleCounter & 1) == 0)) UpdateDisplay(true);

		}

		handled = true;
		break;
		
	case frmCloseEvent:
		FlightPlanFormDeinit();
		handled = false;
		break;

	default:
		break;
	}	
	return handled;
}

