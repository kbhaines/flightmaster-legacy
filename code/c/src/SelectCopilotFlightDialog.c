/*******************************************************************************
 *
 * SelectCopilotFlightDialog.c
 *
 */
#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif

#include "Platform.h"
#include "GlobalTypes.h"
#include "Utils.h"
#include "CpInterface.h"
#include "ResourceDefines.h"
#include "SelectCopilotFlightDialog.h"

#include "DiversionMgr.h"
#include "FlightPlan.h"
#include "FlightDatabase.h"
#include "Modules.h"
#include "FMStrings.h"

#include "FMPreferences.h"

#define ModuleID SelectFlightModuleID

/******************************************************************************
 *
 * global data
 *
 */

extern FlightPlanType           FlightPlan;
extern FlightPlanStackType      FlightPlanStack;
extern FlightPlanStackType      FlightPlanStackB;
extern FMPreferencesType        Preferences;

/******************************************************************************
 *
 * module data
 *
 */

/*
 * pointer to list of integers representing the valid (usable) CoPilot
 * flights
 *
 */

static UInt16 *flightNumbers = NULL;
static Int16 importSegment = 0;

static Boolean internalDB = true;

/******************************************************************************
 *
 * private functions
 *
 * are declared here to allow them to be relocated to another code section
 *
 */

static void InitDialog(void) MODALDIALOGS_SECTION;
static void DeinitForm(void) MODALDIALOGS_SECTION;
static void DrawOneFlight(Int16 itemNum, PFScreenRectType *bounds) MODALDIALOGS_SECTION;
static void SetFlightPlanInfoField(Int16 cpFlight, Int16 segment, Boolean redraw) MODALDIALOGS_SECTION;
static void SetupFlightListBox(Boolean redraw) MODALDIALOGS_SECTION;
static void SendUpdateEvent(void) MODALDIALOGS_SECTION;
static Boolean LoadCoPilotFlight(Int16 flightNum, Int16 importSegment) MODALDIALOGS_SECTION;
static Boolean LoadFlightMasterFlight(Int16 flightNum) MODALDIALOGS_SECTION;

/*
 * function : InitDialog
 *
 * Initialises the dialog
 *
 */

#define MAX_FLIGHTS 220

static void InitDialog(void) {

	if (CpIsInstalled()) {
		
		CpInitialise();
		GUIObjectShow(SCFCoPilotButton);

	} else {

		GUIObjectHide(SCFCoPilotButton);

	}

	PFSafeMalloc(flightNumbers, MAX_FLIGHTS*sizeof(UInt16));

	GUIListSetDrawFunction(FlightListBox, DrawOneFlight);

	SetupFlightListBox(false);

}

/*
 * function : DeinitForm
 *
 */

static void DeinitForm(void) {

	if (CpIsInstalled()) CpClose();

	GUIListSetNumItems(FlightListBox, 0);
	GUIListClearDrawFunction(FlightListBox);
	
	GUIFormReturn();
	PFMemFree(flightNumbers);

}

/*
 * function : DrawOneFlight
 *
 * Callback for the list drawing
 *
 */

static void DrawOneFlight(Int16 itemNum, PFScreenRectType *bounds) { 

	char *fd = internalDB ? FlightDBGetName(itemNum) : CpGetFlightDescription(flightNumbers[itemNum]);

	PFDrawChars(fd, StrLen(fd), bounds->x1, bounds->y1);

}


/*
 * function : SetFlightPlanInfoField
 *
 * Sets the contents of the flight information field on the form
 * and enables/disables the segment control as necessary
 *
 */

static void SetFlightPlanInfoField(Int16 flightNum, Int16 segment, Boolean redraw) {

	char str[32] = "";
	FlightPlanType fp;
	const FlightPlanLegWaypointType *start, *end;

	if (flightNum != noListSelection) {

		if (internalDB) {

			fp = FlightDBGetFlight(flightNum, false);

			if (!FpIsBlank(fp)) {

				start = FpGetWaypoint(fp, 0);
				end = FpGetWaypoint(fp, FpGetNumLegs(fp));

				StrPrintF(str,"%s -> %s", start->ident, end->ident);

			} else {

				StrPrintF(str,StrBlankFlight);

			}

			GUIObjectHide(SCFNextSegmentButton);

			FpFree(fp);

		} else {
	  
			fp = FpNew();

			if (CpImportFlight(flightNumbers[flightNum], fp, segment,false)) {

				start = FpGetWaypoint(fp, 0);
				end = FpGetWaypoint(fp, FpGetNumLegs(fp));

				if (CpGetNumSegments() > 1) {

					StrPrintF(str,"%d) %s -> %s", segment+1, start->ident, end->ident);
					GUIObjectShow(SCFNextSegmentButton);

				} else {

					StrPrintF(str,"%s -> %s", start->ident, end->ident);
					GUIObjectHide(SCFNextSegmentButton);

				}

			} else {

				StrPrintF(str, "????");
				GUIObjectHide(SCFNextSegmentButton);

			}

			FpFree(fp);

		}

	}

	GUIFieldSetText(SCFInfoField, str, redraw);

}

/*
 * function : SetupFlightListBox
 *
 */

void SetupFlightListBox(Boolean redraw) {

	Int16 numFlights;

	GUIObjectGroupSetValue(SCFFlightDBGroup, internalDB ? SCFFMButton : SCFCoPilotButton);

	if (internalDB) {

		numFlights = FlightDBGetNumRecords();
		if (numFlights) GUIObjectShow(SCFDeleteButton);

	} else {

		CpGetValidFlights(MAX_FLIGHTS, &numFlights, flightNumbers);
		GUIObjectHide(SCFDeleteButton);

	}

	GUIListSetNumItems(FlightListBox, numFlights);

	if (numFlights) {

		GUIListSetSelection(FlightListBox, 0);
		GUIObjectShow(SCFOKButton);

	} else {

		GUIListSetSelection(FlightListBox, noListSelection);
		GUIObjectHide(SCFOKButton);

	}

	/* 
	 * set the highlighted item to the currently active flight
	 *
	 */

	if (internalDB) {

		importSegment = 0;
		GUIObjectHide( SCFNextSegmentButton);

	} else {

		Int16 j;
		UInt16 activeFlight;

		importSegment = 0;
		activeFlight = CpGetActiveFlightNum();
		for (j=0;j<numFlights;j++) {

			if (flightNumbers[j] == activeFlight) {

				GUIListSetSelection(FlightListBox, j);
				GUIListMakeItemVisibile(FlightListBox, j);
				break;

			}

		}

	}

	SetFlightPlanInfoField(GUIListGetSelection(FlightListBox), importSegment, redraw);

	if (redraw) GUIListDraw(FlightListBox);

}


/*
 * function : SendUpdateEvent
 *
 */

void SendUpdateEvent(void) {

	PFSendSimpleEvent(evtFlightPlanLoaded);

}

/*
 * function : LoadCoPilotFlight
 *
 * Loads the specified CoPilot flight into FlightPlan, returns
 * true if the flight was loaded OK
 *
 */

static Boolean LoadCoPilotFlight(Int16 flightNum, Int16 importSegment) {

	FlightPlanType newfp = FpNew();

	if (flightNum == noListSelection || !CpImportFlight(flightNum, newfp, importSegment,false)) {

		GUIAlertShow(FlightPlanAlert);
		return false;

	} else {

		/*
		 * plan loaded successfully
		 *
		 */

		FlightPlanType fp;

		FpCopy(FlightPlan, newfp);

		/*
		 * purge FlightPlanStack
		 *
		 */

		while ( (fp = FpStackPop(FlightPlanStack)) ) FpFree(fp);

		/*
		 * Check for a valid alternative flight
		 *
		 */

		if (CpIsAlternateAvailable() && CpImportFlight(flightNum, newfp, importSegment, true)) {

			/*
			 * ask user if OK to use alternate plan and purge FP-StackB
			 * if so
			 *
			 */

			if (GUIAlertShow(LoadAlternateAlert) == 0) {

				while ( (fp = FpStackPop(FlightPlanStackB)) ) FpFree(fp);

				FpStackPush(FlightPlanStackB, newfp);
				FpFree(newfp);

			}

		} else {

			FpFree(newfp);

		}

	}

	return true;

}

/*
 * function : LoadFlightMasterFlight
 *
 * Loads the specified FlightMaster DB flight into FlightPlan, returns
 * true if the flight was loaded OK
 *
 */

static Boolean LoadFlightMasterFlight(Int16 flightNum) {

	FlightPlanType newfp = FlightDBGetFlight(flightNum, false);
	FlightPlanType fp;

	if (flightNum == noListSelection || !newfp) {

		if (newfp) FpFree(newfp);

		GUIAlertShow(FlightPlanAlert);
		return false;

	} 

	/*
	 * plan loaded successfully!
	 *
	 */

	FpCopy(FlightPlan, newfp);
	StrNCopy(Preferences.FlightPlanName, FlightDBGetName(flightNum), sizeof(Preferences.FlightPlanName));

	/*
	 * purge FlightPlanStack
	 *
	 */

	while ( (fp = FpStackPop(FlightPlanStack)) ) FpFree(fp);

	/*
	 * Load alternative flight into FlightPlanStackB if user says so.
	 *
	 */

	FpFree(newfp);

	newfp = FlightDBGetFlight(flightNum, true);
	if (newfp && GUIAlertShow(LoadAlternateAlert) == 0) {

		while ( (fp = FpStackPop(FlightPlanStackB)) ) FpFree(fp);

		FpStackPush(FlightPlanStackB, newfp);

	}

	if (newfp) FpFree(newfp);

	return true;

}


/******************************************************************************
 * 
 * public functions:
 *
 */

/*
 * function : FlightDialogHandleEvent
 *
 */

Boolean FlightDialogHandleEvent(EventPtr event) {

	Boolean handled = false;
	Int16 lstSelection = GUIListGetSelection(FlightListBox);
	FlightPlanType fp;

	switch (PFEventGetType(event)) {

	case frmOpenEvent:
		GUIFormResize(true, true);
		InitDialog();
		GUIFormDraw();		
		handled = true;
		break;

	case winDisplayChangedEvent:
		GUIFormResize(true, true);
		GUIFormDraw();
		handled = true;
		break;

	case keyDownEvent:
		LOGINT16(lstSelection);
		switch (PFEventGetKeyChr(event)) {
		
		/*
		 * page up & page down move the current list selection
		 *
		 */

		case vchrPageUp:
			if (lstSelection>0)
				GUIListSetSelection(FlightListBox, lstSelection-1);
			handled = true;
			break;

		case vchrPageDown:
			if (lstSelection+1<GUIListGetNumItems(FlightListBox))
				GUIListSetSelection(FlightListBox, lstSelection+1);
			handled = true;
			break;

		}

		//GUIListMakeItemVisible(FlightListBox, GUIListGetSelection(FlightListBox));
		break;

	case lstSelectEvent:
		SetFlightPlanInfoField(PFEventGetListSelection(event), 0, true);
		importSegment = 0;
		handled = true;
		break;

	case ctlSelectEvent:

		switch (PFEventGetControlSelectID(event)) {

		/*
		 * if OK is pressed, see if the flight plan can be loaded now
		 *
		 */

		case SCFOKButton: 
			handled = true;
			if (lstSelection == noListSelection) {
				
				break;

			}

			if ((internalDB && LoadFlightMasterFlight(lstSelection)) 
				|| (!internalDB && LoadCoPilotFlight(flightNumbers[lstSelection], importSegment))) {

				DeinitForm();
				SendUpdateEvent();
				handled = true;

			}
			break;

		case SCFCancelButton:
			DeinitForm();
			handled = true;
			break;
			
		case SCFBlankButton:
			FpInit(FlightPlan);

			/*
			 * purge FlightPlanStack
			 *
			 */

			while ( (fp = FpStackPop(FlightPlanStack)) ) FpFree(fp);

			DeinitForm();
			handled = true;
			SendUpdateEvent();
			break;

		case SCFNextSegmentButton:
			if (++importSegment == CpGetNumSegments()) {

				importSegment = 0;

			}

			SetFlightPlanInfoField(lstSelection, importSegment, true);
			handled = true;
			break;

		case SCFFMButton:
			internalDB = true;
			SetupFlightListBox(true);
			handled = true;
			break;

		case SCFCoPilotButton:
			internalDB = false;
			SetupFlightListBox(true);
			handled = true;
			break;

		case SCFDeleteButton:
			if (internalDB && lstSelection != noListSelection && GUIAlertShow(FlightDeleteAlert) == 0) {
				
				FlightDBDeleteFlight(lstSelection);

				GUIListSetNumItems(FlightListBox, FlightDBGetNumRecords());
				GUIListSetSelection(FlightListBox, noListSelection);
				GUIListDraw(FlightListBox);

			}
			handled = true;
			break;
				
		}

		break;
			

	default:
		break;
	}

	return handled;

}
