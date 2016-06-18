/*
 * CommandDialog.c
 *
 * (c) 2003 Blackhawk Systems
 *
 */
#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif

#include "Platform.h"
#include "GlobalTypes.h"
#include "ResourceDefines.h"

#include "CommandDialog.h"

#include "Utils.h"


/*****************************************************************************
 * 
 * Global variables
 *
 */

/*******************************************************************************
 * 
 * module variables
 *
 * 
 */

/*
 * Resource ID of the form where the Command dialog was opened from, it
 * affects which of the buttons to display
 *
 */

static UInt16 CDCurrentForm;


/*
 * callback function supplied by the invoker of the dialog. Allows us
 * to communicate back the button that the user pressed
 *
 */



/*******************************************************************************
 *
 * private functions
 *
 * are declared here to allow easy relocation to another code section
 * 
 */

static void FormInit(void) MODALDIALOGS_SECTION;
static void MapButtonToMenuEvent(UInt16 button) MODALDIALOGS_SECTION;
static void FormDeinit(void) MODALDIALOGS_SECTION;

/*
 * function : MapButtonToMenuEvent
 *
 * Called by the command dialog event handler just prior to the command dialog
 * being closed, after the user has selected a button.
 *
 * This converts the selection into a menu event, so the command dialog is just
 * another method of generating menu events, which are what FlightMaster
 * actually responds to.
 *
 */

const struct {
	UInt16 controlID;
	UInt16 menuID;

} buttonToMenuMapping[] = {

	{ CDNavButton, MnGotoHSI }, 
	{ CDPlanButton, MnGotoPlan }, 
	{ CDDivButton, MnGotoPrx }, 
	{ CDGPSButton, MnGotoGPS }, 
	{ CDTimerButton, MnGotoTimer }, 
	{ CDMapButton, MnGotoMap }, 

	{ CDCoPilotButton, MnFlight }, 
	{ CDDayNightButton, MnDayNight }, 
	{ CDMonitorButton, MnMonitoring }, 
	{ CDPrevWaypointButton, MnPreviousLeg }, 
	{ CDNextWaypointButton, MnNextLeg }, 
	{ CDCRSButton, MnOBS }, 
	{ CDWPButton, MnNewWaypoint }, 

	{ CDUndoButton, MnUndoEdit },
	{ CDVNAVButton, MnVNAV }, 
	{ CDWPInfoButton, MnInformation},
	{ 0, 0 }

};

static void MapButtonToMenuEvent(UInt16 button) {

	Int16 j;

	/*
	 * special case - on Map Dialog, proceed to Map Configuration
	 *
	 */

	if (button == CDCloseButton && CDCurrentForm == MapForm) {

		PFEventSendMenu(MnMapConfig);
		return;

	}


	for (j=0; buttonToMenuMapping[j].controlID; j++) {

		if (button == buttonToMenuMapping[j].controlID) {

			PFEventSendMenu(buttonToMenuMapping[j].menuID);
			return;

		}

	}

}


/*
 * function : FormInit
 *
 */

static void FormInit(void) {
	UInt16 buttonToHide;

	switch (CDCurrentForm) {
	case MainForm:
		buttonToHide = CDNavButton;
		break;

	case FlightPlanForm:
		buttonToHide = CDPlanButton;
		break;

	case GPSForm:
		buttonToHide = CDGPSButton;
		break;

	case DiversionForm:
		buttonToHide = CDDivButton;
		break;

	case MapForm:
		buttonToHide = CDMapButton;
		break;

	case TimerForm:
		buttonToHide = CDTimerButton;
		break;

	default:
		buttonToHide = CDTimerButton;
		break;
	}

	GUIObjectHide( buttonToHide);

}

/*
 * function : FormDeinit
 *
 */

static void FormDeinit(void) {

	GUIFormReturn();

}

/*******************************************************************************
 *
 * public functions
 *
 */

/*
 * function : CommandDialogHandleEvent
 *
 */

Boolean CommandDialogHandleEvent(EventPtr event)
{
	Boolean handled = false;

	LOGINT16(PFEventGetType(event));

	switch (PFEventGetType(event)) 
	{
	case frmOpenEvent:
		FormInit();
		GUIFormResize(true,true);
		GUIFormDraw();
		handled = true;
		break;

	case winDisplayChangedEvent:
		if (GUIFormResize(true, true)) GUIFormDraw();
		handled = true;
		break;
	
	case frmUpdateEvent:
		GUIFormDraw();
		handled = true;
		break;
		
	case ctlSelectEvent:
		FormDeinit();
		MapButtonToMenuEvent(PFEventGetControlSelectID(event));
		handled = true;
		break;

	default:
		break;
	}
			
	return handled;
}

/*
 * function : CommandDialogInit
 *
 */

void CommandDialogInit(void) {
	CDCurrentForm = GUIFormGetActiveID();
}
