/******************************************************************************
 *
 * TimerForm.c
 *
 * (c) Blackhawk Systems Ltd 2002.
 */

#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif
#include "Platform.h"
#include "ResourceDefines.h"
#include "FMStrings.h"
#include "KeypadDialog.h"

#include "Utils.h"
#include "Gps.h"

#include "GlobalTypes.h"
#include "TimerForm.h"

#include "AlarmManager.h"
#include "AlphaPadDialog.h"
#include "Modules.h"

#include "FMPreferences.h"
#include "HeadingIndicator.h"

#define ModuleID TimerModuleID

/**************************************************************************
 *
 * global variables
 *
 */

extern const GPSType GPS;
extern const Boolean GPSState;

extern FMPreferencesType Preferences;

extern const IconWindowsType IconWindows;

extern const HSIMiniPanelType HSIMiniPanel;

/**************************************************************************
 *
 * module variables
 *
 */

static TimerType *timerToSet = NULL;

/**************************************************************************
 *
 * private functions
 *
 */

static Boolean KeypadCallback(UInt16 buttonID, float input) TIMERFORM_SECTION;
static void FormInit(void) TIMERFORM_SECTION;
static void FormDeinit(void) TIMERFORM_SECTION;
static Boolean HandleCtlSelectEvent(EventPtr event) TIMERFORM_SECTION;
static void UpdateDisplay(Boolean full) TIMERFORM_SECTION;

/*
 * function : KeypadCallback
 *
 * Validate the input from the Keypad
 *
 */

Boolean KeypadCallback(UInt16 buttonID, float input) {

	Int32 hours =0 , mins = 0;
	Int32 in = (Int32)input;
	
	if (buttonID == KPUser4)  return true;

	if (buttonID == KPUser3) {

		/*
		 * count-up mode
		 *
		 */

		timerToSet->countdown = false;
		timerToSet->initialValue = 0;
		timerToSet->seconds = 0;
		return true;

	}

	if (in >= 100) {

		hours = (in / 100);
		mins  = (in-(hours*100));

		if (mins > 59) return false;

	} else if (in < 60) {

		mins = in;

	} else {

		return false;

	}

	timerToSet->initialValue = timerToSet->seconds = hours*3600+mins*60;

	if (timerToSet->initialValue == 0) {

		/*
		 * count-up
		 *
		 */

		timerToSet->countdown = false;

	} else {

		timerToSet->countdown = true;

		if (buttonID == KPUser2) {

			timerToSet->autoReset = true;

		}  else {

			timerToSet->autoReset = false;

		}

	}

	return true;

}


/*
 * function : FormInit
 *
 */
static void FormInit(void) 
{

	/*  warning-- don't do any drawing in this routine. */
	/*  Also, don't call FrmSetFocus from here (it must be called *after* */
	/*  FrmDrawForm) */

}

/* 
 * function : FormDeinit
 *
 * Deinitialises the form as it closes
 *
 */

static void FormDeinit(void) {

}

/* 
 * function : HandleCtlSelectEvent
 *
 * Returns true if the event was handled by the function
 *
 */

static Boolean HandleCtlSelectEvent(EventPtr event) {

	Boolean handled = false;
	UInt16 controlID = PFEventGetControlSelectID(event);
	TimerType *timer;

	switch (controlID) {
	
	case Timer1Button:
	case Timer2Button:
	case Timer3Button:
	case Timer4Button:

		timer = &Preferences.timer[controlID-Timer1Button];
		if (timer->state == timerRunning) 
			
			timer->state = timerStopped;

		else 

			timer->state++;

		if (timer->countdown && timer->seconds == 0) timer->seconds = timer->initialValue;
		handled = true;
		UpdateDisplay(true);
		break;

	case Timer1Selector:
	case Timer2Selector:
	case Timer3Selector:
	case Timer4Selector:

		timerToSet = &Preferences.timer[controlID-Timer1Selector];
		KeypadDialogInit(KeypadCallback, 
				(float)((timerToSet->initialValue/3600)*100)+(timerToSet->initialValue/60)%60, 
				0, StrSetTimer, TimerButtons);
		GUIFormPopup(KeypadDialog);
		handled = true;
		break;

	case Timer1LabelSelector:
	case Timer2LabelSelector:
	case Timer3LabelSelector:
	case Timer4LabelSelector:

		timerToSet = &Preferences.timer[controlID-Timer1LabelSelector];
		AlphaPadDialogInit(timerToSet->label);
		GUIFormPopup(AlphaPadDialog);
		handled = true;
		break;

	}

	return handled;
}

/* 
 * function : UpdateForm
 *
 */

static void UpdateDisplay(Boolean full) {

	Int16 j;
	TimerType *timer = &Preferences.timer[0];

	PFScreenLock(false);
	GUIFormDraw();
	
	for (j = 0; j < NUMTIMERS; j++, timer++) {

		char label[32];
		Int32 hours, mins, secs;
		Int32 seconds = timer->seconds;
		
		hours = (seconds / 3600);
		mins = (seconds - (hours * 3600))/60;
		secs = seconds % 60;

		StrPrintF(label,"%02ld:%02ld:%02ld", hours, mins, secs);
		GUIObjectSetLabel(Timer1Selector+j, label);

		//StrPrintF(label,"%02d", secs);
		//GUIObjectSetLabel(Timer1Seconds+j, label, true);

		if (full) {

			GUIObjectSetLabel(Timer1LabelSelector+j, timer->label);
			GUIObjectSetLabel(Timer1Button+j, TimerModes[timer->state]);

		}

	}

	if (HSIMiniPanel) HSIMiniPanelDraw(HSIMiniPanel);
	
	PFScreenUnlock();
	PFSendSimpleEvent(evtScreenRedrawn);

}


/**************************************************************************
 *
 * public functions
 *
 */

/* 
 * function : TimerFormHandleEvent
 *
 * Returns true if the event is handled
 *
 */

Boolean TimerFormHandleEvent(EventPtr event)
{
	Boolean handled = false;
	char *inputStr;

	switch (PFEventGetType(event)) 
	{
	case frmOpenEvent:
		FormInit();
		GUIFormResize(false, false);
		UpdateDisplay(true);
		handled = true;
		break;
			
	case winDisplayChangedEvent:
		if ( GUIFormResize(false, false)) UpdateDisplay(false);
		handled = true;
		break;
		
	case evtGPSSatUpdate:
	case evtGPSSyncLost:
	case evtGPSFixLost:
	case nilEvent:
	case evtGPSPositionUpdate:
		if (!GUIMenuIsDisplayed()) 	UpdateDisplay(false);
		handled = true;
		break;

	case menuEvent:
		if (PFEventGetMenuID(event) == MnDivertEmergency) {
			
			GUIFormGoto(MapForm);
			handled = true;

		}
		break;

	case ctlSelectEvent:
		handled = HandleCtlSelectEvent(event);
		break;

	case evtAlphaInput:
		inputStr = AlphaPadGetInput();
		if (inputStr) {

			if (StrLen(inputStr)) {

				StrNCopy(timerToSet->label, inputStr, 6);
				timerToSet->label[6] = 0;

			}

			PFMemFree(inputStr);
			UpdateDisplay(true);

		}
		handled = true;
		break;
		
	case frmCloseEvent:
		FormDeinit();
		handled = false;
		break;

	default:
		break;
	}	

	return handled;
}

