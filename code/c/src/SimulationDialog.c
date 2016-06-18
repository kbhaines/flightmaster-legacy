/*
 * SimulationDialog.c
 *
 * 
 *
 */

#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif

#include "Platform.h"
#include <PalmOSGlue.h>
#include "FMStrings.h"

#include "ResourceDefines.h"
#include "SimulationDialog.h"

#include "GlobalTypes.h"
#include "Utils.h"
#include "Modules.h"
#include "MathLib.h"
#include "Gps.h"


#include "FMPreferences.h"

/*******************************************************************************
 * 
 * global variables
 *
 * 
 */

extern FMPreferencesType Preferences;

extern const UserConversionType UC;

extern GPSType GPS;

/*******************************************************************************
 * 
 * module variables
 *
 * 
 */

#define SmErrThrow ErrThrow(SimulationDialogModuleID | __LINE__)


/*******************************************************************************
 *
 * private functions
 *
 * are declared here to allow easy relocation to another code section
 * 
 */

static void FormInit(void) MODALDIALOGS_SECTION;
static void FormDeinit(void) MODALDIALOGS_SECTION;
static Boolean ValidateAndSave(void) MODALDIALOGS_SECTION;

/*
 * function : FormInit
 *
 */

static void FormInit(void) {

	char tmp[40];
	float simHeading = Preferences.simHeading;

	if (Preferences.useMagnetic) {

		simHeading -= GPS.posn.magVarn;
		WRAPMAX(simHeading,360);

	}

	StrPrintF(tmp,"%d", (Int16)(simHeading));
	GUIFieldSetText(SimHeadingField, tmp, false);
	
	StrPrintF(tmp,"%d", (Int16)(round((double)(Preferences.simAltitude*UC.altitudeConv))));
	GUIFieldSetText(SimAltitudeField, tmp, false);
	
	StrPrintF(tmp,"%d", (Int16)(round((double)(Preferences.simSpeed*UC.speedConv))));
	GUIFieldSetText(SimSpeedField, tmp, false);

}


/*
 * function : FormDeinit
 *
 */

static void FormDeinit(void) {


}


/*
 * function : ValidateAndSave
 *
 */

static Boolean ValidateAndSave(void) {

	float newHeading;
	float newAltitude;
	float newSpeed;
	
	newHeading = (float)StrAToI(GUIFieldGetText(SimHeadingField));
	if (newHeading < 0 || newHeading > 359) {
		
		GUICustomAlertShow(SimulationAlert,StrHeading,"359", StrDegrees);
		return false;
		
	}	

	if (Preferences.useMagnetic) {

		newHeading += GPS.posn.magVarn;

		if (newHeading<0) newHeading+=360;
		else if (newHeading >= 360) newHeading-=360;

	}

	newAltitude = (float)StrAToI(GUIFieldGetText(SimAltitudeField)) / UC.altitudeConv;
	if (newAltitude < 0 || newAltitude > 10000) {
		
		char tmp[10];

		StrPrintF(tmp, "%ld", (Int32)(UC.altitudeConv*10000));
		GUICustomAlertShow(SimulationAlert,StrAltitude,tmp,UC.altitudeUnits);
		return false;
		
	}	
	
	newSpeed = (float)StrAToI(GUIFieldGetText(SimSpeedField)) / UC.speedConv;
	if (newSpeed < 0 || newSpeed > 999) {
		
		char tmp[10];

		StrPrintF(tmp, "%ld", (Int32)(UC.speedConv*999));
		GUICustomAlertShow(SimulationAlert,StrSpeed,tmp,UC.speedUnits);
		return false;
		
	}	

	Preferences.simHeading = newHeading;
	Preferences.simAltitude= newAltitude;
	Preferences.simSpeed   = newSpeed;

	return true;
}


/*******************************************************************************
 *
 * public functions
 *
 */

/*
 * function : SimulationDialogHandleEvent
 *
 */

Boolean SimulationDialogHandleEvent(EventPtr event)
{
	Boolean handled = false;

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

	case ctlSelectEvent:

		switch (PFEventGetControlSelectID(event)) {
		case SimOKButton:
			if (ValidateAndSave()) {

				GPSSetSimParams(NULL,NULL, Preferences.simHeading, 
						Preferences.simAltitude, Preferences.simSpeed);
				FormDeinit();
				GUIFormReturn();

			}
			handled = true;
			break;

		case SimCancelButton:
			FormDeinit();
			GUIFormReturn();
			break;

		default:
			break;
		}

		handled = true;
		break;

	case frmCloseEvent:
		FormDeinit();
		break;

	default:
		break;
	}
			
	return handled;
}
