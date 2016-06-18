/*******************************************************************************
 *
 * SaveFlightDialog.c
 *
 */

#include "Platform.h"
#include "GlobalTypes.h"
#include "SaveFlightDialog.h"
#include "FlightDatabase.h"
#include "ResourceDefines.h"

#include "FMPreferences.h"

/******************************************************************************
 *
 * global data
 *
 */

extern FMPreferencesType Preferences;
extern const FlightPlanType FlightPlan;
extern const FlightPlanStackType FlightPlanStackB;

/******************************************************************************
 *
 * module data
 *
 */

/******************************************************************************
 *
 * private functions
 *
 * are declared here to allow them to be relocated to another code section
 *
 */

static void InitDialog(void) MODALDIALOGS_SECTION;

/*
 * function : InitDialog
 *
 * Initialises the dialog
 *
 */

static void InitDialog(void) {

	if (StrLen(Preferences.FlightPlanName)) 
		GUIFieldSetText(SaveFlightFileNameField, Preferences.FlightPlanName, false);

}

/******************************************************************************
 * 
 * public functions:
 *
 */

/*
 * function : SaveFlightDialogHandleEvent
 *
 */

Boolean SaveFlightDialogHandleEvent(EventPtr event) {

	Boolean handled = false;
	Boolean savedOk = false;
	char *inputName;

	switch (PFEventGetType(event)) {

	case frmOpenEvent:
		InitDialog();
		GUIFormDraw();		handled = true;
		break;

	case ctlSelectEvent:

		switch (PFEventGetControlSelectID(event)) {

		case SaveFlightOKButton: 
			inputName = GUIFieldGetText(SaveFlightFileNameField);
			if (inputName && StrLen(inputName)) {

				savedOk = FlightDBSaveFlight(GUIFieldGetText(SaveFlightFileNameField), FlightPlan, FpStackPeek(FlightPlanStackB, 0), false);

				if (!savedOk && GUIAlertShow(FlightExistsAlert) == 0) {

					savedOk = FlightDBSaveFlight(GUIFieldGetText(SaveFlightFileNameField), FlightPlan, FpStackPeek(FlightPlanStackB,0), true);

					if (!savedOk) ErrThrow(1966);

				}

			}

			if (savedOk) {

				StrNCopy(Preferences.FlightPlanName, inputName, sizeof(Preferences.FlightPlanName));
				GUIFormReturn();

			}

			handled = true;
			break;

		case SaveFlightCancelButton:
			handled = true;
			GUIFormReturn();
			break;
			
		}

		break;
			

	default:
		break;
	}

	return handled;

}
