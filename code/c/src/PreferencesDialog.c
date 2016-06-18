/*
 * PreferencesDialog.c
 *
 * (c) 2002 Blackhawk Systems
 *
 */

#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif

#include "Platform.h"
#include "Modules.h"
#include "GlobalTypes.h"
#include "ResourceDefines.h"

/* yes, we're including a .c file !!!
 * This is to allow inlining of the registration
 * generator
 */
#include "RegistrationCode.c"
#include "Utils.h"
#include "SelectCopilotFlightDialog.h"

#include "Constants.h"
#include "PreferencesDialog.h"

#include "MorePreferencesDialog.h"



/*
 * we need SetDisplayUnits
 *
 */
#include "MainForm.h"

#include "FlightPlan.h"
#include "CpInterface.h"

#include "FMPreferences.h"

#define ModuleID PreferencesModuleID

/*******************************************************************************
 *
 * Global variables
 *
 */

extern FMPreferencesType Preferences;
extern UInt8             ValidRegistration;
extern FlightPlanType    FlightPlan;
extern DisplayUnitsType  DisplayUnits;
extern UInt32 	  		 DemoDateLimit;

/*******************************************************************************
 *
 * private functions
 *
 * are declared here to allow easy relocation to another code section
 * 
 */

static Boolean ValidateAndStorePreferences(void) MODALDIALOGS_SECTION;
static void SetupPreferencesDialog(void) MODALDIALOGS_SECTION;

/*
 * function : ValidateAndStorePreferences
 *
 */
static Boolean ValidateAndStorePreferences(void){
	UInt32       newCode;

	char        *txt;

	/* 
	 * Check registration code - if it changed then validate it
	 *
	 */
	
	txt = GUIFieldGetText(RegistrationCodeField);

	newCode = (UInt32) StrAToI(txt);

	if (newCode != Preferences.registrationCode32) {

		char *uid = PFGetUserID();
		char *serialNumber = PFGetSerialNumber();

		if (IsValidCode(newCode, uid) || IsValidCode(newCode, serialNumber)) {

			LOGLINE;

			GUIAlertShow(RegistrationSucceededAlert);
			ValidRegistration = 129;

		} else {

			LOGLINE;

			/*
			 * check for valid demonstration code
			 *
			 */

			if (!GetDemoDate(newCode, uid, REGCODESEED, DEMOCODEPRIME, YEAREPOCH) || 
					!HandleDemoChecks(newCode, uid, &DemoDateLimit, REGCODESEED, DEMOCODEPRIME, YEAREPOCH)) {
				
				GUIAlertShow(RegistrationFailedAlert);
				PFMemFree(uid);
				PFMemFree(serialNumber);
				return false;

			}

		}

		PFMemFree(uid);
		PFMemFree(serialNumber);

	}

	/* 
	 * checks complete, set the data in Preferences
	 *
	 */

	Preferences.registrationCode32 = newCode;

	Preferences.autoStartGPS = (Boolean) GUIObjectGetValue(AutoStartGPSCheckbox);
	Preferences.oneSecUpdate = (Boolean) GUIObjectGetValue(PrefOneSecUpdateCheckbox);
	Preferences.gpsSource = GUIListGetSelection(PrefGPSSourceList); 

	/* 
	 * read the units, headings and runway units preferences from the form
	 *
	 */

	Preferences.units = GUIObjectGroupGetValue(UnitsPushButtonGroup) - NmPushButton;
	Preferences.useMagnetic = GUIObjectGroupGetValue(HeadingsPushButtonGroup) == MagneticPushButton;

	return true;
}

/*
 * function : SetupPreferencesDialog
 *
 * Fill the fields on the Preferences Dialog with the values from
 * the Preferences structure.
 *
 */
static void SetupPreferencesDialog(void) {
	char tmp[16];

	/* 
	 * Units and Headings pushbuttons
	 *
	 */

	if (!CpIsInstalled()) GUIObjectHide(CpPushButton);

	GUIObjectGroupSetValue(UnitsPushButtonGroup, Preferences.units+NmPushButton);
	GUIObjectGroupSetValue(HeadingsPushButtonGroup, Preferences.useMagnetic ? MagneticPushButton : TruePushButton);

	/*
	 * GPS Input selector, and Autostart
	 *
	 */

	GUIListSetSelection(PrefGPSSourceList, (UInt16)Preferences.gpsSource);
	GUIObjectSetLabelPtr(PrefGPSPopup, GUIListGetItemText(PrefGPSSourceList, (Int16)Preferences.gpsSource));
	
	GUIObjectSetValue(AutoStartGPSCheckbox, (Int16)Preferences.autoStartGPS);
	GUIObjectSetValue(PrefOneSecUpdateCheckbox, (Int16)Preferences.oneSecUpdate);

	/*
	 * Registration code
	 *
	 */

	StrPrintF(tmp,"%lu",Preferences.registrationCode32);
	GUIFieldSetText(RegistrationCodeField,tmp,false);

}

/*******************************************************************************
 *
 * public functions
 *
 */


/*
 * function : PreferencesEventhandler
 *
 */

Boolean PreferencesDialogHandleEvent(EventPtr event) {

	Boolean handled = false;
	GPSSourceType gpsSource;
	UInt8   units;

	switch (PFEventGetType(event)) {

	case frmOpenEvent:
		GUIFormResize(true, true);
		SetupPreferencesDialog();
		GUIFormDraw();
		handled = true;
		break;

	case frmCloseEvent:
		ValidateAndStorePreferences();
		SetDisplayUnits(Preferences.units);
		handled = false;
		break;

	case winDisplayChangedEvent:
		GUIFormResize(true, true);
		handled = true;
		GUIFormDraw();
		handled = true;
		break;

	case ctlSelectEvent:

		/*
		 * if OK pressed then validate the preferences
		 *
		 */

		switch (PFEventGetControlSelectID(event)) {
		case  PrefOKButton:
			if (ValidateAndStorePreferences()){
				GUIFormReturn();
			}
			handled = true;
			break;

		case MorePrefsButton:

			/*
			 * have to call SetDisplayUnits because the ATZ radius
			 * on the More Preferences form needs to be specified
			 * in the current unit settings that are on this form. 
			 *
			 */

			units = (UInt8) (GUIObjectGroupGetValue(UnitsPushButtonGroup) - NmPushButton);
			SetDisplayUnits(units);
			GUIFormPopup(MorePreferencesDialog);
			handled = true;
			break;

		case PrefCancelButton:
			GUIFormReturn();
			handled = true;
			break;

		}
		break;

	case popSelectEvent:

		/*
		 * verify that the user has selected a GPS input source
		 * that is supported by the device
		 *
		 */

		gpsSource = (GPSSourceType)PFEventGetPopupSelection(event);
		/*
		 * if the source isn't available, reset the list selection. Notice the
		 * setting of handled to true, this stops further processing of the
		 * popSelect event which stops the label from being automatically changed
		 * to the new value by PalmOS.
		 *
		 */

		if (!GPSSourceSupported(gpsSource)) {
			
			UInt16 alertId;
			
			switch (gpsSource) {

			case gpsBluetooth:
				alertId = BluetoothAlert;
				break;

			case gpsCard:
				alertId = CardInputAlert;
				break;

			default:
			case gpsGarmin:
				alertId = GarminInputAlert;
				break;

				
			}

			GUIAlertShow(alertId);
			GUIListSetSelection(PrefGPSSourceList,PFEventGetPopupPriorSelection(event));
			handled = true;
		}
		break;

	default:
		break;
	}
	LOGINT16(PFEventGetType(event));
	LOGINT16(handled);
	return handled;
}

