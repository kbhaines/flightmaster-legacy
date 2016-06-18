/*
 * VNAVDialog.c
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
#include "VNAVDialog.h"
#include "Utils.h"

#include "FMStrings.h"

#include "FlightPlan.h"
#include "KeypadDialog.h"

#include "NavManager.h"
#include "FMPreferences.h"


/*******************************************************************************
 *
 * global variables
 *
 */

extern FlightPlanType FlightPlan;
extern const UserConversionType UC;
extern const DisplayUnitsType DisplayUnits;
extern FMPreferencesType Preferences;

/*******************************************************************************
 * 
 * module variables
 *
 * 
 */

static VNAVDataType vnd;

/*******************************************************************************
 *
 * private functions
 *
 * are declared here to allow easy relocation to another code section
 * 
 */

static void FormInit(void) MODALDIALOGS_SECTION;
static void FormDeinit(Boolean saveValues) MODALDIALOGS_SECTION;
static void SetControlLabels(void) MODALDIALOGS_SECTION;
static Boolean AltitudeCallback(UInt16 button, float value) MODALDIALOGS_SECTION;
static Boolean RateCallback(UInt16 button, float value) MODALDIALOGS_SECTION;
static Boolean ByCallback(UInt16 button, float value) MODALDIALOGS_SECTION;
static Boolean GPSAltCallback(UInt16 button, float value) MODALDIALOGS_SECTION;

/*
 * FormInit
 *
 */

static void FormInit(void) {

	Int16 currentLeg = FpGetCurrentLeg(FlightPlan);
	const VNAVDataType *legVnd = FpGetVNAV(FlightPlan, currentLeg);


	/*
	 * set up local copy of VNAV data structure
	 *
	 */

	if (legVnd) {

		/*
		 * already in flight plan - use these values
		 *
		 */

		vnd = *legVnd;

	} else {

		/*
		 * fill with some sensible initial values
		 *
		 */

		if (currentLeg < FpGetNumLegs(FlightPlan)-1) {

			/*
			 * use altitude from next leg of flight plan
			 *
			 */

			const FlightPlanLegType *nextLeg = FpGetLeg(FlightPlan, currentLeg+1, false);

			if (nextLeg->altitude > 0.0) {
				
				vnd.altType = vnavAltAMSL;
				vnd.targetAltitude = nextLeg->altitude;

			} else {
				
				vnd.altType = Preferences.vnavEnroute.altType;
				vnd.targetAltitude = Preferences.vnavEnroute.targetAltitude;
				
			}
			
			vnd.rateType = Preferences.vnavEnroute.rateType;
			vnd.distanceBefore = Preferences.vnavEnroute.distanceBefore;
			vnd.targetRate = Preferences.vnavEnroute.targetRate;

		} else {

			vnd = Preferences.vnavApproach;
			
		}

	}

	SetControlLabels();

}

/*
 * FormDeinit
 *
 */

static void FormDeinit(Boolean saveValues) {

	if (saveValues) {
		
		if (FpGetCurrentLeg(FlightPlan) < FpGetNumLegs(FlightPlan)-1) {
			
			Preferences.vnavEnroute = vnd;
			
		} else {
			
			Preferences.vnavApproach = vnd;
			
		}

	}
	
}

/*
 * SetControlLabels
 *
 * Sets the labels on the selector controls, for altitude, rate and 'by'
 *
 */

static void SetControlLabels(void) {

	char label[32];

	StrPrintF(label, "%s %s %s", 
			FloatToStr(vnd.targetAltitude*UC.altitudeConv,0), 
			UC.altitudeUnits,
			vnd.altType == vnavAltAMSL ? VNAltButtons[0] : VNAltButtons[1] );
	GUIObjectSetLabel(VnAltSelector, label);

	if (vnd.rateType == vnavDegrees) {

		StrPrintF(label, "%s %s", FloatToStr(RAD_TO_DEG(vnd.targetRate),1), VNRateButtons[0][2]);

	} else {

		StrPrintF(label, "%s %s", 
				FloatToStr(vnd.targetRate*UC.altitudeConv,0),
				VNRateButtons[DisplayUnits.altitude][vnd.rateType]);

	}
	GUIObjectSetLabel(VnRateSelector, label);

	if (vnd.distanceBefore == 0.0) {

		StrCopy(label, VNByButtons[1]);

	} else {

		StrPrintF(label, "%s %s",
				FloatToStr(vnd.distanceBefore*UC.distanceConv,1),
				UC.distanceUnits);

	}
	GUIObjectSetLabel(VnBySelector, label);

}



/*
 * AltitudeCallback
 *
 * Called by the Keypad Dialog when the user has input an altitude
 *
 */

static Boolean AltitudeCallback(UInt16 button, float value) {

	if (button == KPUser3) return true;  // cancelled

	if (value < 0.0 || value > 60000) return false;

	switch (button) {
	
	case KPUser1:	// AMSL
		vnd.altType = vnavAltAMSL;
		vnd.targetAltitude = value / UC.altitudeConv;
		break;

	case KPUser2:	// Above WP
		vnd.altType = vnavAltAboveWP;
		vnd.targetAltitude = value / UC.altitudeConv;
		break;

	}

	return true;

}

/*
 * RateCallback
 *
 * Called by the Keypad Dialog when the user has input a rate of descent
 *
 */

static Boolean RateCallback(UInt16 button, float value) {

	if (button == KPUser4) return true; // cancelled

	if (value < 0.0) return false;

	switch (button) {

	case KPUser1:	// ft/min
		if (value > 3000.0) return false;
		vnd.rateType = vnavRatePerMin;
		vnd.targetRate= value / UC.altitudeConv;
		break;

	case KPUser2:	// ft/nm
		if (value > 3000.0) return false;
		vnd.rateType = vnavRatePerMile;
		vnd.targetRate= value / UC.altitudeConv;
		break;

	case KPUser3:	// degrees
		if (value > 6.0) return false;
		vnd.rateType = vnavDegrees;
		vnd.targetRate = DEG_TO_RAD(value);
		break;

	default:
		break;

	}
		
	return true;

}

/*
 * ByCallback
 *
 * Called by the Keypad Dialog when the user has input a by-distance
 *
 */

static Boolean ByCallback(UInt16 button, float value) {

	if (button == KPUser3) return true; // cancelled

	if (value < 0.0 || value > 100.0) return false;

	switch (button) {

	case KPUser1:	// OK
		vnd.distanceBefore = value / UC.distanceConv;
		break;

	case KPUser2:	// Waypoint
		vnd.distanceBefore = 0.0;
		break;

	default:
		break;

	}

	return true;

}

/*
 * GPSAltCallback
 *
 * Called by Keypad Dialog when user has set an altitude offset
 *
 */

static Boolean GPSAltCallback(UInt16 button, float value) {

	if (button == KPUser3) return true; // cancelled

	if (value < 0.0 || value > 100000) return false;

	switch (button) {

	case KPUser1:	// AMSL
		 GPSSetAMSLAltitude(value / UC.altitudeConv);
		 break;

	case KPUser2:	// Reset
		 GPSSetAMSLAltitude(-1.0);
		 break;

	default:
		 break;

	}

	return true;

}

/*******************************************************************************
 *
 * public functions
 *
 */

/*
 * function : VNAVDialogHandleEvent
 *
 */

Boolean VNAVDialogHandleEvent(EventPtr event) {

	Boolean       handled        = false;

	FntSetFont(largeBoldFont);

	switch (PFEventGetType(event)) {

	case frmOpenEvent:
		GUIFormResize(true, true);
		FormInit();
		GUIFormDraw();
		handled = true;
		break;
		
	case winDisplayChangedEvent:
		GUIFormResize(true, true);
		GUIFormDraw();
		handled = true;
		break;

	case frmCloseEvent:
		FormDeinit(false);
		break;

	case evtKeypadInput:
		SetControlLabels();
		handled = true;
		break;
	
	case evtGPSPositionUpdate:

		//SetGPSAltitudeSelector();
		break;

	case ctlSelectEvent:

		switch (PFEventGetControlSelectID(event)) {

		float     rate;
		UInt16    precision;

		case VnAltSelector:
			KeypadDialogInit(AltitudeCallback, vnd.targetAltitude*UC.altitudeConv,0,
					VNAltPrompt, VNAltButtons);
			GUIFormPopup(KeypadDialog);
			handled = true;
			break;

		case VnRateSelector:
			if (vnd.rateType == vnavDegrees) {

				rate = RAD_TO_DEG(vnd.targetRate);
				precision = 1;

			} else {

				rate = vnd.targetRate * UC.altitudeConv;
				precision = 0;

			}

			KeypadDialogInit(RateCallback, rate, precision,
					VNRatePrompt, VNRateButtons[DisplayUnits.altitude]);
			GUIFormPopup(KeypadDialog);
			handled = true;
			break;

		case VnBySelector:
			KeypadDialogInit(ByCallback, vnd.distanceBefore * UC.distanceConv, 1, 
					VNByPrompt, VNByButtons);
			GUIFormPopup(KeypadDialog);
			handled = true;
			break;

		case VnGPSAltSelector:
			KeypadDialogInit(GPSAltCallback, NavGetFloatValue(navAltitude), 0, 
					VNGPSAltPrompt, VNGPSButtons);
			GUIFormPopup(KeypadDialog);
			handled = true;
			break;

		case VnOKButton:
			FpSetVNAV(FlightPlan, FpGetCurrentLeg(FlightPlan), &vnd);
			FormDeinit(true);
			GUIFormReturn();
			handled = true;
			break;

		case VnCancelButton:
			FormDeinit(false);
			GUIFormReturn();
			handled = true;
			break;
			
		case VnClearButton:

			FpClearVNAV(FlightPlan, FpGetCurrentLeg(FlightPlan));
			FormDeinit(false);
			GUIFormReturn();
			handled = true;
			break;
			
		}

		break;

	default:
		break;

	}
	return handled;
}
