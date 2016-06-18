/******************************************************************************
 *
 * GPSForm.c
 *
 * (c) Blackhawk Systems Ltd 2002.
 */

#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif
#include "Platform.h"
#include <FloatMgr.h>
#include "ResourceDefines.h"
#include "FMStrings.h"

#include "Utils.h"
#include "Gps.h"

#include "GlobalTypes.h"
#include "GPSForm.h"

#include "CpInterface.h"

#include "MathLib.h"


#include "AlarmManager.h"
#include "Instruments.h"

#include "FMPreferences.h"
#include "HeadingIndicator.h"

/**************************************************************************
 *
 * global variables
 *
 */

extern const HSIMiniPanelType HSIMiniPanel;


extern GPSType GPS;

/*
 * Allows form to control the GPS status
 *
 */

extern Boolean GPSState;

/*
 * GPS source required
 *
 */

extern FMPreferencesType Preferences;

extern const FlightPlanType FlightPlan;

extern const AppColourPrefsType AppColourPrefs;

extern const IconWindowsType IconWindows;
extern const UserConversionType UC;

/**************************************************************************
 *
 * module variables
 *
 */

static SatConstType satConst;

/*
 * mag variation templates
 *
 */

static const char *mvWestTemplate = "%sW";
static const char *mvEastTemplate = "%sE";

static const char *blankLat="--\260--.----'-";

/**************************************************************************
 *
 * private functions
 *
 */

static void GPSFormInit(void) GPSFORM_SECTION;
static void GPSFormDeinit(void) GPSFORM_SECTION;
static Boolean HandleCtlSelectEvent(EventPtr event) GPSFORM_SECTION;
static void UpdateDisplay(void) GPSFORM_SECTION;
static Boolean HandleKeyDownEvent(EventPtr evt) GPSFORM_SECTION;
static void UpdateAltitude(void) GPSFORM_SECTION;

/*
 * function : GPSFormInit
 *
 * Initialises the controls on the form according to the
 * state of the GPS.
 */
static void GPSFormInit(void) {
	UInt16 pushButton;

	/*  warning-- don't do any drawing in this routine. */
	/*  Also, don't call FrmSetFocus from here (it must be called *after* */
	/*  FrmDrawForm) */

	/* Set state of push buttons and status field */
	if (GPSState && !GPSSimulating())

		pushButton = StartPushButton;

	else if (GPSState && GPSSimulating)

		pushButton = SimPushButton;

	else
		pushButton = StopPushButton;

	GUIObjectGroupSetValue(GPSPushButtonGroup, pushButton);

	if (GPSState && !GPSSimulating())
		GUIObjectHide( SimPushButton);

	satConst = SatConstNew(&GPS, 4, 54, 320, 160);

}

/* 
 * function : GPSFormDeinit
 *
 * Deinitialises the form as it closes
 *
 */

static void GPSFormDeinit(void) {

	SatConstFree(satConst);

}

/* 
 * function : HandleCtlSelectEvent
 *
 * Returns true if the event was handled by the function
 */
static Boolean HandleCtlSelectEvent(EventPtr event) {

	Boolean handled = false;
	Err error;
	double lat= DEG_TO_RAD(51+(40.0/60.0));
	double lon= DEG_TO_RAD(2+(3.0/60.0));

	switch (PFEventGetControlSelectID(event)) {

	case StartPushButton:

		if (HostGremlinIsRunning())
			break;

		if (GPSState && GPSSimulating()) {

			GPSClose();
			GPSState = false;

		}

		if (GPSState) {

			handled = true;
			break;

		}

		GPSState = true;
		error = GPSInit(Preferences.gpsSource, &Preferences.bluetoothID);

		if (error) {

			char tmp[9];

			StrPrintF(tmp, "%0X", error);
			GUICustomAlertShow(SerialPortAlert,tmp,NULL,NULL);
			GUIObjectGroupSetValue(GPSPushButtonGroup,StopPushButton);
			GPSState = false;

		}

		if (GPSState) {

			GUIObjectHide( SimPushButton);

		}

		handled= true;
		break;

	case StopPushButton:

		if (HostGremlinIsRunning())
			break;

		/*
		 * if user presses stop and it was running then stop the GPS
		 *
		 */

		if (GPSState) {

			GPSState = false;
			GPSClose();
			UpdateDisplay();

			GUIObjectShow( SimPushButton);
		}
		handled= true;
		break;

	case SimPushButton:

		/*
		 * ignore the push if the GPS is already on
		 *
		 */

		if (GPSState && !GPSSimulating()) {

			GUIObjectGroupSetValue(GPSPushButtonGroup,StartPushButton);

			handled = true;
			break;

		}

		lat = INT32_TO_RAD(Preferences.lastMapLat);
		lon = INT32_TO_RAD(Preferences.lastMapLon);

		GPSSetSimParams(&lat, &lon, Preferences.simHeading,
				Preferences.simAltitude, Preferences.simSpeed);

		error = GPSInit(gpsSimulate, &Preferences.bluetoothID);

		/*
		 AlarmSetCondition( MessageDialogDataNew(GPSFix2dString, 1, 
		 "Prx", MnGotoPrx, 
		 NULL, 0, NULL,0, NULL, 0), alarmMessage);
		 AlarmSetCondition( MessageDialogDataNew(GPSFix3dString, 1, 
		 StrMap, MnGotoMap, NULL,0,
		 NULL, 0, NULL,0), alarmMessage);
		 */

		GPSState = true;
		handled = true;
		break;

	case GPSTimeZonePlusButton:
		Preferences.localTimeZone ++;
		if (Preferences.localTimeZone > 26)
			Preferences.localTimeZone = -23;
		UpdateDisplay();
		handled = true;
		break;

	case GPSTimeZoneMinusButton:
		Preferences.localTimeZone--;
		if (Preferences.localTimeZone < -23)
			Preferences.localTimeZone = 26;
		UpdateDisplay();
		handled = true;
		break;

	default:
		break;
	}
	return handled;
}

/*
 * function : UpdateAltitude
 *
 * Updates the altitude field
 *
 */

static void UpdateAltitude(void) {

	char temp[16];

	temp[0] = 0;

	if (GPS.sat.fixType > 2) {

		float alt = GPS.posn.altitude * UC.altitudeConv;

		StrPrintF(temp, "%s %s", FloatToStr(alt, 0), UC.altitudeUnits);

	} else if (GPS.sat.fixType == 1) {

		StrCopy(temp, "---");

	}

	GUIFieldSetText(AltField,temp,true);

}

/* 
 * function : UpdateDisplay
 *
 * Called in response to a evtGPSXXX event which signals that fresh GPS data
 * is available. Updates the fields on the form with the new GPS data
 *
 */

static void UpdateDisplay(void) {

	char temp[30];
	char temp2[30];

	Int16 timeZone = (Int16)Preferences.localTimeZone;

	PFScreenLock(true);

	GUIFormDraw();
	if (GPS.sat.fixType > 1) {

		StrPrintF(temp, "%c%c\260%s\'%c", GPS.posn.northStr[1],
				GPS.posn.northStr[2], &GPS.posn.northStr[3],
				GPS.posn.northStr[0]);
		GUIFieldSetText(LatField, temp, true);

		StrPrintF(temp, "%c%c%c\260%s\'%c", GPS.posn.eastStr[1],
				GPS.posn.eastStr[2], GPS.posn.eastStr[3], &GPS.posn.eastStr[4],
				GPS.posn.eastStr[0]);
		GUIFieldSetText(LonField, temp, true);

		UpdateAltitude();

		if (GPS.posn.magVarn>0) {
			StrPrintF(temp, mvEastTemplate, FloatToStr(GPS.posn.magVarn, 1));
		} else {
			StrPrintF(temp, mvWestTemplate, FloatToStr(-GPS.posn.magVarn, 1));
		}
		GUIFieldSetText(MagField,temp,true);

		/*
		 * DOP fields
		 *
		 */

		GUIFieldSetText(GPSHDOPField, FloatToStr(GPS.sat.hdop,1), true);
		GUIFieldSetText(GPSVDOPField, FloatToStr(GPS.sat.vdop,1), true);
		GUIFieldSetText(GPSPDOPField, FloatToStr(GPS.sat.pdop,1), true);

	} else {

		GUIFieldSetText(LatField, blankLat , true);
		GUIFieldSetText(LonField, blankLat , true);
		GUIFieldSetText(MagField , "" , true);
		GUIFieldSetText(GPSHDOPField, "", true);
		GUIFieldSetText(GPSVDOPField, "", true);
		GUIFieldSetText(GPSPDOPField, "", true);
		UpdateAltitude();

	}

	if (GPS.sat.fixType < 2) {

		StrCopy(temp, StrNoFix);

	} else {

		if (GPSSimulating()) {

			StrPrintF(temp, StrSim);

		} else {

			StrPrintF(temp, "%d-D %s", GPS.sat.fixType, GPS.sat.waas ? "DGPS"
					: "");

		}

	}
	GUIFieldSetText(GPSStatusField, temp, true);

	if (GPS.sat.fixType > 0) {

		Int16 hour = GPS.posn.utc.hour;
		Int16 min = GPS.posn.utc.minute;

		Int16 dayMins = hour * 60 + min;

		/*
		 * got time info, set UTC & local
		 *
		 */

		StrPrintF(temp, "%02d:%02d", hour, min);

		dayMins += Preferences.localTimeZone*30;
		WRAPMAX(dayMins, 1440);

		hour = dayMins / 60;
		min = dayMins % 60;
		StrPrintF(temp2, "%02d:%02d", hour, min);

	} else {

		temp[0] = 0;
		temp2[0] = 0;

	}

	GUIFieldSetText(GPSUTCField, temp,true);
	GUIFieldSetText(GPSLocalField, temp2, true);

	/*
	 * set time zone
	 *
	 */

	if (timeZone < 0)
		timeZone = -timeZone;
	StrPrintF(temp, "%s%d:%02d", Preferences.localTimeZone < 0 ? "-" : " ",
			timeZone / 2, (timeZone & 1) ? 30 : 0);
	GUIFieldSetText(GPSTimeZoneField, temp, true);

	SatConstDraw(satConst);

	if (HSIMiniPanel) HSIMiniPanelDraw(HSIMiniPanel);

	PFScreenUnlock();

	PFSendSimpleEvent(evtScreenRedrawn);

}

/*
 *  HandleKeyDownEvent
 *
 */

static Boolean HandleKeyDownEvent(EventPtr event) {

	Boolean handled = false;

	switch (PFEventGetKeyChr(event)) {

	case vchrPageDown:
		if (Preferences.gpsAltOffset > -1000)
			Preferences.gpsAltOffset -= 50;
		handled = true;
		break;

	case vchrPageUp:
		if (Preferences.gpsAltOffset < 1000)
			Preferences.gpsAltOffset += 50;
		handled = true;
		break;

	}

	if (handled) {

		GPSSetAltitudeCorrection((float)Preferences.gpsAltOffset);
		UpdateAltitude();

	}

	return handled;

}
/**************************************************************************
 *
 * public functions
 *
 */

/* 
 * function : GPSFormHandleEvent
 *
 * Returns true if the event is handled
 */
Boolean GPSFormHandleEvent(EventPtr event) {
	Boolean handled = false;

	switch (PFEventGetType(event)) {
	case frmOpenEvent:
		GPSFormInit();
		GUIFormResize(false, false);
		UpdateDisplay();
		handled = true;
		break;

	case winDisplayChangedEvent:
		if (GUIFormResize(false, false)) UpdateDisplay();
		handled = true;
		break;

	case evtGPSPositionUpdate:
	case evtGPSSatUpdate:
	case evtGPSSyncLost:
	case evtGPSFixLost:
		if (!GUIMenuIsDisplayed()) UpdateDisplay();
		handled = true;
		break;

	case menuEvent:
		if (PFEventGetMenuID(event) == MnDivertEmergency) {

			GUIFormGoto(MapForm);
			handled = true;

		}
		break;

	case ctlSelectEvent:
		//GUIFormPopup(AlphaPadDialog);
		handled = HandleCtlSelectEvent(event);
		break;

	case keyDownEvent:
		handled = HandleKeyDownEvent(event);
		break;

	case frmCloseEvent:
		GPSFormDeinit();
		handled = false;
		break;

	default:
		break;
	}
	return handled;
}

