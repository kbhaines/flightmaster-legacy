/******************************************************************************
 *
 * Main.c
 *
 * FlightMaster main application module
 *
 * (c) 2002 Blackhawk Systems Ltd.
 *
 */

#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif

#include "Platform.h"

#include "Gps.h"
#include "ResourceDefines.h"

#include "MainForm.h"
#include "GPSForm.h"
#include "FlightPlanForm.h"
#include "DiversionForm.h"
#include "MapForm.h"
#include "TimerForm.h"

//#include "EditWaypointForm.h"
#include "MapDialog.h"
#include "SimulationDialog.h"
#include "SelectCopilotFlightDialog.h"
#include "CommandDialog.h"
#include "KeypadDialog.h"
#include "AlphaPadDialog.h"

#include "WPInfoDialog.h"
#include "TextDialog.h"

#include "PreferencesDialog.h"
#include "MorePreferencesDialog.h"
#include "MessageDialog.h"
#include "MonitorDialog.h"
#include "SaveFlightDialog.h"

#include "HeadingIndicator.h"
#include "AIGauge.h"

#include "Utils.h"
#include "Constants.h"
#include "GlobalTypes.h"
#include "Upgrades.h"
#include "MathLib.h"
#include "WDManager.h"

#include "AlarmManager.h"
#include "VNAVDialog.h"
#include "FlightDatabase.h"

/* yes, we're including a .c file !!!
 * This is to allow inlining of the registration
 * generator
 */

#include "RegistrationCode.c"
#include "CpInterface.h"
#include "DiversionMgr.h"

#include "AsDatabase.h"
#include "PalmNavigator.h"
#include "Colours.h"
#include "NavManager.h"

#include "FMStrings.h"
#include "GarminChars.h"
#include "TrackLog.h"
#include "Instruments.h"

#include "TerrainType.h"
#include "OBSManager.h"
#include "Modules.h"

#include "FMPreferences.h"

#define ModuleID MainModuleID

/*******************************************************************************
 *
 * Global Variables
 *
 */

/*
 * Indicates if the user has started the GPS
 * 
 */

Boolean GPSState;

/*
 * Store data received from the GPS in this structure
 *
 */

GPSType GPS;

/*
 * The Flight Plan - need I say more!
 *
 */

FlightPlanType FlightPlan= NULL;
FlightPlanStackType FlightPlanStack;
FlightPlanStackType FlightPlanStackB;

/*
 * Waypoint dataset
 *
 */

WDMHandle WPDataset;
FMDataset FMDS;

/*
 * set to 129 to indicate a valid registration code is entered
 *
 */

UInt8 ValidRegistration = 0;

/*
 * User preferences, set on the Preferences dialog and in the Waypoint Scanning
 * dialog. Stored in the application preferences section when application exits
 *
 */

FMPreferencesType Preferences;

/*
 * Controls how the units are displayed e.g. Nautical, Statute or Metric
 *
 */

DisplayUnitsType DisplayUnits;
UserConversionType UC;

/*
 * Colour preferences
 *
 */

AppColourPrefsType AppColourPrefs;
Boolean nightMode = false;

/*
 * bitmap windows, icon dimensions and indexes of the various bitmaps
 *
 */

IconWindowsType IconWindows;
IconWindowsType SmallIconWindows;

/*
 * Strings representing the commonly used compass directions
 *
 */

const char *CompassText[] = { "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
		"S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW" };

/*
 * Time strings
 *
 */

const char *Dashes[] = { "", "-", "--", "---", "----" };
const char *Stars[] = { "", "*", "**", "***", "****" };
const char *Blanks[] = { "", " ", "  ", "   ", "    " };

/*
 * FM draws the message flag at this location - it is set
 * on a per-form basis.
 *
 */

PointType MessageFlag;

/*
 * cycle counter, updated every evtGPSPositionUpdate
 *
 */

Int32 CycleCounter = 0;

/*
 * globally available satellite constellation & HSI
 *
 */

SatConstType SatConst= NULL;
HSIMiniPanelType HSIMiniPanel= NULL;

/*
 * global terrain cache. This is global to save us from having to reload the
 * cache everytime the map page is left/entered.
 *
 */

TerrainType Terrain= NULL;

/*
 * Date at which the demo expires
 *
 */

UInt32 DemoDateLimit = 0xFFFFFFFF;

/*******************************************************************************
 *
 * module variables
 *
 */

static Boolean newInstallation = false;

/*
 * Function key to menu/control ID mappings
 *
 * ref: keymappings
 *
 */

static const FunctionKeymapType commandPopupMapping[] = {

{ vchrF1, mapToControlID, CDCloseButton }, { vchrF2, mapToControlID,
		CDUndoButton }, { vchrF3, mapToControlID, CDVNAVButton }, { vchrF4,
		mapToControlID, CDWPInfoButton },

{ vchrPageUp, mapToControlID, CDPrevWaypointButton }, { vchrPageDown,
		mapToControlID, CDNextWaypointButton }, { vchrRockerLeft,
		mapToControlID, CDCRSButton }, { vchrRockerRight, mapToControlID,
		CDWPButton }, { 0, 0, NULL }

};

static const FunctionKeymapType mapFormMapping[] = {

{ vchrF1, mapToMenuID, MnCommandPopup }, { vchrF2, mapToMenuID, MnEditPlan }, {
		vchrF3, mapToMenuID, MnGotoPrx }, { vchrF4, mapToMenuID, MnGotoHSI }, {
		5641, mapToMenuID, MnFlightFlipFlop }, { 0, 0, NULL }

};

static const FunctionKeymapType mainFormMapping[] = {

{ vchrF1, mapToMenuID, MnCommandPopup }, { vchrF2, mapToMenuID, MnEditPlan }, {
		vchrF3, mapToMenuID, MnGotoMap }, { vchrF4, mapToMenuID, MnGotoPlan },

{ vchrPageUp, mapToMenuID, MnPreviousLeg }, { vchrPageDown, mapToMenuID,
		MnNextLeg }, { 5641, mapToMenuID, MnFlightFlipFlop }, { 0, 0, NULL }

};

static const FunctionKeymapType flightPlanFormMapping[] = {

{ vchrF1, mapToMenuID, MnCommandPopup }, { vchrF2, mapToMenuID, MnEditPlan }, {
		vchrF3, mapToMenuID, MnGotoHSI }, { vchrF4, mapToMenuID, MnGotoTimer },

{ vchrPageUp, mapToMenuID, MnPreviousLeg }, { vchrPageDown, mapToMenuID,
		MnNextLeg }, { 5641, mapToMenuID, MnFlightFlipFlop }, { 0, 0, NULL }

};

static const FunctionKeymapType timerFormMapping[] = {

{ vchrF1, mapToMenuID, MnCommandPopup }, { vchrF2, mapToMenuID, MnEditPlan }, {
		vchrF3, mapToMenuID, MnGotoPlan }, { vchrF4, mapToMenuID, MnGotoGPS },
		{ 0, 0, NULL }

};

static const FunctionKeymapType GPSFormMapping[] = {

{ vchrF1, mapToMenuID, MnCommandPopup }, { vchrF2, mapToMenuID, MnEditPlan }, {
		vchrF3, mapToMenuID, MnGotoTimer }, { vchrF4, mapToMenuID, MnGotoPrx },
		{ 0, 0, NULL }

};

static const FunctionKeymapType searchFormMapping[] = {

{ vchrF1, mapToMenuID, MnCommandPopup }, { vchrF2, noMapping, 0 }, { vchrF3,
		mapToMenuID, MnGotoGPS }, { vchrF4, mapToMenuID, MnGotoMap }, { 0, 0,
		NULL }

};

static const FunctionKeymapType vnavDialogMapping[] = {

{ vchrF1, mapToControlID, VnOKButton },
		{ vchrF2, mapToControlID, VnClearButton }, { vchrF3, noMapping, 0 }, {
				vchrF4, mapToControlID, VnCancelButton }, { 0, 0, NULL }

};

static const FunctionKeymapType monitorDialogMapping[] = {

{ vchrF1, mapToControlID, MdOKButton }, { vchrF2, noMapping, 0 }, { vchrF3,
		noMapping, 0 }, { vchrF4, mapToControlID, MdCancelButton }, { 0, 0,
		NULL }

};

static const FunctionKeymapType loadFlightDialogMapping[] = {

{ vchrF1, mapToControlID, SCFOKButton }, { vchrF2, mapToControlID,
		SCFBlankButton }, { vchrF3, mapToControlID, SCFDeleteButton }, {
		vchrF4, mapToControlID, SCFCancelButton },

{ vchrRockerLeft, mapToControlID, SCFFMButton }, { vchrRockerRight,
		mapToControlID, SCFCoPilotButton }, { vchrRockerCenter, mapToControlID,
		SCFNextSegmentButton }, { 0, 0, NULL }

};

static const FunctionKeymapType saveFlightDialogMapping[] = {

{ vchrF1, mapToControlID, SaveFlightOKButton }, { vchrF2, noMapping, 0 }, {
		vchrF3, noMapping, 0 }, { vchrF4, mapToControlID,
		SaveFlightCancelButton }, { 0, 0, NULL }

};

static const FunctionKeymapType mapDialogMapping[] = {

{ vchrF1, mapToMenuID, MnMapConfig }, { vchrF2, noMapping, 0 }, { vchrF3,
		noMapping, 0 }, { vchrF4, noMapping, 0 },

{ 0, 0, NULL }

};

static const FunctionKeymapType messageDialogMapping[] = {

{ vchrF1, mapToControlID, MDButton1 }, { vchrF2, mapToControlID, MDButton2 }, {
		vchrF3, mapToControlID, MDButton3 }, { vchrF4, mapToControlID,
		MDButton4 },

{ 0, 0, NULL }

};

static const FunctionKeymapType alphaDialogMapping[] = {

{ vchrF1, mapToControlID, AlphaPadOK },
		{ vchrF3, mapToControlID, AlphaPadClear }, { vchrF4, mapToControlID,
				AlphaPadCancel }, { 10, mapToControlID, AlphaPadOK },

		{ 0, 0, NULL }

};

static const FunctionKeymapType keypadDialogMapping[] = {

{ vchrF1, mapToControlID, KPUser1 }, { vchrF2, mapToControlID, KPUser2 }, {
		vchrF3, mapToControlID, KPUser3 }, { vchrF4, mapToControlID, KPUser4 },

{ 0, 0, NULL }

};

static const FunctionKeymapType preferencesDialogMapping[] = {

{ vchrF1, mapToControlID, PrefOKButton }, { vchrF2, mapToControlID,
		MorePrefsButton }, { vchrF3, noMapping, 0 }, { vchrF4, mapToControlID,
		PrefCancelButton },

{ 0, 0, NULL } };

static const FunctionKeymapType morePrefsDialogMapping[] = {

{ vchrF1, mapToControlID, MpOKButton }, { vchrF2, noMapping, 0 }, { vchrF3,
		noMapping, 0 }, { vchrF4, mapToControlID, MpCancelButton },

{ 0, 0, NULL } };

static const FunctionKeymapType simulationDialogMapping[] = {

{ vchrF1, mapToControlID, SimOKButton }, { vchrF2, noMapping, 0 }, { vchrF3,
		noMapping, 0 }, { vchrF4, mapToControlID, SimCancelButton },

{ 0, 0, NULL } };

static const FunctionKeymapType wpInfoDialogMapping[] = {

{ vchrF1, mapToControlID, WPOKButton },
		{ vchrF2, mapToControlID, WPGotoButton }, { vchrF3, mapToControlID,
				WPPanMapButton }, { vchrF4, noMapping, 0 }, { 0, 0, NULL }

};

static const FunctionKeymapType textDialogMapping[] = {

// Just an empty mapping, to stop OS-keyhandler from exiting
		// when a key is pressed
		{ vchrF1, noMapping, 0 }, { vchrF2, noMapping, 0 }, { vchrF3,
				noMapping, 0 }, { vchrF4, noMapping, 0 },

		{ 0, 0, NULL }

};

static Boolean disclaimerShown = false;

/******************************************************************************
 *
 * module functions
 *
 * are declared here to allow easy relocation to different code sections
 *
 */

static Int16 IndexUpdateProgess(Int16 j) STARTUP_SECTION;
static Boolean HandleCommonMenuEvent(EventPtr event)  STARTUP_SECTION;
static Boolean OBSCallback(UInt16 button, float value) MODALDIALOGS_SECTION;
static void UseDefaultCopilotFlight(void) STARTUP_SECTION;

static Err AppStart(Boolean launchedFromCoPilot) STARTUP_SECTION;
static void AppStop(void) STARTUP_SECTION;
static void AppEventLoop(void) STARTUP_SECTION;

static void MapFunctionKeyToNewEvent(EventPtr event) STARTUP_SECTION;
static void DrawAlarmBanner(void) STARTUP_SECTION;
static void CheckAndDrawMessageFlag(void) STARTUP_SECTION;
static void UpdateTrackLog(void) STARTUP_SECTION;
static void DoObstacleScan(void) STARTUP_SECTION;
static void UpdateTimers(void) STARTUP_SECTION;

Err FlightMaster(Boolean launchedFromCoPilot) STARTUP_SECTION;

/*
 * IndexUpdateProgess 
 *
 * Callback function to display progress of indexing
 *
 */

static Int16 IndexUpdateProgess(Int16 j) {

	static Int16 k, c = 0;
	char str2[9] = "|/-\\|/-\\";

	if (j == 0)
		k++;

	//StrPrintF(str, "%d (%d)", j, k);
	PFDrawChars(&str2[c++ & 7], 1, 0, k*FntLineHeight());

	//	DebugDrawChar(str2[c++ & 7], 0, k*(FntLineHeight()));

	return 0;

}

/*
 * function : CheckAndDrawMessageFlag
 *
 * Checks the prescence and level of the messages in the Alarm Manager
 * and draws an appropriate coloured flag at 'MessageFlag'
 * 
 * Should be called from every main form after a screen update
 *
 */

static void CheckAndDrawMessageFlag(void) {

	/*
	 * Draw the message flag
	 *
	 */

	if (AlarmGetMaxLevel() > alarmOff) {

		IndexedColorType colour;

		if (CycleCounter & 1) {

			colour = GUIGetSystemColour(GUIObjectForeground);

		} else {

			switch (AlarmGetMaxLevel()) {

			case alarmMessage:
				colour = AppColourPrefs.green;
				break;

			case alarmWarning:
				colour = AppColourPrefs.yellow;
				break;

			case alarmAlert:
				colour = AppColourPrefs.red;
				break;

			case alarmOff:
				colour = AppColourPrefs.red;
				break;

			default:
				colour = AppColourPrefs.black;
				break;

			}

		}

		DrawFlag(&MessageFlag, MessageFlagString, colour);

	}

}

/*
 * function : DrawAlarmBanner
 *
 */

static void DrawAlarmBanner(void) {

	const char *urgentAlarm = AlarmGetMostUrgent();

	if (urgentAlarm) {

		PFScreenRectType r;
		IndexedColorType fore, back;

		PFScreenRectangleSetRel(&r, 1, pfScreen.height
				- pfScreen.largeBoldHeight - 2, pfScreen.width-2,
				pfScreen.largeBoldHeight+1);

		PFDrawStatePush();

		if (CycleCounter & 1) {

			fore = GUIGetSystemColour(GUIObjectForeground);
			back = GUIGetSystemColour(GUIObjectFill);

		} else {

			fore = GUIGetSystemColour(GUIObjectFill);
			back = GUIGetSystemColour(GUIObjectForeground);

		}

		PFSetForeColour(fore);
		PFSetBackColour(back);

		PFEraseRectangle(&r, 4);
		PFPaintRectangleFrame(&r, roundFrame);

		PFSetTextColour(fore);
		FntSetFont(largeBoldFont);
		DrawAlignedChars(urgentAlarm, ALIGNCENTRE, pfScreen.xcentre,
				pfScreen.height - pfScreen.largeBoldHeight-2);

		PFDrawStatePop();

		//  TODO - add wakeup, somehow...

	} else {

		CheckAndDrawMessageFlag();

	}

}

/*
 * function : HandleCommonMenuEvent
 *
 * Handles the common menu events, returns true if the event was handled
 *
 */

static Boolean HandleCommonMenuEvent(EventPtr event) {

	UInt16 newForm = 0;
	Boolean handled = false;
	MessageDialogDataType *md;
	EventType e;
	FlightPlanStackType tmpStack;

	switch (PFEventGetMenuID(event)) {

	case MnExit:
		PFExitApp();
		handled = true;
		break;

	case MnCommandPopup:

		/*
		 * check for alarm condition first, if an alarm is present
		 * then show the alarm dialog in preference to the command popup
		 *
		 */

		if (AlarmGetMaxLevel() > alarmOff) {

			AlarmShow();

		} else {

			CommandDialogInit();
			GUIFormPopup(CommandDialog);

		}
		handled = true;
		break;

	case MnGotoHSI:
		newForm = MainForm;
		handled = true;
		break;

	case MnGotoGPS:
		newForm = GPSForm;
		handled = true;
		break;

	case MnGotoPlan:
		newForm = FlightPlanForm;
		handled = true;
		break;

	case MnGotoTimer:
		newForm = TimerForm;
		handled = true;
		break;

	case MnGotoPrx:
		newForm = DiversionForm;
		handled = true;
		break;

	case MnGotoMap:
		newForm = MapForm;
		handled = true;
		break;

	case MnFlight:
		GUIFormPopup(SelectCopilotFlightDialog);
		handled = true;
		break;

	case MnFlightFlipFlop:
		OBSClear();

		FpStackPush(FlightPlanStack, FlightPlan);
		FpFree(FlightPlan);

		tmpStack = FlightPlanStack;
		FlightPlanStack = FlightPlanStackB;
		FlightPlanStackB = tmpStack;

		FlightPlan = FpStackPop(FlightPlanStack);
		ModErrThrowIf(!FlightPlan)
		;
		NavUpdatePosition(&GPS.posn);
		handled = false;
		break;

#ifdef XXXX
		case MnFlightSave:
		if (StrLen(FlightPlanName)) {

			Boolean savedOk = FlightDBSaveFlight(FlightPlanName, FlightPlan, FpStackPeek(FlightPlanStackB, 0), false);

			if (!savedOk && GUIAlertShow(FlightExistsAlert) == 0) {

				savedOk = FlightDBSaveFlight(FlightPlanName, FlightPlan, FpStackPeek(FlightPlanStackB, 0), true);

				if (!savedOk) ErrThrow(1967);
				handled = true;

			}

		} else {

			GUIFormPopup(SaveFlightDialog);
			handled = true;

		}
		break;
#endif

	case MnFlightSave:
		GUIFormPopup(SaveFlightDialog);
		handled = true;
		break;

	case MnPreferences:
		GUIFormPopup(PreferencesDialog);

		/*
		 * allow active form to detect this menu event and react to it
		 *
		 */

		handled = false;
		break;

	case MnSimulation:
		GUIFormPopup(SimulationDialog);
		handled = true;
		break;

	case MnAbout:
		GUIAlertShow(AboutBox);
		handled = true;
		break;

	case MnOBS:
		if (!FpIsBlank(FlightPlan)) {

			OBSClear();
			KeypadDialogInit(OBSCallback, OBSGetBearing(), 0, OBSInputPrompt,
					OBSLabelStrings);
			GUIFormPopup(KeypadDialog);

		}

		handled = true;
		break;

	case MnNextLeg:
	case MnPreviousLeg:

		if (!FpIsBlank(FlightPlan)) {

			handled = PFEventGetMenuID(event) == MnNextLeg ?
			FpNextLeg(FlightPlan) : FpPreviousLeg(FlightPlan);

			if (handled && (!GPSState || OBSActive())) {

				OBSClear();

			}

			NavUpdatePosition(&GPS.posn);

			/*
			 * allow active form to detect leg change
			 *
			 */

			handled = false;

		} else {

			handled = true;

		}

		break;

	case MnDivertEmergency:
		if (DvEmergencyDiversion())
			newForm = MapForm;
		handled = true;
		break;

	case MnUndoEdit:
		FpStackPopOverExisting(FlightPlanStack, &FlightPlan);
		OBSClear();

		/*
		 * allow active form to detect undo request
		 *
		 */

		handled = false;
		break;

	case MnShowAlarm:
		AlarmShow();
		handled = true;
		break;

	case MnVNAV:
		if (!FpIsBlank(FlightPlan)) {

			GUIFormPopup(VNAVDialog);

		}
		handled = true;
		break;

	case MnDayNight:
		Preferences.nightMode = !Preferences.nightMode;
		if (Preferences.nightMode) {

			ColoursSetNight(&AppColourPrefs);

		} else {

			ColoursSetDay(&AppColourPrefs);

		}
		if (Terrain)
			TerrainSetPalette(Terrain, terrainNormalWarn,
					FILEROOT APPNAME"-Palette.fma", Preferences.nightMode);

		handled = true;
		GUIFormGoto(GUIFormGetActiveID());
		break;

	case MnClearTrackLog:
		TLClear();
		handled = false;
		break;

	case MnObstacleMap:
		Preferences.mapNumber = 3;
		Preferences.mapSetting[3].icons |= wpObstacle;
		Preferences.mapSetting[3].scale = 3; // 10 nm
		newForm = MapForm;
		handled = true;
		break;

	case MnObstacleOff:
		Preferences.obstacleWarnings = false;
		handled = true;
		md = MessageDialogDataNew(ObstacleNearby, 2, ObstacleMapOption,
				MnObstacleMap, ObstacleOffOption, MnObstacleOff, 
				NULL, 0, NULL, 0);
		AlarmSetCondition(md, alarmOff);
		break;

	case MnTurnAnticipationOff:
		Preferences.turnAnticipation = false;
		handled = true;
		break;

	case MnMonitoring:
		GUIFormPopup(MonitorDialog);
		handled = true;
		break;

	}

	if (newForm && (!GPSState || GPSSimulating() || ValidRegistration))
		GUIFormGoto(newForm);

	return handled;

}

/*
 * function : OBSCallback
 *
 * Validate and accept the OBS that was entered on the keypad
 *
 */

static Boolean OBSCallback(UInt16 button, float value) {

	Boolean ok = false;

	LOGENTRY;

	if (button == KPUser2) {

		OBSClear();
		ok = true;

	} else if (button == KPUser1 && value >= 0.0 && value < 360) {

		const FlightPlanLegWaypointType *wp= FpGetCurrentWaypoint(FlightPlan);

		OBSSet(wp, value, Preferences.useMagnetic);
		ok = true;

	} else if (button == KPUser3) {

		return true;

	}

	if (ok) {

		PFSendSimpleEvent(evtCrsOverride);
	}

	LOGEXIT;

	return ok;

}

/*
 * function : AppHandleEvent
 *
 * Handles application-level events
 *
 * Returns true if the event is handled
 *
 */

static Boolean AppHandleEvent(EventPtr event) {

	UInt16 formId;

#ifdef KEYLOG
	if (PFEventGetType(event) == keyDownEvent) {

		char log[32];

		StrPrintF(log, "%d %d %d", PFEventGetKeyChr(event), event->data.keyDown.keyCode, event->data.keyDown.modifiers);

		PFDrawChars(log, StrLen(log),0,0);

	}
#endif

	if (PFEventGetType(event) == evtScreenRedrawn) {

		DrawAlarmBanner();

	}

	/*
	 * handle menu events
	 *
	 */

	if (HandleCommonMenuEvent(event))
		return true;

	/*
	 * Handle the form load events
	 * 
	 */

	if (PFEventGetType(event) == frmLoadEvent) {

		formId = PFEventGetFormLoadID(event);

		/*  
		 *  Set the event handler for the form.	The handler of the 
		 *  currently active form is called by FrmHandleEvent each 
		 *  time it receives an event. 
		 */

		switch (formId) {
		case MainForm:
			GUIFormLoad(formId, MainFormHandleEvent);
			MessageFlag.x = 41;
			MessageFlag.y = 0;
			Preferences.lastForm = formId;
			break;

		case GPSForm:
			GUIFormLoad(formId, GPSFormHandleEvent);
			MessageFlag.x = 28;
			MessageFlag.y = 0;
			Preferences.lastForm = formId;
			break;

		case FlightPlanForm:
			GUIFormLoad(formId, FlightPlanFormHandleEvent);
			MessageFlag.x = 33;
			MessageFlag.y = 0;
			Preferences.lastForm = formId;
			break;

		case DiversionForm:
			MessageFlag.x = 41;
			MessageFlag.y = 0;
			GUIFormLoad(formId, DiversionFormHandleEvent);
			Preferences.lastForm = formId;
			break;

		case MapForm:
			MessageFlag.x = 35;
			MessageFlag.y = 0;
			GUIFormLoad(formId, MapFormHandleEvent);
			Preferences.lastForm = formId;
			break;

		case TimerForm:
			MessageFlag.x = 41;
			MessageFlag.y = 0;
			GUIFormLoad(formId, TimerFormHandleEvent);
			Preferences.lastForm = formId;
			break;

		case CommandDialog:
			GUIFormLoad(formId, CommandDialogHandleEvent);
			break;

		case WPInfoDialog:
			GUIFormLoad(formId, WPInfoDialogHandleEvent);
			break;

		case TextDialog:
			GUIFormLoad(formId, TextDialogHandleEvent);
			break;

		case KeypadDialog:
			GUIFormLoad(formId, KeypadDialogHandleEvent);
			break;

		case MapDialog:
			GUIFormLoad(formId, MapDialogHandleEvent);
			break;

		case SimulationDialog:
			GUIFormLoad(formId, SimulationDialogHandleEvent);
			break;

		case AlphaPadDialog:
			GUIFormLoad(formId, AlphaPadDialogHandleEvent);
			break;

		case MessageDialog:
			GUIFormLoad(formId, MessageDialogHandleEvent);
			break;

		case VNAVDialog:
			GUIFormLoad(formId, VNAVDialogHandleEvent);
			break;

		case MonitorDialog:
			GUIFormLoad(formId, MonitorDialogHandleEvent);
			break;

		case SelectCopilotFlightDialog:
			GUIFormLoad(formId, FlightDialogHandleEvent);
			break;

		case SaveFlightDialog:
			GUIFormLoad(formId, SaveFlightDialogHandleEvent);
			break;

		case PreferencesDialog:
			GUIFormLoad(formId, PreferencesDialogHandleEvent);
			break;

		case MorePreferencesDialog:
			GUIFormLoad(formId, MorePreferencesDialogHandleEvent);
			break;

		default:
			ErrThrow(ERR_INVALID_FORM_LOAD_EVENT);
			break;

		}
		return true;
	} else
		return false;
}

/*
 *  function: UpdateTimers
 *
 */

static void UpdateTimers(void) {

	static UInt32 lastSeconds = 0;

	UInt32 nowSeconds = PFGetSeconds();
	UInt32 interval = nowSeconds - lastSeconds;
	Int16 j;

	/*
	 * timer control
	 *
	 */

	if (interval == 0)
		return;

	for (j = 0; j < NUMTIMERS; j++) {

		TimerType *timer = &Preferences.timer[j];
		if (timer->state == timerRunning || (timer->state == timerFlight
				&& GPS.posn.speed > 30)) {

			if (timer->countdown)

				timer->seconds -= interval;

			else

				timer->seconds += interval;

			if (timer->seconds > 86400*2)
				timer->seconds = 0;

			if (timer->seconds < 0) {

				/*
				 * countdown timer has expired - send an alert and reset
				 * if necessary
				 *
				 */

				char message[32];
				MessageDialogDataType *md;

				StrPrintF(message, TimerMessage, timer->label);
				md = MessageDialogDataNew(message, 1, TimerMessageButton,
						MnGotoTimer, 
						NULL, 0, NULL, 0, NULL, 0);

				AlarmSetCondition(md, alarmMessage);

				StrPrintF(message, FILEROOT"snd-%c.fma", j+49);
				PFPlaySound(FILEROOT"snd-timer.fma", Preferences.voiceVolume);
				PFPlaySound(message, Preferences.voiceVolume);
				PFPlaySound(FILEROOT"snd-expired.fma", Preferences.voiceVolume);

				if (timer->autoReset) {

					timer->seconds = timer->initialValue;

				} else {

					timer->state = timerStopped;
					timer->seconds = 0;

				}

			}
		}

	}

	lastSeconds = nowSeconds;

}

/*
 *  function: DoObstacleScan
 *
 */

static void DoObstacleScan(void) {

	WDMSearchHandle sh;
	Boolean foundObstacle = false;
	MessageDialogDataType *md;
	static Boolean nearObstacle = false; // held true for duration of being near obstacle(s)
	ShortWaypointType *swp;

	/*
	 * Obstacle scan
	 *
	 */

	if (!Preferences.obstacleWarnings)
		return;

	LOGTAG("Obs");
	LOGTIMESTART;

	sh = WDMInitProxScan(WPDataset, GPS.posn.lat32,
			GPS.posn.lon32, FMDS.obstacles, wpAnyObstacle);

	if (!sh) return;

	swp = WDMGetProxWaypoint(sh, RAD_TO_INT32(NM_TO_RAD(5.0)));

	while (swp != NULL && !foundObstacle) {

		Int32 obsAlt = swp->extra[1] * 256 + swp->extra[2];

		if ((Int32)GPS.posn.altitude - obsAlt < 1000) {

			foundObstacle = true;

		}

		swp = WDMGetProxWaypoint(sh, RAD_TO_INT32(NM_TO_RAD(5.0)));

	}

	md = MessageDialogDataNew(ObstacleNearby, 2,
			ObstacleMapOption, MnObstacleMap,
			ObstacleOffOption, MnObstacleOff,
			NULL, 0, NULL, 0);

	if (foundObstacle && !nearObstacle) {

		nearObstacle = true;
		PFPlaySound(FILEROOT"snd-warning.fma", Preferences.voiceVolume);
		PFPlaySound(FILEROOT"snd-obstacle.fma", Preferences.voiceVolume);

	} else if (!foundObstacle) {

		nearObstacle = false;

	}

	AlarmSetCondition(md, foundObstacle ? alarmWarning : alarmOff);

	WDMFreeSearch(sh);

	LOGTIMESTOP;

}

/*
 *  function: UpdateTrackLog
 *
 */

static void UpdateTrackLog(void) {

	static UInt32 lastTrackLogTime = 0;
	const UInt32 trackLogInterval[4] = { 0, 1, 5, 10 };

	if (VALIDGPSFIX && Preferences.trackLog> trackLogOff &&
	PFTimerHasExpired(lastTrackLogTime,
			trackLogInterval[Preferences.trackLog]*PFTicksPerSecond())) {

		TLAddPoint(&GPS.posn);

		lastTrackLogTime = PFGetTicks();

	}

}

/*
 * function : AppEventLoop
 *
 * Receives all events from the PalmOS and dispatches them to the various event
 * handlers, until one of them signals that it has processed the event.
 *
 */

static void AppEventLoop(void) {
	EventType myEvent;
	EventPtr event = &myEvent;
	static Boolean memSpy = false;
	static Boolean powerSaveMode = false; // Garmin feature

	do {

		if (GPSState) {

			/*
			 * GPS Module has its own event mechanism, it
			 * will release control when it has an event
			 * from PalmOS.
			 *
			 */

			GPSProcess(dutyNormal, event);
			if (!powerSaveMode)
				PFResetAutoOffTimer();

			UpdateTrackLog();

		} else {

			PFGetEvent(event, PFTicksPerSecond()/2);

		}

		/*
		 * handle key events which can be remapped to controls
		 * on the current form
		 *
		 */

		if (PFEventGetType(event) == keyDownEvent) {

			MapFunctionKeyToNewEvent(event);
			if (VALIDGPSFIX && PFEventGetKeyChr(event) == vchrShiftF2) {

				DvEmergencyDiversion();
				GUIFormGoto(MapForm);

			}

			if (PFEventGetKeyChr(event) == vchrPowerSave) {

				MessageDialogDataType *md;

				powerSaveMode = !powerSaveMode;

				md = MessageDialogDataNew(powerSaveMode ? PowerSaveOn
						: PowerSaveOff, 0, 
				NULL, 0, NULL, 0, NULL, 0, NULL, 0);

				AlarmSetCondition(md, alarmInfo);

				continue;

			}

		}

		/*
		 * if appStopEvent while the GPS is running then
		 * ask the user to confirm that the exit was
		 * intended before reacting to the event
		 *
		 */

		if (PFEventGetType(event) == appStopEvent) {

			if (!GPSState || GUIAlertShow(ExitAlert)==0)

				return;

			else

				PFGetEvent(event, 0);

		}

		UpdateTimers();

		if (PFEventGetType(event) == winDisplayChangedEvent) {

			LOGTAG("displayChanged");

			if (SatConst)
				SatConstFree(SatConst);
			if (HSIMiniPanel)
				HSIMiniPanelFree(HSIMiniPanel);

			SatConst = NULL;
			HSIMiniPanel = NULL;

			if (pfScreen.width > StandardPageWidth) {

				SatConst
						= SatConstNew(&GPS, StandardPageWidth+2, 0,
								pfScreen.width - (StandardPageWidth+2),
								pfScreen.height);
				HSIMiniPanel = NULL;

			} else if (pfScreen.height > StandardPageHeight) {

				SatConst = SatConstNew(&GPS, 0, (StandardPageHeight+2),
						pfScreen.width, pfScreen.height
								- (StandardPageHeight+2));
				HSIMiniPanel = HSIMiniPanelNew(0, StandardPageHeight,
						pfScreen.width, pfScreen.height-StandardPageHeight);

			}

		}

		/*
		 * Let the Navigation Manager handle the GPS position update
		 * first
		 *
		 */

		if (PFEventGetType(event) == evtGPSPositionUpdate && VALIDGPSFIX) {

			CycleCounter++;
			NavUpdatePosition(&GPS.posn);

			DoObstacleScan();

		}

		PFHandleEvent(event, &AppHandleEvent);

		if (0 && memSpy && PFEventGetType(event) == evtGPSPositionUpdate) {

			char s[40];
			UInt32 free, maxCont;

			MemHeapFreeBytes(0, &free, &maxCont);
			StrPrintF(s, "%ld %ld", free, maxCont);
			PFDrawChars(s, StrLen(s), 0,0);

		}

	} while (1);

}

/*
 * function : UseDefaultCopilotFlight
 *
 * Open the current copilot flight. 
 *
 */

static void UseDefaultCopilotFlight(void) {
	UInt16 fn;
	UInt16 j;
	UInt16 *validFlights= PFMalloc(220*sizeof(UInt16));
	UInt16 total;
	Boolean found=false;

	CpInitialise();

	fn = CpGetActiveFlightNum();
	CpGetValidFlights(220, &total, validFlights);

	/*
	 * look for default flight in list of valid flights
	 *
	 */

	for (j=0; j<total; j++) {

		if (validFlights[j] == fn) {

			if (CpImportFlight(fn, FlightPlan, 0, false)) {

				found=true;

				if (CpIsAlternateAvailable()) {

					FlightPlanType alt = FpNew();

					if (!CpImportFlight(fn, alt, 0, true)) {

						FpInit(alt);
						GUIAlertShow(FlightPlanAlert);

					}

					FpStackPush(FlightPlanStackB, alt);
					FpFree(alt);

				}

				break;
			}
		}
	}

	PFMemFree(validFlights);

	if (!found) {

		GUIAlertShow(FlightPlanAlert);
		FpInit(FlightPlan);

	}

	CpClose();

}

#ifdef UNITTEST
static void AppUnitTests(void) {

	UInt16 unitNumber;
	UInt16 numOfTests;
	HostFILE *f;

	/*
	 * unitNames and testHeads :
	 *
	 * Keep these two structures in sync !!!
	 *
	 */

	static const char * const unitNames[] = {
		StrCoPilotInterface,
		"Map Form"
	};

	static UInt16 (*headFunc[])(TestActionType, UInt16, HostFILE *) = {
		CpUnitTestHead,
		MapUnitTestHead
	};

	const UInt16 numUnits = 2;

	f = HostLogFile();

	for (unitNumber = 0;unitNumber < numUnits;unitNumber++) {
		numOfTests = headFunc[unitNumber](reportNumTests,0,f);
		if (numOfTests>0) {
			UInt16 j;

			HostFPrintF(f,"UT:Unit %s, %d tests", unitNames[unitNumber],numOfTests);
			HostFFlush(f);
			for (j=0;j<numOfTests;j++) {
				UInt16 result;

				result = headFunc[unitNumber](executeTest, j, f);
				if (result == 0) {
					HostFPrintF(f,"UT:Test %d passed", j+1);
				} else {
					HostFPrintF(f,"UT:Test %d failed code %d",j+1,result);
				}
				HostFFlush(f);
			}
			HostFPrintF(f,"UT:Unit %s finished", unitNames[unitNumber]);
			HostFPrintF(f,"UT:================");
			HostFFlush(f);
		} else {
			HostFPrintF(f,"UT:Unit %s, %d tests", unitNames[unitNumber],numOfTests);
			HostFFlush(f);
		}
	}
}
#endif

/* 
 * function : AppStart
 *
 * Called as FlightMaster powers up. Performs numerous functions, 
 * too many to list here ;-)
 *
 * Returns an error code if FlightMaster start up code fails.
 *
 */

static Err AppStart(Boolean launchedFromCoPilot) {
	Err error = 0;
	Int16 j;
	char *uid, *serialNumber;
	UInt32 free, max;
	Coord tmpx, tmpy;

	LOGENTRY;

	MemHeapFreeBytes(1, &free, &max);

	LOGINT32(free);
	LOGINT32(max);

	PFInit();

	TLOpen();

	/*
	 * check that mathlib is available, and load it if so
	 *
	 */

	error = SysLibFind(MathLibName, &MathLibRef);
	if (error)
		error = SysLibLoad(LibType, MathLibCreator, &MathLibRef);

	if (error) {
		GUIAlertShow(MathLibAlert);
		LOGEXIT;
		return ERR_MATHLIB_NOT_FOUND;
	} else {
		error = MathLibOpen(MathLibRef, MathLibVersion);
		if (error) {
			GUIAlertShow(MathLibAlert);
			LOGEXIT;
			return ERR_MATHLIB_NOT_FOUND;
		}
	}

	LOGLINE;

	FlightDBOpen();

	AsOpenDatabase();

	LOGLINE;

	if (!PrfReadPreferences(&Preferences)) {

		newInstallation = true;

	}

	GPSSetAltitudeCorrection((float)Preferences.gpsAltOffset);

	LOGLINE;

	/*
	 * Global terrain cache - Loaded here for palette reasons
	 *
	 */

	Terrain = TerrainNew(FILEROOT APPNAME"-Terrain.fma", 320,320);

	/*
	 * check for palette file, load into terrain if both are available
	 *
	 */

	if (Terrain)
		TerrainSetPalette(Terrain, terrainNormalWarn,
				FILEROOT APPNAME"-Palette.fma", Preferences.nightMode);

	/*
	 * setup the display units and initialise the heading indicator, load
	 * the icons. 
	 *
	 * The bitmaps must be loaded *before* the 2red compatibility code
	 * alters the palette otherwise PalmOS (too) cleverly renders the
	 * bitmap using the closest colours it can out of the modified "red"
	 * palette.
	 *
	 * NB This is why the icons *MUST* be rendered to a window first
	 *
	 */

	LOGLINE;

	IconWindows.aircraft = PFBitmapLoadToWindow(AircraftBitmaps, &tmpx, &tmpy);
	IconWindows.aircraftDim = tmpx / 8;

	IconWindows.icons = PFBitmapLoadToWindow(IconBitmaps, &tmpx, &tmpy);
	IconWindows.iconMasks = PFBitmapLoadToWindow(IconMaskBitmaps, &tmpx, &tmpy);
	IconWindows.iconDim = tmpx / 8;

	SmallIconWindows.icons = PFBitmapLoadToWindow(IconBitmapsLow, &tmpx, &tmpy);
	SmallIconWindows.iconMasks
			= PFBitmapLoadToWindow(IconBitmapsLowMask, &tmpx, &tmpy);
	SmallIconWindows.iconDim = (tmpx / 8)/2; // TODO - some weird effect requires /2


	LOGLINE;

	/*
	 * Initialise waypoint dataset, which can comprise up to 12 databases.
	 *
	 * 2 x CoPilot and 10 x Obstacles.
	 *
	 */

	WPDataset = WDMNew();

	FntSetFont(stdFont);

#ifndef AEROPALM

	DrawAlignedChars("   Loading CoPilot User Database", ALIGNLEFT, 0, 0);
	FMDS.cpUser = WDMAddDB(WPDataset, "CoPilot User Waypoint", NULL, NULL,
			IndexUpdateProgess);

	DrawAlignedChars("   Loading CoPilot System Database", ALIGNLEFT, 0,
			FntLineHeight());

	FMDS.cpSystem = WDMAddDB(WPDataset, "CoPilot Waypoint",
			"/Palm/Programs/CoPilot/CoPilot_Waypoint.pdb", NULL,
			IndexUpdateProgess);

#else

	DrawAlignedChars("   Loading AeroPalm User Database",ALIGNLEFT,0,0);
	FMDS.cpUser = WDMAddDB(WPDataset, "AeroPalm User Waypoint", NULL, NULL, IndexUpdateProgess);

	DrawAlignedChars("   Loading AeroPalm System Database",ALIGNLEFT,0,FntLineHeight());

	FMDS.cpSystem = WDMAddDB(WPDataset, "AeroPalm Waypoint", "/Palm/Programs/AeroPalm/AeroPalm_Waypoint.pdb", NULL, IndexUpdateProgess);

#endif

	PFResetAutoOffTimer();

	FMDS.obstacles = FMDS.cpUser; // allow user to add obstacles to his DB
	FMDS.all = FMDS.cpUser | FMDS.cpSystem;
	for (j=0; j<10; j++) {

		const char
				*obsInternal[10] = { APPNAME"-Obstacle1", APPNAME"-Obstacle2",
						APPNAME"-Obstacle3", APPNAME"-Obstacle4",
						APPNAME"-Obstacle5", APPNAME"-Obstacle6",
						APPNAME"-Obstacle7", APPNAME"-Obstacle8",
						APPNAME"-Obstacle9", APPNAME"-Obstacle10" };

		/*
		 const char *obsInternal[10] = {
		 "Update Waypoin0",
		 "Update Waypoin1", "Update Waypoin2", "Update Waypoin3",
		 "Update Waypoin4", "Update Waypoin5", "Update Waypoin6",
		 "Update Waypoin7", "Update Waypoin8", "Update Waypoin9"};
		 
		 */
		const char *obsCompact[10] = { FILEROOT APPNAME"-Obstacle1.fma",
FILEROOT 				APPNAME"-Obstacle2.fma", FILEROOT APPNAME"-Obstacle3.fma",
FILEROOT 				APPNAME"-Obstacle4.fma", FILEROOT APPNAME"-Obstacle5.fma",
FILEROOT 				APPNAME"-Obstacle6.fma", FILEROOT APPNAME"-Obstacle7.fma",
FILEROOT 				APPNAME"-Obstacle8.fma", FILEROOT APPNAME"-Obstacle9.fma",
FILEROOT 				APPNAME"-Obstacle10.fma" };

		char msg[64];

		StrPrintF(msg, "   Checking Obstacle DB %d...", j+1);
		DrawAlignedChars(msg, ALIGNLEFT, 0, (j+2)*FntLineHeight());
		FMDS.obs[j] = WDMAddDB(WPDataset, obsInternal[j], NULL, obsCompact[j],
				IndexUpdateProgess);
		if (FMDS.obs[j] != wdmNotFound) {

			StrPrintF(msg, StrLoaded);
			DrawAlignedChars(msg, ALIGNRIGHT, pfScreen.width, (j+2)
					*FntLineHeight());

		}

		FMDS.obstacles |= FMDS.obs[j];
		FMDS.all |= FMDS.obs[j];

	}

	PFResetAutoOffTimer();
	PFEraseWindow();

	FMDS.noObstacles = FMDS.cpUser | FMDS.cpSystem;

	WDMLoadDatabase(WPDataset, wdmTempDB, APPNAME"-TempWaypoints.fma");

	LOGLINE;

	if (Preferences.units == CP_UNITS && !CpIsInstalled())
		Preferences.units = NM_UNITS;

	SetDisplayUnits(Preferences.units);

	/*
	 * restore flight plan stack & status
	 *
	 * Priority:
	 *
	 * 1. Use CoPilot Plan if launched by CoPilot
	 * 2. Try to restore state
	 * 3. Display flight selector
	 *
	 */

	FlightPlanStack = FpStackNew();
	FlightPlanStackB = FpStackNew();

	if (!launchedFromCoPilot && FpStackLoad(FlightPlanStack,
			APPNAME"-FlightStack.fma") && FpStackLoad(FlightPlanStackB,
			APPNAME"-FlightStackB.fma")) {

		FlightPlan = FpStackPop(FlightPlanStack);

	} else {

		FlightPlan = FpNew();

		if (launchedFromCoPilot) {

			Preferences.units = CP_UNITS;
			UseDefaultCopilotFlight();

		} else {

			/*
			 * Flight selector woz ere
			 *
			 */

			FpInit(FlightPlan);

		}

		if (!FpStackPeek(FlightPlanStackB, 0)) {

			FlightPlanType newfp = FpNew();

			FpStackPush(FlightPlanStackB, newfp);
			FpFree(newfp);

		}

	}

	/*
	 * startup the GPS if the preference to do so is selected
	 *
	 */

	LOGLINE;
	if (HostGremlinIsRunning()) {

		Preferences.lastForm = GPSForm;

	} else {

		if (Preferences.autoStartGPS && Preferences.gpsSource != gpsSimulate) {

			GPSState = true;
			error = GPSInit(Preferences.gpsSource, &Preferences.bluetoothID);
			if (error) {

				char tmp[9];

				StrPrintF(tmp, "%0X", error);
				GUICustomAlertShow(SerialPortAlert,tmp,NULL,NULL);
				GPSState = false;

			}

		} else {

			GPSState = false;

		}

	}

#ifdef UNITTEST
	AppUnitTests();
#endif

	LOGLINE;

	/*
	 * Map initial position
	 *
	 */

	GPSSetLocation(Preferences.lastMapLat, Preferences.lastMapLon);

	if (Preferences.nightMode)

		ColoursSetNight(&AppColourPrefs);

	else

		ColoursSetDay(&AppColourPrefs);
	LOGEXIT;

	uid = PFGetUserID();
	serialNumber = PFGetSerialNumber();

	if (IsValidCode(Preferences.registrationCode32, uid)
	|| IsValidCode(Preferences.registrationCode32, serialNumber)) {

		ValidRegistration = 129;

	} else if (GetDemoDate(Preferences.registrationCode32, uid, REGCODESEED, DEMOCODEPRIME, YEAREPOCH)) {

		/*
		 * demonstration code - check dates
		 *
		 */

		if (HandleDemoChecks(Preferences.registrationCode32, uid,
				&DemoDateLimit, REGCODESEED, DEMOCODEPRIME, YEAREPOCH)) {

			ValidRegistration = 129;

		}

		/*
		 *  always go to the GPS form when not fully registered
		 *
		 */

		Preferences.lastForm = GPSForm;

	} else {

		GUIAlertShow(RegistrationNagAlert);
		Preferences.lastForm = GPSForm;

	}

	PFMemFree(serialNumber);
	PFMemFree(uid);

	GUIFormGoto(Preferences.lastForm);

	if (newInstallation) {

		GUIFormPopup(PreferencesDialog);
		newInstallation = false;

	}

	if (!disclaimerShown) {

		void *info;
		disclaimerShown = true;

		info = AsGetDatabaseInfo();

		if (info) {

			TextDialogSet(info);
			PFMemFree(info);
			GUIFormPopup(TextDialog);

		}

	}

	return errNone;

}

/******************************************************************************/

/*
 * function : AppStop
 *
 * Called as FM receives an appStop event. 
 *
 */

static void AppStop(void) {

	Err error;
	UInt16 useCount;

	LOGLINE;

	if (Terrain)
		TerrainFree(Terrain);

	if (SatConst)
		SatConstFree(SatConst);
	if (HSIMiniPanel)
		HSIMiniPanelFree(HSIMiniPanel);

	FlightDBClose();

	TLClose();

	ColoursSetDay(&AppColourPrefs);

	AlarmPurgeAll();

	/*
	 * save flight state
	 *
	 */

	LOGLINE;

	FpStackPush(FlightPlanStack, FlightPlan);

	LOGLINE;
	if (WDMSaveDatabase(WPDataset, wdmTempDB, APPNAME"-TempWaypoints.fma")) {

		LOGLINE;
		FpStackSave(FlightPlanStack, APPNAME"-FlightStack.fma");
		FpStackSave(FlightPlanStackB, APPNAME"-FlightStackB.fma");

	}

	FpStackFree(FlightPlanStack);
	FpStackFree(FlightPlanStackB);
	OBSClear();
	FpFree(FlightPlan);

	LOGLINE;

	GPSClose();

	LOGLINE;

	WDMFree(WPDataset);

	PrfSavePreferences(&Preferences);

	LOGLINE;

	/*
	 * Dispose of the icon windows
	 *
	 */

	PFWindowDelete(IconWindows.aircraft,false);
	PFWindowDelete(IconWindows.icons,false);
	PFWindowDelete(IconWindows.iconMasks,false);

	/*
	 * unload MathLib
	 *
	 */

	LOGLINE;

	error = MathLibClose(MathLibRef, &useCount);
	if (useCount == 0)
		SysLibRemove(MathLibRef);

	LOGLINE;

	AsCloseDatabase();

	PFDeinit();

}

/*
 * MapFunctionKeyToNewEvent
 *
 * Traps a function key and maps it to an associated menu or control event
 * according to the current form.
 *
 * The mappings are contained at the top of the file (Ref: keymappings)
 *
 * Returns true if the function key was mapped
 *
 */

static const struct {

	UInt16 formID;
	const FunctionKeymapType *keymap;

} formToFunctionKeymap[] = {

{ CommandDialog, commandPopupMapping }, { MapForm, mapFormMapping }, {
		MainForm, mainFormMapping }, { FlightPlanForm, flightPlanFormMapping },
		{ TimerForm, timerFormMapping }, { GPSForm, GPSFormMapping }, {
				DiversionForm, searchFormMapping }, { VNAVDialog,
				vnavDialogMapping }, { MonitorDialog, monitorDialogMapping }, {
				SelectCopilotFlightDialog, loadFlightDialogMapping }, {
				SaveFlightDialog, saveFlightDialogMapping }, { MapDialog,
				mapDialogMapping }, { MessageDialog, messageDialogMapping }, {
				AlphaPadDialog, alphaDialogMapping }, { KeypadDialog,
				keypadDialogMapping }, { PreferencesDialog,
				preferencesDialogMapping }, { MorePreferencesDialog,
				morePrefsDialogMapping }, { SimulationDialog,
				simulationDialogMapping },
		{ WPInfoDialog, wpInfoDialogMapping },
		{ TextDialog, textDialogMapping }, { 0, 0 }

};

static void MapFunctionKeyToNewEvent(EventPtr event) {

	Int16 j;
	WChar fkey= PFEventGetKeyChr(event);
	UInt16 formID = GUIFormGetActiveID();
	const FunctionKeymapType *keymap= NULL;
	Boolean isApplicationKey;

	isApplicationKey = (PFEventGetKeyChr(event) >= vchrF1 && PFEventGetKeyChr(event) <= vchrF4);

	if (GUIMenuIsDisplayed()) {

		/*
		 * allow default navigation key behaviour if a menu is being displayed
		 *
		 */

		if (!isApplicationKey)
			return;

	}

	for (j=0; formToFunctionKeymap[j].formID; j++) {

		if (formID == formToFunctionKeymap[j].formID) {

			keymap = formToFunctionKeymap[j].keymap;
			break;

		}

	}

	if (!keymap)
		return;

	/*
	 * try to find the function key in the mapping, if we can then
	 * substitute the equivalent event (be it menu or control event)
	 *
	 */

	for (j=0; keymap[j].keyID; j++) {

		if (keymap[j].keyID == fkey) {

			if (keymap[j].mapTo != noMapping) {

				switch (keymap[j].mapTo) {

				case mapToMenuID:
					PFEventSetType(event,menuEvent);
					PFEventSetMenuID(event, keymap[j].itemID);
					break;

				case mapToControlID:
					PFEventSetType(event, ctlSelectEvent);
					PFEventSetControlSelectID(event, keymap[j].itemID);
					break;

				default:
					ModErrThrowIf(1)
					;
					break;

				}

			} else {

				PFEventSetType(event, nilEvent);

			}

			return;

		}

	}

	return;

}

/*
 * FlightMaster
 *
 */

Err FlightMaster(Boolean launchedFromCoPilot) {

	Err err = AppStart(launchedFromCoPilot);

	if (err)
		return err;

	AppEventLoop();

	AppStop();

	return 0;

}

