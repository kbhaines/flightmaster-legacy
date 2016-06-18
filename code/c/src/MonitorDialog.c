/*
 * MonitorDialog.c
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

#include "MonitorDialog.h"

#include "Utils.h"


#include "TrackLog.h"

#include "FMPreferences.h"

/*******************************************************************************
 * 
 * global variables
 *
 * 
 */

extern FMPreferencesType Preferences;

/*******************************************************************************
 * 
 * module variables
 *
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
static void FormSaveValues(void) MODALDIALOGS_SECTION;

/*
 * FormInit
 *
 */

static void FormInit(void) {

	GUIObjectSetValue(MdTurnAnticipation, Preferences.turnAnticipation ? 1 : 0);
	GUIObjectSetValue(MdObstacleWarnings, Preferences.obstacleWarnings ? 1 : 0);
	GUIObjectSetValue(MdVoiceVolume, Preferences.voiceVolume);
	GUIObjectGroupSetValue(MdTrackLogGroup, (UInt16)Preferences.trackLog+MdTrackLogOff);
	
}

/*
 * FormSaveValues
 *
 */

static void FormSaveValues(void) {

	TrackLogSettingType newTl = (TrackLogSettingType) (GUIObjectGroupGetValue(MdTrackLogGroup) - MdTrackLogOff);
	Preferences.turnAnticipation = GUIObjectGetValue(MdTurnAnticipation);
	Preferences.obstacleWarnings = GUIObjectGetValue(MdObstacleWarnings);
	Preferences.voiceVolume = GUIObjectGetValue(MdVoiceVolume);

	if (newTl == trackLogOff && Preferences.trackLog > trackLogOff) {

		TLWriteBreak();

	}

	Preferences.trackLog = newTl;

}

/*******************************************************************************
 *
 * public functions
 *
 */

/*
 * function : MonitorDialogHandleEvent
 *
 */
Boolean MonitorDialogHandleEvent(EventPtr event) {

	Boolean       handled        = false;

	FntSetFont(largeBoldFont);

	switch (PFEventGetType(event)) {

	case frmOpenEvent:
		GUIFormResize(true, true);
		FormInit();
		GUIFormDraw();		handled = true;
		break;

	case winDisplayChangedEvent:
		GUIFormResize(true, true);
		GUIFormDraw();
		handled = true;
		break;

	case ctlSelectEvent:

		switch (PFEventGetControlSelectID(event)) {

		case MdVoiceVolume:
			PFPlaySound(FILEROOT"snd-warning.fma", GUIObjectGetValue(MdVoiceVolume));
			handled = true;
			break;

		case MdOKButton:
			FormSaveValues();
			GUIFormReturn();
			handled = true;
			break;

		case MdCancelButton:
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
