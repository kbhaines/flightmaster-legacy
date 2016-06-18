/*
 * MapDialog.c
 *
 * Handles the map command popup for configuring the
 * map page
 *
 */

#include "Platform.h"
#include "MapDialog.h"
#include "Utils.h"
#include "ResourceDefines.h"
#include "GlobalTypes.h"

#include "PalmNavigator.h"
#include "AsDatabase.h"
#include "TerrainType.h"
#include "KeypadDialog.h"
#include "FMStrings.h"

#include "FMPreferences.h"

/*******************************************************************************
 *
 * global variables
 *
 */

extern FMPreferencesType Preferences;
extern AppColourPrefsType AppColourPrefs;
extern UserConversionType UC;

/*******************************************************************************
 *
 * module variables
 *
 */

#define NUM_FILTER_BUTTONS 17
#define FIRST_AIRSPACE_BUTTON 7

/*
 * This array maps from a button number (0 -> NUM_FILTER_BUTTONS) to the
 * corresponding waypoint or airspace mask to set/clear in the map preferences
 * structure.
 *
 * These need to follow the order of definition as in ResourceDefines.h, from
 * McAfButton to McLowerAirwayButton
 *
 */

static const UInt16 filterButtonMap[NUM_FILTER_BUTTONS] = {

	wpAirfield,
   	wpLargeAirfield, 
	vorFilter,
	ndbFilter,
	wpOther, 
	wpIntersection,
	wpAnyObstacle,
	
	asTypeClassA,
	asTypeClassB,
	asTypeClassC,
	asTypeClassD,
	asTypeClassE,
	asTypeClassF,
	asTypeClassG,
	asTypeSUAS | asTypeOther,

	asTypeHighAirway,
	asTypeLowAirway

};

/*
 * pointers into the map preferences, corresponding to each button on the form.
 * 0-6 will point to the icon settings, 7-16 will point to the airspace filter
 * settings etc.
 *
 */

static UInt16 *filters[NUM_FILTER_BUTTONS];
static UInt16 *labels[NUM_FILTER_BUTTONS];

/*
 * when some controls are pressed it that means that the map should be rebuilt
 *
 */

static Boolean sendUpdate;

/*
 * dialog has two stages of display, first the buttons, then the checkboxes
 *
 */

static Int16 dialogStage;


/*******************************************************************************
 *
 * private functions
 *
 */

static void SetTerrainRef(void) MODALDIALOGS_SECTION;
static void MapDialogInit() MODALDIALOGS_SECTION;
static Boolean AltitudeCallback(UInt16 button, float value) MODALDIALOGS_SECTION;
static void DrawFilterOverlay(Int16 buttonNumber) MODALDIALOGS_SECTION;
static void DrawFilterOverlays(void) MODALDIALOGS_SECTION;
static void HandleFilter(Int16 buttonNumber) MODALDIALOGS_SECTION;
static void DrawAirspaceFilters(void) MODALDIALOGS_SECTION;
static void HandleAirspaceFilter(Int16 buttonNumber) MODALDIALOGS_SECTION;
static void MapDialogSaveSettings(void) MODALDIALOGS_SECTION;
static void SetMapDialogStage() MODALDIALOGS_SECTION;

/*
 * function : SetTerrainRef
 *
 * Sets up the terrain reference selector trigger
 *
 */

static void SetTerrainRef(void) {

	char newString[16];

	/*
	 * selector trigger
	 *
	 */

	if (mapPrefs.terrainRefAlt > 0) {

		StrPrintF(newString, "%s %s",
				FloatToStr(mapPrefs.terrainRefAlt*UC.altitudeConv,0),
				UC.altitudeUnits);

	} else {

		StrCopy(newString, StrGPS);

	}

	GUIObjectSetLabel(McTerrainLevelSelector, newString);

}

/*
 * function : MapDialogInit
 *
 */

static void MapDialogInit() {

	Int16 j;

	/*
	 * set up the filter pointers
	 *
	 */

	for (j=0;j<FIRST_AIRSPACE_BUTTON;j++) {

		filters[j] = (UInt16*)&mapPrefs.icons;
		labels[j] = (UInt16*)&mapPrefs.labels;

	}

	for (j=FIRST_AIRSPACE_BUTTON;j < NUM_FILTER_BUTTONS;j++) {

		filters[j] = &mapPrefs.airspace;
		labels[j]  = &mapPrefs.airspaceLabels;

	}
	
	SetTerrainRef();

	/*
	 * control boxes
	 *
	 */

	GUIObjectSetValue(McTracklineCheckbox, mapPrefs.showTrack);
	GUIObjectSetValue(McATZCheckbox, mapPrefs.showZones);
	GUIObjectSetValue(McTrackupCheckbox, mapPrefs.trackUp);
	GUIObjectSetValue(McHeadingArcCheckbox, mapPrefs.showHeadingArc);
	GUIObjectSetValue(McRouteLineCheckbox, mapPrefs.route);
	GUIObjectSetValue(McTrackLogCheckbox, mapPrefs.trackLog);

	sendUpdate = false;

}

/*
 * AltitudeCallback
 *
 * Called by the Keypad callback
 *
 */

static Boolean AltitudeCallback(UInt16 button, float value) {

	if (button == KPUser3) return true;  // cancelled

	if (value < 1 || value > 30000) return false;

	switch (button) {
	
	case KPUser1:	// AMSL
		mapPrefs.terrainRefAlt = value / UC.altitudeConv;
		break;

	case KPUser2:	// Reset
		mapPrefs.terrainRefAlt = 0.0;
		break;

	}

	sendUpdate = true;

	return true;

}

/*
 * function : DrawFilterOverlay
 *
 * Overlays either the 'off' or 'label' bitmaps across the
 * specified filter button on the Map dialog.
 *
 * Buttons are numbered 0 - 9
 *
 */

static void DrawFilterOverlay(Int16 buttonNumber) {

	PFScreenRectType r;
	Int16 buttonGraphic = McOffBmp;

	if (*labels[buttonNumber] & filterButtonMap[buttonNumber]) 

		buttonGraphic = McLabelBmp;

	else if (*filters[buttonNumber] & filterButtonMap[buttonNumber])

		buttonGraphic = 0;

	if (buttonGraphic) {

		GUIObjectGetBounds(buttonNumber + McAfButton, &r);
		GUIBitmapDraw(buttonGraphic, r.x1, r.y1);

	}

}

/*
 * function : DrawFilterOverlays
 *
 * Draws all the filter overlays
 *
 */

static void DrawFilterOverlays(void) {

	Int16 j;

	PFDrawStatePush();
	for (j=0; j <= McLowerAirwayButton - McAfButton; j++) {

		DrawFilterOverlay(j);

	}

	PFDrawStatePop();
	
}

/*
 * function : HandleFilter
 *
 * Called in response to a tap on one of the filter buttons on the Map dialog. 
 *
 * Updates the specified button with a new graphic setting, and updates the
 * preferences via pointers in filters and labels.
 *
 * Input is the button number, 0 to NUM_FILTER_BUTTONS
 * 
 */

static void HandleFilter(Int16 buttonNumber) {

	if (*labels[buttonNumber] & filterButtonMap[buttonNumber]) {

		/*
		 * label & icon/airspace are on, turn off
		 *
		 */

		*labels[buttonNumber] ^= filterButtonMap[buttonNumber];
		*filters[buttonNumber] ^= filterButtonMap[buttonNumber];

	} else if (*filters[buttonNumber] & filterButtonMap[buttonNumber]) {

		/*
		 * icon/airspace on, turn labels on
		 *
		 */

		*labels[buttonNumber] ^= filterButtonMap[buttonNumber];

	} else {

		/*
		 * icon/airspace & labels off, turn on
		 *
		 */

		*filters[buttonNumber] ^= filterButtonMap[buttonNumber];

	}

	PFDrawStatePush();
	DrawFilterOverlay(buttonNumber);
	PFDrawStatePop();

}

/*
 * function : DrawAirspaceFilters
 *
 * Draws the current value of the airspace filters over the
 * top of the button bitmaps
 * 
 * Also draws terrain setting on the terrain button
 *
 */

static void DrawAirspaceFilters() {

	Int16 j;
	Int16 bitmap;
	PFScreenRectType r;

	PFDrawStatePush();

	PFSetDrawMode(blockMode);
	FntSetFont(boldFont);
	for (j=McUpperAltButton;j<=McLowerAltButton;j++) {

		char c[2] = " ";

		GUIObjectGetBounds(j, &r);

		if (j==McUpperAltButton && mapPrefs.upperFilter) c[0] = mapPrefs.upperFilter + '0';
		if (j==McLowerAltButton && mapPrefs.lowerFilter) c[0] = mapPrefs.lowerFilter + '0';

		if (c[0] != 32) PFDrawOutlineChars(c, ALIGNLEFT, r.x1+26, r.y1+16);

	}

	/*
	 * draw terrain setting
	 *
	 */

	if (mapPrefs.terrain > terrainOff) {

		bitmap = mapPrefs.terrain + McTerrainNormalBmp - 1;

	} else {

		bitmap = McTerrainNormalBmp;

	}

	GUIObjectSetBitmap(McTerrainButton, bitmap);

	if (mapPrefs.terrain == terrainOff) {

		PFSetTextColour(AppColourPrefs.black);
		GUIObjectGetBounds(McTerrainButton, &r);
		PFDrawOutlineChars("OFF", ALIGNCENTRE, (r.x1 + r.x2)/2, r.y1+14);

	}

	PFDrawStatePop();

}
	
/*
 * function : HandleAirspaceFilter
 *
 */

static void HandleAirspaceFilter(Int16 buttonNumber) {

	UInt8 *filter;

	filter = buttonNumber == 0 ? &mapPrefs.upperFilter:&mapPrefs.lowerFilter;

	(*filter)++;

	if (*filter>3) *filter = 0;

	DrawAirspaceFilters();
	
}

/*
 * function : MapDialogSaveSettings
 *
 */

static void MapDialogSaveSettings(void) {

	Boolean trackUp = GUIObjectGetValue(McTrackupCheckbox); 
	Boolean route = GUIObjectGetValue(McRouteLineCheckbox); 
	Boolean trackLog = GUIObjectGetValue(McTrackLogCheckbox); 
		
	if (trackUp != mapPrefs.trackUp ||
			route != mapPrefs.route ||
			trackLog != mapPrefs.trackLog) {

		sendUpdate = true;

	}

	mapPrefs.trackUp = trackUp;

	mapPrefs.showTrack = GUIObjectGetValue(McTracklineCheckbox);
	mapPrefs.showZones = GUIObjectGetValue(McATZCheckbox);
	mapPrefs.showHeadingArc = GUIObjectGetValue(McHeadingArcCheckbox);
	mapPrefs.route = GUIObjectGetValue(McRouteLineCheckbox);
	mapPrefs.trackLog = GUIObjectGetValue(McTrackLogCheckbox);

}

/*
 * function : SetMapDialogStage
 *
 */

void SetMapDialogStage(void) {

	Int16 hideStart, hideEnd;
	Int16 showStart, showEnd;
	Int16 j;

	if (dialogStage == 0) {

		showStart = McAfButton;
		showEnd   = McTerrainButton;
		hideStart = McTerrainLevelLabel;
		hideEnd = McTrackLogCheckbox;


	} else {

		hideStart = McAfButton;
		hideEnd   = McTerrainButton;
		showStart = McTerrainLevelLabel;
		showEnd = McTrackLogCheckbox;

	}

	for (j = hideStart; j <= hideEnd; j++) {

		GUIObjectHide(j);

	} 
	
	for (j = showStart; j <= showEnd; j++) {

		GUIObjectShow(j);

	} 

}

/*******************************************************************************
 *
 * public functions
 *
 */


/*
 * function : MapDialogHandleEvent
 *
 */

Boolean MapDialogHandleEvent(EventPtr event) {

	Boolean handled = false;

	switch (PFEventGetType(event)) 
	{
	case frmOpenEvent:
		dialogStage = 0;
		MapDialogInit();
		SetMapDialogStage();
		GUIFormResize(true,true);
		GUIFormDraw();
		DrawAirspaceFilters();
		DrawFilterOverlays();
		handled = true;
		break;

	case winDisplayChangedEvent:
		GUIFormResize(true, true);
		SetMapDialogStage();
		GUIFormDraw();
		if (dialogStage == 0) {
		
			DrawAirspaceFilters();
			DrawFilterOverlays();
			
		}
		handled = true;
		break;

	case evtKeypadInput:
		SetTerrainRef();
		handled = true;
		break;
	
	case ctlSelectEvent:
		if (PFEventGetControlSelectID(event) >= McAfButton && PFEventGetControlSelectID(event) <= McLowerAirwayButton) {

			HandleFilter(PFEventGetControlSelectID(event) - McAfButton);
			handled = true;
			sendUpdate = true;

		} else if (PFEventGetControlSelectID(event) >= McUpperAltButton && PFEventGetControlSelectID(event) <= McLowerAltButton) {

			HandleAirspaceFilter(PFEventGetControlSelectID(event) - McUpperAltButton);
			handled = true;

		} else if (PFEventGetControlSelectID(event) == McTerrainButton) {

			if (mapPrefs.terrain == terrainWarn) 
				
				mapPrefs.terrain = terrainOff;

			else

				mapPrefs.terrain++;

			DrawAirspaceFilters();

			sendUpdate = true;
			handled = true;

		} else if (PFEventGetControlSelectID(event) == McTerrainLevelSelector) {

			KeypadDialogInit(AltitudeCallback, mapPrefs.terrainRefAlt*UC.altitudeConv,0,
					TerrainRefPrompt, TerrainRefButtons);
			GUIFormPopup(KeypadDialog);
			handled = true;

		}

		break;

	case menuEvent:

		if (PFEventGetMenuID(event) == MnMapConfig) {

			if (dialogStage == 0) {
				
				/*
				 * don't quit, show second stage of dialog
				 *
				 */

				dialogStage++;
				SetMapDialogStage();

			} else {
				
				MapDialogSaveSettings();

				GUIFormReturn();

				if (sendUpdate){ 

					PFSendSimpleEvent(evtMapConfigUpdate);

				}

			}

			handled = true;

		}  

		break;

	default:
		break;
	}
			
	return handled;
}
