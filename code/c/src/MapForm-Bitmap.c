/******************************************************************************
 *
 * MapForm.c
 *
 * (c) Blackhawk Systems Ltd 2003.
 *
 */

/*
 * uncomment this line to add code that stress-tests the mapping code, and
 * provides diagnostic information about the performance.
 *
 * You will also need to build the code with LOG=Main, to get the logging
 * functionality turned on.
 *
 * This can be used as a profiling tool, as well as a testing system, and also
 * for tuning the memory usage.
 *
 * Enter 'r' on the map page to start the tests
 *
 */

//#define RUNMAPTESTS

#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif
#include "Platform.h"
#include "MapForm.h"
#include "ResourceDefines.h"
#include "Utils.h"
#include "GlobalTypes.h"
#include "CpInterface.h"
#include "DiversionMgr.h"
#include "MathLib.h"
#include "AvCalcs.h"
#include "WPInfoDialog.h"
#include "TextDialog.h"
#include "Modules.h"
#include "CollapseUtils.h"
#include "Gps.h"
#include "AsDatabase.h"
#include "PalmNavigator.h"
#include "FlightPlan.h"

#include "MapAirspace.h"
#include "MapManager.h"

#include "WDManager.h"
#include "NavManager.h"
#include "Instruments.h"
#include "FMStrings.h"
#include "OBSManager.h"
#include "AlarmManager.h"
#include "TapPlan.h"
#include "PanManager.h"

#define ModuleID MapFormModuleID

/**************************************************************************
 *
 * global variables
 *
 */

extern FMPreferencesType Preferences;
extern const GPSType GPS;
extern const Boolean GPSState;
extern const char *CompassText[];
extern const IconWindowsType IconWindows;

extern FlightPlanType FlightPlan;
extern FlightPlanStackType FlightPlanStack;
extern const AppColourPrefsType AppColourPrefs;

extern const ScreenInfoType pfScreen;
extern const UserConversionType UC;
extern const DisplayUnitsType DisplayUnits;
extern const WDMHandle WPDataset;
extern const Int32 CycleCounter;

/**************************************************************************
 *
 * module variables
 *
 */

#define TRACKUP_ORIGIN_Y (lscreen.height - (lscreen.standardHeight / 3))

static Boolean planningMode;

/*
 * local copy of Screen and icon information, that we can scale if high-density
 * is available
 *
 */

static ScreenInfoType lscreen;
static IconWindowsType liconWindows;

static PFScreenRectType mapNumberHotspot;

static Coord fddUpperY1;
static Coord fddUpperY2;
static Coord fddLowerLineY1;
static Coord fddLowerLineY2;
static Coord fddStatusLineY;

static NavDataType mapTimeField = navETE;

static PFScreenRectType timeFieldRect;

static PFScreenRectType zoomInRect, zoomOutRect;

/*
 * pointers to the display and new mapspaces. The display is drawn using the
 * displayMap pointer, whereas the newMap pointer is used to
 * construct the latest mapspace using up-to-date data. When completed, the
 * displayMapspace pointer is set to the newMap pointer and the old
 * mapspace can be discarded.
 *
 */

static MapType displayMap;
static MapType newMap;

#define ROTATEPOINT(s,nx,ny,sn,cs) nx = (s.x*cs + s.y*sn)/COSINEZERO;ny = (s.y*cs-s.x*sn)/COSINEZERO;

static VSIType vsi;
static GSIType gsi;

/********************************************************************************
 *
 * Map Scale control
 *
 *
 * Mapping basics:
 *
 *	 MAX(Int32) = PI Radians = 10800nm
 *
 * therefore:
 *
 * 	198841     = 1nm
 *
 * Local flat-earth approximation is used. Latitude corresponds directly to NM,
 * therefore in the y-axis it is possible to subtract one latitude from the
 * other to obtain y-range: y-range=lat2-lat1.
 *
 * For x-axis, the longitude is scaled according to cos(lat), but the same
 * applies: x-range = (lon2-lon1) * cos(lat)
 *
 */

#define MAX_SCALES 12

/*
 * the scales available in each of the units
 *
 */

static const Int16 scale[3][MAX_SCALES] = {

{ 1, 2, 5, 10, 15, 20, 30, 40, 50, 75, 100, 150 }, { 1, 2, 5, 10, 15, 20, 30,
		40, 50, 75, 100, 150 }, { 2, 5, 10, 20, 30, 40, 50, 75, 100, 150, 200,
		250 }

};

/*
 * scaleDivisorSeed is used to map from lat/lon to map coordinates for the
 * three unit scales. The formula is:
 *
 * SD (scaling divisor)
 * w  (screen width)
 * s  (scale, e.g. 10nm, 20nm etc)
 *
 * SD = s * 198841
 *      ----------
 *           w
 *
 * ( 198841 is for NM scales, use other (smaller) values for MI and KM )
 *
 */

static const Int32 scaleDivisorSeed[3] = {

NMUnitValue, NMUnitValue / MI_PER_NM, NMUnitValue / KM_PER_NM

};

/*
 * how many pixels are in 5nm, used for obstacle clearance
 *
 */

static Coord fiveNMPixels;

/*
 * shortcut to map settings in Preferences...
 *
 */

#define mapPrefs Preferences.mapSetting[Preferences.mapNumber]
#define mapPrefsNumber Preferences.mapNumber
#define mapPrefsScale Preferences.mapSetting[Preferences.mapNumber].scale

/*
 *
 * cpuWarn is decremented every time a map takes too
 * long to draw. Two consecutive long-draws will trip the
 * CPU warning
 *
 */

static Int16 cpuWarn = 2;

static const char blankTime[] = "--:--";

static Int16 editCursor = 0;

static char *importStatus;

#define MAX_LABEL_CHARS 20

#define MAX_INFO_LINE_CHARS 15

static WaypointIDType preselectedWaypoint;

static char selectionLine2[MAX_INFO_LINE_CHARS+1];
static char selectionLine1[MAX_INFO_LINE_CHARS+1];
static PFScreenRectType selectionInfoBox;

static Int16 fpVersion;

/*
 * Map quick find variables
 *
 * MQF allows the user to select a waypoint by using a keyboard to enter
 * the waypoint ID (max 5 chars).
 *
 */

#define MQF_MAX_CHARS 5
static char mqfWaypointID[MQF_MAX_CHARS+1] = "";
static Int32 mqfInputTimeout;

/*
 * tracks the value of the altitude slider
 *
 */

static Int32 altSlider = 0;

static MapSelection selection= NULL;

/*************************************************************************
 *
 * private functions
 *
 * Are declared here in order to allow the code to be placed into
 * alternate code sections
 *
 */

static void HighDensityCheck(void) MAP_SECTION;
static void ShowOrHideEditControls(Boolean preview) MAP_SECTION;
static void MapFormInit(FormPtr form) MAP_SECTION;
static void MapFormDeinit(FormPtr form) MAP_SECTION;
static void DrawSelectionBox(void) MAP_SECTION;
static void DrawHeadingArc(Int16 heading, Coord xc, Coord yc) MAP_SECTION;
static void DrawFlightDataDisplay(Boolean full) MAP_SECTION;
static void MapSpaceConstructor(Boolean initialise, UInt32 maxTicks) MAP_SECTION;
static void UpdateDisplay(void) MAP_SECTION;
static void SetupNewDisplay(void) MAP_SECTION;
static Boolean HandleMapQuickFindKey(char inputChar, Boolean *updateScreen) MAP_SECTION;
static Boolean HandleMapZoomEvent(Int16 direction) MAP_SECTION;
static Boolean HandleKeyEvent(EventPtr e) MAP_SECTION;
static Boolean HandleCtlSelectEvent(EventPtr event) MAP_SECTION;
static void SetupSelectionInformationBox(void) MAP_SECTION;
static Boolean HandlePenDownEvent(EventPtr e) MAP_SECTION;
static Boolean ResizeForm(FormPtr form) MAP_SECTION;
static void MapImportCallback(const char *s) MAP_SECTION;
static void MapImport(Int16 dbNumber) MAP_SECTION;

static Boolean HandleCtlRepeatEvent(EventPtr event) MAP_SECTION;
static void RunMapTests(void) MAP_SECTION;

static HandleMnInformationEvent(void) MAP_SECTION;
static void HandleMnNewWaypoint(void) MAP_SECTION;

/*
 * function : HighDensityCheck
 *
 * Sets up the lscreen structure for high-density devices
 *
 */

static void HighDensityCheck(void) {

	/*
	 * double screen dimensions if we're in high-density mode
	 *
	 */

	if (lscreen.highDensity) {

		LOGTAG("High-density");
		lscreen.width *= 2;
		lscreen.height *= 2;
		lscreen.xcentre *= 2;
		lscreen.ycentre *= 2;
		lscreen.standardPanelWidth *= 2;
		lscreen.standardHeight *= 2;
		lscreen.boldHeight *= 2;
		lscreen.largeBoldHeight *= 2;
		lscreen.ledHeight *= 2;

		liconWindows.aircraftDim *= 2;
		liconWindows.iconDim *= 2;

	}

}

/*
 * function : ShowOrHideEditControls
 *
 */

static void ShowOrHideEditControls(Boolean planning) {

	Int16 j;
	const FormPtr form = FrmGetActiveForm();

	if (planning) {

		if (!FpIsBlank(FlightPlan)) {

			GUIObjectShow( MapCursorLeftButton);
			GUIObjectShow( MapCursorRightButton);

		}

		//		GUIObjectShow( MapAltitudeSlider);

	} else {

		for (j=0; j<FrmGetNumberOfObjects(form); j++)
			FrmHideObject(form, j);

	}

}

/*
 * function : MapFormInit
 *
 */

static BitmapTypeV3 *testBmp;

static void MapFormInit(FormPtr form) {

	Coord width;
	UInt16 error;
	UInt16 *bmpBits;
	BitmapType *bm2;
	UInt32 j;

	LOGENTRY;

	bm2 = BmpCreate(480, 480, 16, NULL, &error);
	ModErrThrowIf(error);
	bmpBits = (UInt16*)BmpGetBits(bm2);
	testBmp = BmpCreateBitmapV3(bm2, kDensityDouble, bmpBits, NULL);
	
	for (j=0; j<480L*480L; j++) {
		bmpBits[j] = (UInt16) (j & 0xFFFF);
	}

	lscreen = pfScreen;
	liconWindows = IconWindows;

	HighDensityCheck();

	PFScreenRectangleSetRel(&mapNumberHotspot, 0, pfScreen.height
			- pfScreen.boldHeight, 33, pfScreen.boldHeight);

	displayMap = NULL;
	newMap = NULL;

	vsi = VSINew(pfScreen.width - 6, pfScreen.ycentre - 40, 5, 80, false,
			DisplayUnits.altitude == METRE_UNITS ? 300 : 800);

	gsi = GSINew(0, pfScreen.ycentre-40, 6, 80, DisplayUnits.altitude
			== METRE_UNITS ? 200 : 400, 100);

	fpVersion = FpGetVersion(FlightPlan, majorVersion);

	/*
	 * selection information box
	 *
	 */

	FntSetFont(stdFont);
	width = FntCharWidth('A')*MAX_INFO_LINE_CHARS;
	PFScreenRectangleSetRel(&selectionInfoBox, pfScreen.xcentre - width/2,
			pfScreen.height - pfScreen.boldHeight*3 - 1, width, pfScreen.boldHeight*2);

	fddUpperY1 = 0;
	fddUpperY2 = lscreen.ledHeight-3;
	fddStatusLineY = lscreen.height - lscreen.boldHeight;
	fddLowerLineY2 = fddStatusLineY - lscreen.largeBoldHeight;
	fddLowerLineY1 = 2+(fddLowerLineY2 - lscreen.largeBoldHeight);

	PFScreenRectangleSet(&timeFieldRect, 0, fddLowerLineY2, 0, fddLowerLineY2
			+lscreen.largeBoldHeight);
	PFScreenRectangleSet(&zoomInRect, 0, fddUpperY1, lscreen.width, fddUpperY2);
	PFScreenRectangleSet(&zoomOutRect, 2, fddUpperY1, 0, fddUpperY2);

	LOGEXIT;

}

/* 
 * function : MapFormDeinit
 *
 * Deinitialises the form as it closes
 *
 */

static void MapFormDeinit(FormPtr form) {

	if (newMap && displayMap != newMap)
		MapFree(newMap);

	if (displayMap)
		MapFree(displayMap);

	MapSelectionFree(&selection);

	BmpDelete(testBmp);
	PFEraseWindow();

	VSIFree(vsi);
	GSIFree(gsi);

}

/*
 * function : DrawSelectionBox
 *
 */

static void DrawSelectionBox(void) {

	if (mqfWaypointID[0] || selection) {

		/*
		 * draw selection information box
		 *
		 */

		Coord x = selectionInfoBox.x1+2;
		Coord y = selectionInfoBox.y1+1;

		PFSetForeColour(GUIGetSystemColour(GUIFormFrame));
		PFSetTextColour(GUIGetSystemColour(GUIObjectForeground));
		PFSetBackColour(GUIGetSystemColour(GUIObjectFill));

		PFEraseRectangle(&selectionInfoBox, 4);
		PFPaintRectangleFrame(&selectionInfoBox, roundFrame);

		PFSetForeColour(GUIGetSystemColour(GUIObjectForeground));

		if (mqfWaypointID[0]) {

			FntSetFont(largeBoldFont);
			DrawAlignedChars(mqfWaypointID, ALIGNCENTRE, x
					+ PFGetRectangleWidth(selectionInfoBox)/2, y);

		} else {

			FntSetFont(stdFont);
			PFDrawCharsTruncated(selectionLine1, StrLen(selectionLine1), x,y,
					PFGetRectangleWidth(selectionInfoBox)-4);
			PFDrawCharsTruncated(selectionLine2, StrLen(selectionLine2),
					x,y+FntLineHeight()-1,PFGetRectangleWidth(selectionInfoBox)-4);

		}

	}

}

/*
 * function : DrawHeadingArc
 *
 */

static void DrawHeadingArc(Int16 heading, Coord xc, Coord yc) {

	Int16 start;
	Int16 end;
	Int32 radius;
	Int16 j;
	LineType *lines = PFMalloc(sizeof(LineType)*80);
	LineType *ptr = lines;

	LOGENTRY;

	ModErrThrowIf(!lines);

	LOGTIMESTART;

	PFDrawStatePush();

	//PFSetForeColour(AppColourPrefs.black);
	PFSetTextColour(AppColourPrefs.black);

	WinSetDrawMode(winOverlay);
	FntSetFont(boldFont);

	if (lscreen.landscape) {

		start = heading - 120;
		end = heading + 130;

	} else {

		start = heading - 110;
		end = heading + 120;

	}

	WRAPMAX(start,360);
	WRAPMAX(end, 360);
	start = start/5;
	end = end/5;

	/*
	 * position is the centre of the arc
	 *
	 */

	radius = (Int32) (lscreen.standardPanelWidth / 2);

	j = start;
	do {

		Int32 sn, cs;
		Coord ax,ay;
		Coord x,y;
		Coord lastx, lasty;
		char label[3];
		Int16 blip = j*5 - heading;

		WRAPMAX(blip,360);

		/*
		 * ax,ay are coordinates relative to centre of arc,
		 * x,y are coordinates on screen
		 *
		 */

		GetSinCos(blip, &sn, &cs);
		ax = (Coord)(sn*radius/COSINEZERO);
		ay = (Coord)(cs*radius/COSINEZERO);

		x = ax + xc;
		y = yc - ay;

		if (j == start) {

			/*@i1@*/lastx = x; // used before definition
			/*@i1@*/lasty = y;
			j++;
			WRAPMAX(j,72);
			continue;

		}

		ptr->x1 = lastx; ptr->y1 = lasty;
		ptr->x2 = x; ptr->y2 = y;
		ptr++;

		if ((j & 1) == 0) {

			ptr->x1 = x; ptr->y1 = y;
			ptr->x2 = x-ax/16; ptr->y2 = y+ay/16;
			ptr++;

		}

		if (j % 6 == 0) {

			if (j == 0 || j == 18 || j == 36 || j == 54) {

				const char nesw[4] = "NESW";
				label[0] = nesw[j/18];
				label[1] = 0;

			} else {

				StrPrintF(label, "%02d", j/2);

			}

			if (lscreen.highDensity)

			PFDrawOutlineChars(label, ALIGNCENTRE, x-ax/8, y+ay/8 - FntLineHeight()/2);

			else

			DrawAlignedChars(label, ALIGNCENTRE, x-ax/8, y+ay/8 - FntLineHeight()/2);

		}

		lastx = x;
		lasty = y;
		j++;
		WRAPMAX(j,72);

	}while (j!=end);

	PFDrawThickLines(lines, ptr-lines, AppColourPrefs.black, AppColourPrefs.white, 1);

	WinPopDrawState();

	PFMemFree(lines);

	LOGTIMESTOP;

	LOGEXIT;

}

/*
 * function : DrawFlightDataDisplay
 *
 * If full=true, draws all flight data, otherwise just draws the status bar
 *
 */

static void DrawFlightDataDisplay(Boolean full) {

	char temp[22];
	PFScreenRectType r;

	LOGENTRY;

	PFDrawStatePush();

	/*
	 * draw status line
	 *
	 */

	PFSetDrawMode(blockMode);

	FntSetFont(boldFont);

	PFScreenRectangleSetRel(&r, 0, fddStatusLineY, lscreen.width,
			lscreen.boldHeight);

	/*
	 * if in landscape mode we need to reset the bar into the
	 * middle of the screen
	 *
	 */

	if (lscreen.landscape) {

		PFSetForeColour(AppColourPrefs.statusBarLight);
		PFPaintRectangle(&r, 4);

		PFScreenRectangleSetRel(&r, lscreen.xcentre - lscreen.standardPanelWidth/2,
				fddStatusLineY, lscreen.standardHeight, lscreen.boldHeight);

	}

	PFSetForeColour(GUIGetSystemColour(GUIFormFrame));
	PFPaintRectangle(&r, 4);

	/*
	 * draw leg info 
	 *
	 */

	PFSetBackColour(AppColourPrefs.statusBar);
	PFSetBackColour(GUIGetSystemColour(GUIFormFrame));
	PFSetTextColour(GUIGetSystemColour(GUIFormFill));

	FntSetFont(boldFont);
	if (TapPlanGetStatusString()) {

		PFSetTextColour(AppColourPrefs.black);
		PFDrawOutlineChars(TapPlanGetStatusString(), ALIGNCENTRE, lscreen.width
				/ 2, r.y1);

	} else {

		if (!planningMode && !FpIsBlank(FlightPlan)) {

			FntSetFont(stdFont);
			DrawAlignedChars(NavGetStrValue(navFromIdent), ALIGNRIGHT,
					lscreen.xcentre - FntCharWidth('\xbb'), r.y1);
			FntSetFont(boldFont);
			DrawAlignedChars(NavGetStrValue(navToIdent), ALIGNLEFT,
					lscreen.xcentre + FntCharWidth('\xbb'), r.y1);
			DrawAlignedChars("\xbb", ALIGNCENTRE, lscreen.xcentre, r.y1);

		}

		/*
		 * draw map scale & number
		 *
		 */

		if (PanGetModeString()) {

			StrCopy(temp, PanGetModeString());

		} else {

			StrPrintF(temp, StrMapNum, mapPrefsNumber+1);

		}
		DrawAlignedChars(temp, ALIGNLEFT, r.x1+3, r.y1);

		if (!cpuWarn) {

			PFSetTextColour(AppColourPrefs.red);
			StrPrintF(temp, "CPU!");

		} else {

			StrPrintF(temp, "%d%s",
					scale[DisplayUnits.distance][mapPrefsScale],
					UC.distanceUnits);
			cpuWarn = 2;

		}

		DrawAlignedChars(temp, ALIGNRIGHT, r.x2-3, r.y1);

	}

	PFSetTextColour(GUIGetSystemColour(GUIObjectForeground));
	PFSetBackColour(GUIGetSystemColour(GUIFormFill));
	PFSetForeColour(GUIGetSystemColour(GUIFormFill));

	if (PanGetModeString()) {

		FntSetFont(symbolFont);
		zoomOutRect.x2 = zoomOutRect.x1 + 2*PFDrawOutlineChars("\005",
				ALIGNLEFT, zoomOutRect.x1, zoomOutRect.y1);
		zoomInRect.x1 = lscreen.width - 2*PFDrawOutlineChars("\006",
				ALIGNRIGHT, zoomInRect.x2, zoomInRect.y1);

		//		PFSetForeColour(AppColourPrefs.black);
		//		PFPaintRectangleFrame(&zoomOutRect, roundFrame);
		//		PFPaintRectangleFrame(&zoomInRect, roundFrame);

	}

	/*
	 * status bar completed, return if that's all that's required
	 *
	 */

	if (!full) {

		WinPopDrawState();
		LOGEXIT;
		return;

	}

	/*
	 * track & ground speed only displayed if the GPS is running
	 *
	 */

	PFSetDrawMode(blockMode);
	if (!planningMode) {

		NavDrawField(navTrack, lscreen.xcentre-4, fddUpperY1, ALIGNCENTRE,
				ledFont);
		NavDrawField(navGS, lscreen.width, fddUpperY1, ALIGNRIGHT, ledFont);

		FlightDirectorDraw(lscreen.xcentre, 18, 30);

	}

	/*
	 * Range, bearing & time only drawn only if the GPS is running and
	 * flightplan has at least one waypoint
	 *
	 */

	if (planningMode) {

		WinPopDrawState();

		LOGEXIT;
		return;

	}

	//PFDrawOutlineChars(NavGetStrValue(navToIdent), 0, 0, ALIGNLEFT,boldFont);
	NavDrawField(navBearingTo, 0, fddUpperY1, ALIGNLEFT, ledFont);
	NavDrawField(navDistanceTo, 2, fddUpperY2, ALIGNLEFT, largeBoldFont);
	FntSetFont(largeBoldFont);
	PFDrawOutlineChars(NavGetStrValue(navXTRK), ALIGNRIGHT, lscreen.width,
			lscreen.ledHeight);

	PFSetDrawMode(blockMode);
	PFSetTextColour(AppColourPrefs.black);
	timeFieldRect.x2 = NavDrawField(mapTimeField, timeFieldRect.x1,
			timeFieldRect.y1, 
			ALIGNLEFT, largeBoldFont);

	/*
	 * draw VNAVETE and TurnETE times, if they have less than 60 seconds to run
	 *
	 */

	if (NavGetIntValue(navTurnETE) < 60) {

		NavDrawField(navTurnETE, 0, fddLowerLineY1, ALIGNLEFT, largeBoldFont);

	}

	if (NavGetIntValue(navVNAVETE) < 60) {

		NavDrawField(navVNAVETE, lscreen.width, fddLowerLineY1, ALIGNRIGHT,
				largeBoldFont);

	}

	/*
	 * altitude
	 *
	 */

	NavDrawField(navAltitude, lscreen.width, fddLowerLineY2, ALIGNRIGHT,
			largeBoldFont);

	/*
	 * VSI & GSI. GSI is only drawn if VNAV is armed
	 *
	 */

	if (pfScreen.highDensity)
		WinSetCoordinateSystem(kCoordinatesStandard);

	VSIDraw(vsi, NavGetIntValue(navVSI), NavGetIntValue(navVNAVRequiredVS));
	if (NavGetIntValue(navVNAVRequiredVS)) {

		GSIDraw(gsi, NavGetIntValue(navVNAVAltError));

	}

	WinPopDrawState();

	LOGEXIT;

}

/*
 * function : UpdateDisplay
 *
 */

static void UpdateDisplay(void) {

	Int32 mapLat, mapLon, mapRotation;
	Coord xOrigin, yOrigin;
	Coord aircraftx, aircrafty;
	Int32 lowFilter, highFilter;
	Int32 lowFilter2, highFilter2;
	Int32 terrainRefAlt;
	Boolean usedRefAlt = false;
	double selectionRange, selectionBearing;

	/*
	 * check menu not visible
	 *
	 */

	LOGENTRY;

	if (GUIMenuIsDisplayed()) {

		LOGEXIT;
		return;

	}

	if (Preferences.lastMapLat == 1 && Preferences.lastMapLon == 1
			&& FpIsBlank(FlightPlan)) {

		FntSetFont(boldFont);
		DrawAlignedChars(StrInitialiseMap, ALIGNCENTRE, 80, 80);

		LOGEXIT;
		return;

	}

	PFDrawStatePush();

	if (lscreen.highDensity)
		WinSetCoordinateSystem(kCoordinatesNative);

	PFScreenLock();

	/*
	 * set up translation matrix
	 *
	 * (Aircraft, rotation and screen origin)
	 * 
	 * map translation - move the map relative to the aircraft if
	 * we're not in pan mode
	 *
	 */

	if (PanGetModeString() || planningMode) {

		/*
		 * don't translate the map
		 *
		 */

		mapLat = MAPORIGIN;
		mapLon = MAPORIGIN;

	} else {

		/*
		 * translate the map to the aircraft origin
		 *
		 */

		mapLat = GPS.posn.lat32;
		mapLon = GPS.posn.lon32;

	}

	/*
	 * rotation, if in track-up mode
	 *
	 */

	if (!planningMode && mapPrefs.trackUp) {

		mapRotation = -(Int16)GPS.posn.trueHeading;
		WRAPMAX(mapRotation, 360);

	} else {

		mapRotation = 0;

	}

	/*
	 * screen origin
	 *
	 */

	xOrigin = lscreen.xcentre;
	if (!PanGetModeString() && !planningMode && mapPrefs.trackUp) {

		/*
		 * trackup locates the aircraft at the bottom of the screen
		 *
		 */

		yOrigin = TRACKUP_ORIGIN_Y;

	} else {

		/*
		 * otherwise, origin is the centre of the screen
		 *
		 */

		yOrigin = lscreen.ycentre;

	}

	/*
	 * set up airspace filtering limits
	 *
	 * Low & high filter define a band of airspace that we want to display, any
	 * airspace lying entirely outside of the band is not drawn.
	 *
	 * Low2 & high2 define a band of airspace within which the airspace is
	 * drawn with coloured borders for emphasis.
	 *
	 * Filtering only occurs when the GPS is on, or if the altitude slider is
	 * active.
	 *
	 */

	lowFilter = lowFilter2 = 0;
	highFilter = highFilter2 = 999999;

	if ((!planningMode || altSlider)
			&& (mapPrefs.lowerFilter || mapPrefs.upperFilter)) {

		Int32 gpsAltitude = (Int32) (GPS.posn.altitude);
		Int32 climbRate = 0;
		Int32 filterTolerance = 300;

		if (planningMode) {

			gpsAltitude = altSlider;
			filterTolerance = 0;

		}

		if (mapPrefs.lowerFilter) {

			lowFilter = gpsAltitude-(Int32)mapPrefs.lowerFilter * 1000;
			lowFilter2 = gpsAltitude - filterTolerance;

		}

		if (mapPrefs.upperFilter) {

			highFilter = gpsAltitude+(Int32)mapPrefs.upperFilter * 1000;
			highFilter2 = gpsAltitude + filterTolerance;

		}

		climbRate = (Int32) (GPS.posn.deltaAltitude * 60.0);

		if (!planningMode && climbRate) {

			if (climbRate > 0) {

				highFilter += climbRate;
				if (CycleCounter & 1)
					highFilter2 += (climbRate - filterTolerance);

			} else {

				lowFilter += climbRate;
				if (CycleCounter & 1)
					lowFilter2 += (climbRate + filterTolerance);

			}

		}
	}

	if (0) {

		terrainRefAlt = 99999;

		if (mapPrefs.terrain> terrainNormal && mapPrefs.terrainRefAlt> 0.0) {

			terrainRefAlt = (Int32) (mapPrefs.terrainRefAlt);
			usedRefAlt = true;

		}

	} else {

		if (planningMode) {

			terrainRefAlt = altSlider ? altSlider : 99999;

		} else {

			terrainRefAlt = (Int32)GPS.posn.altitude;

		}

		if (mapPrefs.terrain> terrainNormal && mapPrefs.terrainRefAlt> 0.0) {

			Int32 gpsAlt = (Int32)GPS.posn.altitude;

			terrainRefAlt = (Int32) mapPrefs.terrainRefAlt;
			if (gpsAlt < terrainRefAlt) {

				terrainRefAlt = gpsAlt;

			} else {

				usedRefAlt = true;

			}

		}

	}

	LOGINT32(terrainRefAlt);

	if (!MapDraw(displayMap, planningMode ? 5*SysTicksPerSecond()/16 : 5
			*SysTicksPerSecond()/16, mapLat, mapLon, (Int16)mapRotation,
			GPS.posn.lat32, GPS.posn.lon32, terrainRefAlt, xOrigin, yOrigin,
			lscreen.width, lscreen.height, liconWindows.iconDim, lowFilter,
			highFilter, lowFilter2, highFilter2, (Int32)GPS.posn.altitude,
			planningMode ? 99999 : (Int32)GPS.posn.altitude - 1000)) {

		if (cpuWarn)
			cpuWarn--;

	}

	MapSelectionDraw(displayMap, selection);

	if (usedRefAlt) {

		char alert[32];

		StrPrintF(alert, StrTref, FloatToStr((float)terrainRefAlt
				* UC.altitudeConv, 0), UC.altitudeUnits);

		FntSetFont(boldFont);
		PFSetTextColour(AppColourPrefs.black);
		PFDrawOutlineChars(alert, ALIGNCENTRE, lscreen.width / 2,
				lscreen.ledHeight);

	}

	/*
	 * draw highlight around selection
	 *
	 */

	if (MapSelectionIsWaypoint(selection) && TapPlanGetState() != tpInitial) {

		Coord ix, iy;
		PFScreenRectType r;
		const WorldCoords *w = MapSelectionGetCoords(selection);

		MapGetScreenCoords(displayMap, w->lat, w->lon, &ix, &iy);

		ix = ix - 2 - IconWindows.iconDim / 2;
		iy = iy - 2 - IconWindows.iconDim / 2;

		PFScreenRectangleSetRel(&r, ix, iy, IconWindows.iconDim+4,
				IconWindows.iconDim+4);
		PFSetForeColour(AppColourPrefs.green);
		PFPaintRectangleFrame(&r, boldRoundFrame);

	}

	/*
	 * update range, bearing & ETE for selected icon
	 *
	 */

	if (selection) {

		const WorldCoords *w = MapSelectionGetCoords(selection);

		selectionBearing = IntCalcGreatCircleCourse(GPS.posn.lat32, GPS.posn.lon32,
				w->lat, w->lon, &selectionRange);

	}

	if (selection && !planningMode && VALIDGPSFIX) {

		double range;
		Int16 intBearing;
		char time[10];
		double gs = (double)(GPS.posn.speed * UC.speedConv);

		DEG_MAGVAR_GPS(intBearing, selectionBearing, GPS.posn);

		range = selectionRange * (double)UC.distanceConv;

		if (GPS.posn.speed>0.0) {

			Int16 hour=0, min=0;
			UInt32 secsToGo = (UInt32) (range*3600/gs);

			min = (secsToGo / 60) % 60;
			hour = (secsToGo/3600);

			if (hour<100)

				StrPrintF(time, "%02d:%02d", hour, min);
			else

				StrCopy(time, blankTime);

		} else {

			StrCopy(time, blankTime);

		}

		StrPrintF(selectionLine1, "%03d\260 %s  %s", intBearing, DoubleToStr(
				range, range < 99.9 ? 1 : 0), time);

	}

	/*
	 * if the aircraft is on the screen, then draw it
	 *
	 */

	if (!planningMode && MapGetScreenCoords(displayMap, GPS.posn.lat32,
			GPS.posn.lon32, &aircraftx, &aircrafty)) {

		Int16 screenHeading = (Int16)GPS.posn.trueHeading + (Int16)mapRotation;
		Int16 iconIndex;
		PFScreenRectType r;
		LineType l[2];

		WRAPMAX(screenHeading,360);

		if (mapPrefs.showTrack) {

			double crs= DEG_TO_RAD(screenHeading);

			l[0].x1 = aircraftx;
			l[0].y1 = aircrafty;
			l[0].x2 = aircraftx+320*sin(crs);
			l[0].y2 = aircrafty-320*cos(crs);
			PFDrawThickLines(l, 1, AppColourPrefs.black, 0, 0);

			if (!FpIsBlank(FlightPlan) ) {

				crs = NavGetDoubleValue(navRadBearingTo) + DEG_TO_RAD(mapRotation);

				l[0].x1 = aircraftx;
				l[0].y1 = aircrafty;
				l[0].x2 = aircraftx+(lscreen.standardPanelWidth/2)*sin(crs);
				l[0].y2 = aircrafty-(lscreen.standardPanelWidth/2)*cos(crs);
				PFDrawThickLines(l, 1, AppColourPrefs.black, 0, 0);

			}

		}

		if (MapSelectionIsWaypoint(selection)) {

			double crs = selectionBearing + DEG_TO_RAD(mapRotation);
			l[0].x1 = aircraftx;
			l[0].y1 = aircrafty;
			l[0].x2 = aircraftx+(lscreen.standardPanelWidth/2)*sin(crs);
			l[0].y2 = aircrafty-(lscreen.standardPanelWidth/2)*cos(crs);

			PFDrawThickLines(l, 1, AppColourPrefs.pointer, 0, 0);

		}

		/*
		 * For lo-res, there are 16 aircraft in the bitmap (2 rows of 8). 360
		 * degrees needs to be mapped to 16 values from 0 to 15. This
		 * is 22.5 degrees per bitmap, multiplying everything by 4
		 * keeps the maths in integer.
		 *
		 * For hi-res, there are 32 aircraft (4 rows of 8) i.e. 11.25 degress per
		 * bitmap, so we multiply by 8
		 *
		 */

		if (lscreen.highDensity) {

			iconIndex = ( (UInt16)(screenHeading*8) + 45) / 90;
			WRAPMAX(iconIndex, 32);

		} else {

			iconIndex = ( (UInt16)(screenHeading*4) + 45) / 90;
			WRAPMAX(iconIndex, 16);

		}

		aircraftx -= liconWindows.aircraftDim/2;
		aircrafty -= liconWindows.aircraftDim/2;

		PFScreenRectangleSetRel(&r, (liconWindows.aircraftDim*(iconIndex & 7)),
				liconWindows.aircraftDim*((iconIndex & 0x18)/8),
				liconWindows.aircraftDim, liconWindows.aircraftDim);

		PFCopyRectangle(liconWindows.aircraft, &r, WinGetDrawWindow(),
				aircraftx, aircrafty, winOverlay);

		aircraftx += liconWindows.aircraftDim/2;
		aircrafty += liconWindows.aircraftDim/2;

		if (!VALIDGPSFIX) {

			PFSetTextColour(AppColourPrefs.warning);
			FntSetFont(largeBoldFont);

			PFDrawOutlineChars("???", ALIGNCENTRE, aircraftx, aircrafty
					- lscreen.largeBoldHeight/2);

		}

		/*
		 * draw the heading arc
		 *
		 */

		if (VALIDGPSFIX && !PanGetModeString() && mapPrefs.trackUp && mapPrefs.showHeadingArc) {

			Int16 heading;

			heading = (Int16)NavGetIntValue(navTrack);

			WRAPMAX(heading,360);
			DrawHeadingArc(heading, aircraftx, aircrafty);

		}

	}

	DrawFlightDataDisplay( (planningMode || PanGetModeString() ) ? false : true);

	LOGTAG("Bitmap");
	LOGTIMESTART;
	PFSetDrawMode(blockMode);
	WinPaintBitmap(testBmp, 10,10);
	LOGTIMESTOP;

	/*
	 * everything else on the screen is drawn using the standard coordinate system
	 *
	 */

	if (lscreen.highDensity)
		WinSetCoordinateSystem(kCoordinatesStandard);

	if (PanGetModeString()) {

		FntSetFont(boldFont);

		PFSetDrawMode(blockMode);
		PFSetTextColour(AppColourPrefs.black);
		PFSetBackColour(AppColourPrefs.white);

		//PFDrawOutlineChars(pan.state == panActive?StrPan:StrPlan,ALIGNRIGHT, Screen.width,
		//panResetHotspot.y1);

	}

	if (planningMode || PanGetModeString()) {

		FormPtr form = FrmGetActiveForm();
		Int16 j;
		char altSliderStr[16];

		for (j=0; j<FrmGetNumberOfObjects(form); j++)
			CtlDrawControl(FrmGetObjectPtr(form, j));

		//		StrPrintF(altSliderStr, "%ldft", altSlider);
		//		PFDrawOutlineChars(altSliderStr, ALIGNCENTRE, Screen.xcentre, 0);

	}

	DrawSelectionBox();

	WinPopDrawState();

	PFScreenUnlock();

	PFSendSimpleEvent(evtScreenRedrawn);

	LOGEXIT;

}

/*
 * function : MapSpaceConstructor
 *
 */

static void MapSpaceConstructor(Boolean initialise, UInt32 maxTicks) {

	static enum {starting, building, finished} state = starting;
	static UInt32 lastMapTimeStamp = 0;
	UInt32 now = TimGetTicks();

	LOGENTRY;

	if (initialise) {

		/*
		 * remove existing maps
		 *
		 */

		if (newMap && newMap != displayMap) {

			MapFree(newMap);

		}

		if (displayMap)
			MapFree(displayMap);

		displayMap = NULL;

		state = starting;

	}

	LOGLINE;

	/*
	 * check to see if its time to construct a new map space, if it isn't
	 * and the map is finished then we don't do anything
	 *
	 * NB 30 seconds, in conjunction with the 5nm/km overlap, allows an
	 * aircraft to travel at up to 600kts without icons suddenly appearing
	 * in the middle of the screen (due to a new mapspace)
	 * 
	 */

	if (state == finished) {

		if (TimerHasExpired(lastMapTimeStamp, SysTicksPerSecond()*30) ) {

			state = starting;

		} else {

			LOGEXIT;

			return;

		}

	}

	if (state == starting) {

		Int32 scaling;
		Int32 lat = 1, lon = 1;
		Coord width, height;

		WaypointClassType iconMask;

		LOGTAG("Starting new map");

		state = building;

		/*
		 * this can only be set by an external module (WPInfoDialog at the moment)
		 * so we can assume that selection is NULL
		 *
		 */

		if (preselectedWaypoint != wpNotFound) {

			Waypoint *wp = WDMGetWaypoint(WPDataset, preselectedWaypoint);

			selection = MapSelectionNewWaypoint(preselectedWaypoint);
			PanForceActivate(wp);
			PFMemFree(wp);

			SetupSelectionInformationBox();
			preselectedWaypoint = wdmNotFound;

		}

		/*
		 * setup scaling 
		 *
		 * Note the use of 'standard' width for the screen. The scale
		 * is always based on the standard width, not the landscape
		 * width. 
		 *
		 */

		//		scaling = (MAX(10,(Int32)NavGetFloatValue(navDistanceTo)*2.5)*
		scaling = (scale[DisplayUnits.distance][mapPrefsScale]
				* scaleDivisorSeed[DisplayUnits.distance]
				+ lscreen.standardPanelWidth/2) / lscreen.standardPanelWidth;

		fiveNMPixels = (lscreen.standardPanelWidth)*5 / scale[0][mapPrefsScale];

		LOGINT32(scaling);

		/*
		 * initialise centre of map space using current GPS position, pan
		 * position or waypoint position
		 *
		 */

		if (PanGetModeString()) {

			PanGetPosition(&lat, &lon);

		} else if (!planningMode) {

			/*
			 * aircraft position
			 *
			 */

			lat = GPS.posn.lat32;
			lon = GPS.posn.lon32;

			if (mapPrefs.trackUp) {

				/*
				 * the centre of the map is somewhere out in front of the aircraft or
				 * pan position when in track-up mode
				 *
				 * Work out how many pixels in front of the aircraft there are in
				 * trackup mode, then convert that into a radian-distance we can
				 * use to calculate lat/lon of a point that far in front of the
				 * position
				 *
				 */

				Int32 pixelsInFront= TRACKUP_ORIGIN_Y - lscreen.ycentre;
				double newLat= INT32_TO_RAD(lat);
				double newLon= INT32_TO_RAD(lon);
				double
						range= NM_TO_RAD((pixelsInFront * scale[0][mapPrefsScale]) / lscreen.standardPanelWidth);

				AvCalcShiftPoint(&newLat, &newLon, DEG_TO_RAD(GPS.posn.trueHeading), range);

				lat = RAD_TO_INT32(newLat);
				lon = RAD_TO_INT32(newLon);

			}

		} else {

			/*
			 * map preview mode, controlled by the edit cursor
			 *
			 */

			if (FpIsBlank(FlightPlan)) {

				lat = Preferences.lastMapLat;
				lon = Preferences.lastMapLon;

			} else {

				const FlightPlanLegWaypointType *wp;

				editCursor = MIN(editCursor, FpGetNumLegs(FlightPlan));
				wp = FpGetWaypoint(FlightPlan, editCursor);

				lat = wp->lat;
				lon = wp->lon;

			}

		}

		Preferences.lastMapLat = lat;
		Preferences.lastMapLon = lon;

		if (planningMode)
			GPSSetLocation(lat, lon);

		if (!PanGetModeString() && mapPrefs.trackUp && !planningMode) {

			width = height = MAX(lscreen.height, lscreen.width);

		} else {

			width = lscreen.width;
			height = lscreen.height;

		}

		LOGINT16(width);
		LOGINT16(height);

		iconMask = mapPrefs.icons;
		if (mapPrefsScale> 8)
			iconMask &= (~wpAnyObstacle);

		newMap = MapInit(lat, lon, scaling, width, height, 
		mapPrefs.route, mapPrefs.trackLog, iconMask, mapPrefs.labels, 
		mapPrefs.airspace, mapPrefs.airspaceLabels, FlightPlan, planningMode,
				mapPrefs.terrain);

		if (!displayMap)
			displayMap = newMap;

	}

	LOGLINE;

	if (maxTicks && MapBuild(newMap, TimerTicksLeft(now, maxTicks))) {

		state = finished;
		LOGTAG("Finished map");

		lastMapTimeStamp = now;

		/*
		 * update the selection information to point to the element in
		 * the new mapspace
		 *
		 * MapSelectByReference returns false if the selection can't be found in the
		 * map, but we ignore this because we allow the item to remain selected on this
		 * form, even if it can't be shown on the map.
		 * 
		 */

		//		if (selection) {
		//			
		//			MapSelectByReference(newMap);
		//				
		//		}

		/*
		 * swap newMap and displayMap if they are different
		 *
		 */

		if (newMap != displayMap) {

			MapType tmp = displayMap;
			displayMap = newMap;

			/*
			 * plan may have been edited while the map was being built
			 *
			 */

			MapUpdatePlan(displayMap, FlightPlan);

			/*
			 * delete old display map
			 *
			 */

			MapFree(tmp);
			newMap = NULL;

		}

	}

	LOGEXIT;

}

/*
 * function : SetupNewDisplay
 *
 */

static void SetupNewDisplay(void) {

	/*
	 * vary the initialisation time-slice according to
	 * GPS active or scroll key being held
	 *
	 */

	UInt32 timeSlice;

	if (planningMode) {

		timeSlice = SysTicksPerSecond()/2;

	} else {

		timeSlice = SysTicksPerSecond()/4;

	}

	if (PFKeyHeld(pfCursorKeys))
		timeSlice = 0;

	MapSpaceConstructor(true, timeSlice);
	ShowOrHideEditControls(planningMode);
	UpdateDisplay();

}

/*
 * function : HandleMapQuickFindKey
 *
 */

static Boolean HandleMapQuickFindKey(char inputChar, Boolean *updateScreen) {

	Boolean handled = false;
	Int16 len = StrLen(mqfWaypointID);

	if (inputChar >= 'a' && inputChar <= 'z')
		inputChar -= ('a'-'A');

	if (((inputChar >= 'A' && inputChar <='Z') || (inputChar >= '0'
			&& inputChar <='9')) && len < MQF_MAX_CHARS) {

		mqfWaypointID[len++] = inputChar;
		mqfWaypointID[len] = 0;
		handled = true;

	}

	if (inputChar == 8 && len > 0) {

		/*
		 * handle backspace
		 *
		 */

		mqfWaypointID[--len] = 0;
		handled = true;

	}

	if ((inputChar == ' ' || inputChar == 10) && len > 1) {

		WaypointIDType found= wpNotFound;

		//FntSetFont(stdFont);
		//PFDrawOutlineChars("Searching",ALIGNCENTRE, Screen.xcentre, Screen.ycentre);
		found = WDMSearchForWaypointByLocation(WPDataset, mqfWaypointID,
				Preferences.lastMapLat, Preferences.lastMapLon,
				!planningMode ? 0*RAD_TO_INT32(NM_TO_RAD(1500.0)) : 0);
		//PFDrawOutlineChars("Done",ALIGNCENTRE, Screen.xcentre, Screen.ycentre);

		if (found != wpNotFound) {

			MapSelectionFree(&selection);
			selection = MapSelectionNewWaypoint(found);

			SetupSelectionInformationBox();

			mqfWaypointID[0] = 0;
			*updateScreen = true;

			if (inputChar == 10) {

				Waypoint *w = WDMGetWaypoint(WPDataset, found);
				PanForceActivate(w);
				PFMemFree(w);

			}

		}

		handled = true;

	}

	if (handled) {

		DrawSelectionBox();
		mqfInputTimeout = PFGetTicks();

		/*
		 * any keypress cancels TapPlan mode
		 *
		 */

		TapPlanCancel();

	}

	return handled;

}

/*
 * function : HandleMapZoomEvent
 * 
 */

static Boolean HandleMapZoomEvent(Int16 direction) {

	if (direction < 0) {

		if (mapPrefsScale> 0) {

			mapPrefsScale--;
			return true;

		}

	} else if (direction > 0) {

		if (mapPrefsScale != MAX_SCALES-1) {

			mapPrefsScale++;
			return true;

		}

	}

	return false;

}

/*
 * function : HandleKeyEvent
 *
 */
static Boolean HandleKeyEvent(EventPtr event) {

	Boolean handled = false;
	Boolean updateReq = false;

	LOGINT16(event->data.keyDown.chr);

	if (PanHandleKeyEvent(event->data.keyDown.chr, FlightPlan, GPS, editCursor,
			displayMap, !GPSState, lscreen)) {

		handled = true;
		updateReq = true;

	} else {

		switch (event->data.keyDown.chr) {

#ifdef RUNMAPTESTS
		case 'r':
		RunMapTests();
		handled = true;
		break;
#endif

		case vchrPageDown:
			updateReq = handled = HandleMapZoomEvent(-1);
			break;

		case vchrPageUp:
			updateReq = handled = HandleMapZoomEvent(1);
			break;

		case vchrRockerLeft:

			if (mapPrefsNumber == 0)

				mapPrefsNumber = 3;

			else

				mapPrefsNumber --;

			updateReq = true;
			handled = true;
			break;

		case vchrRockerRight:

			mapPrefsNumber = (mapPrefsNumber + 1) % 4;
			updateReq = true;
			handled = true;
			break;

		}

	}

	if (!handled) {

		handled = HandleMapQuickFindKey(event->data.keyDown.chr, &updateReq);

	}

	if (updateReq) {

		SetupNewDisplay();

	}

	return handled;
}

/*
 * function : HandleCtlRepeatEvent
 *
 */

static Boolean HandleCtlRepeatEvent(EventPtr event) {

	switch (event->data.ctlRepeat.controlID) {

	case MapAltitudeSlider:
		altSlider = (Int32)event->data.ctlRepeat.value * 100;
		return true;
		break;

	default:
		return false;
		break;

	}

	return true;

}

/*
 * function : HandleCtlSelectEvent
 *
 */

static Boolean HandleCtlSelectEvent(EventPtr event) {

	Boolean handled = false;
	Boolean updated = false;
	Boolean redraw = true;

	editCursor = MIN(editCursor, FpGetNumLegs(FlightPlan));

	switch (event->data.ctlSelect.controlID) {

	case MapAltitudeSlider:
		//altSlider = CtlGetValue(GetObjectPtr(FrmGetActiveForm(), MapAltitudeSlider)) * 100;
		//redraw = true;
		break;

	case MapCursorLeftButton:
		if (editCursor > 0) {

			if (PanGetModeString()) {

				PanForceDeactivate();

			} else {

				editCursor --;

			}

			if (planningMode) {

				SetupNewDisplay();

			} else {

				ModErrThrowIf(1);

			}

		}
		handled = true;
		break;

	case MapCursorRightButton:
		if (editCursor < FpGetNumLegs(FlightPlan)) {

			if (PanGetModeString()) {

				PanForceDeactivate();

			} else {

				editCursor++;

			}
			if (planningMode) {

				SetupNewDisplay();

			} else {

				ModErrThrowIf(1);

			}

		}
		handled = true;
		break;

	}

	if (!FpIsBlank(FlightPlan)) {

		editCursor = MIN(editCursor, FpGetNumLegs(FlightPlan));
		FpSetCurrentLeg(FlightPlan, MIN(FpGetCurrentLeg(FlightPlan), FpGetNumLegs(FlightPlan)-1));

	}

	if (updated) {

		SetupNewDisplay();

	} else if (redraw) {

		UpdateDisplay();

	}

	return handled;

}

/*
 * function : SetupSelectionInformationBox
 * 
 */

static void SetupSelectionInformationBox(void) {

	if (MapSelectionIsWaypoint(selection)) {

		if (MapSelectionGetType(selection) == msWaypoint) {

			Waypoint *wp = WDMGetWaypoint(WPDataset,
					MapSelectionGetWaypointID(selection));

			StrNCopy(selectionLine2, GetStringFromList(wp->ident, 1),
					MAX_INFO_LINE_CHARS);

			PFMemFree(wp);

		} else if (MapSelectionGetType(selection) == msFreePoint) {

			StrNCopy(selectionLine2, "CURSOR", MAX_INFO_LINE_CHARS);

		} else {

			StrNCopy(selectionLine2, "Route Waypoint", MAX_INFO_LINE_CHARS);

		}

		selectionLine1[0] = 0;

	} else if (MapSelectionGetType(selection) == msAirspace) {

		AirspaceType *a = AsGetAirspace(MapSelectionGetAirspaceID(selection));

		char upperAlt[12], lowerAlt[12];
		Int32 altFeet;
		Int16 j;

		if (planningMode) {

			StrNCopy(selectionLine1, GetStringFromList(a->segmentCode, 1),
					MAX_INFO_LINE_CHARS+1);

			for (j=0; j<StrLen(selectionLine1); j++)
				if (selectionLine1[j] == '\n')
					selectionLine1[j] = 0;

		} else {

			selectionLine1[0] = 0;

		}

		AsDecodeAltitude(lowerAlt, &altFeet, DisplayUnits.altitude,
				a->lowerAltRef, a->lowerAlt);
		AsDecodeAltitude(upperAlt, &altFeet, DisplayUnits.altitude,
				a->upperAltRef, a->upperAlt);

		if (a->type & (asTypeClassA | asTypeClassBG)) {

			char type;

			switch (a->type & asTypeMask) {
			case asTypeClassA:
				type='A';
				break;
			case asTypeClassB:
				type='B';
				break;
			case asTypeClassC:
				type='C';
				break;
			case asTypeClassD:
				type='D';
				break;
			case asTypeClassE:
				type='E';
				break;
			case asTypeClassF:
				type='F';
				break;
			case asTypeClassG:
				type='G';
				break;
			default:
				type='O';
				break;
			}

			StrPrintF(selectionLine2, "X %s-%s", lowerAlt, upperAlt);
			selectionLine2[0] = type;

		} else if (a->type & asTypeAirway) {

			StrPrintF(selectionLine2, "%s-%s", lowerAlt, upperAlt);

		} else if (a->type & asTypeSUAS) {

			StrPrintF(selectionLine2, "%s %s-%s",
					suasTypes[a->type & asSubTypeMask], lowerAlt, upperAlt);

		}

		DBRecordFree(a);

	}

}
/*
 * function : HandlePenDownEvent
 *
 */

static Boolean HandlePenDownEvent(EventPtr event) {

	MapSelection newSelection;

	LOGENTRY;

	// TODO- need some way to activate pan for non-5-way devices

#ifdef XXXX
	if (RctPtInRectangle(event->screenX, event->screenY, &panResetHotspot)) {

		if (pan.state == panInvalid) {

			if (!planningMode) {

				pan.lat = GPS.posn.lat32;
				pan.lon = GPS.posn.lon32;

			} else {

				if (FpIsBlank(FlightPlan)) {

					pan.lat = Preferences.lastMapLat;
					pan.lon = Preferences.lastMapLon;

				} else {

					Waypoint *wp;

					editCursor = MIN(editCursor, FpGetNumLegs(FlightPlan));
					wp = FpGetWaypoint(FlightPlan, editCursor);

					pan.lat = RAD_TO_INT32(wp->latitude);
					pan.lon = RAD_TO_INT32(wp->longitude);

					PFMemFree(wp);

				}

			}

			pan.state = panInactive;

		} else if (pan.state == panInactive) {

			pan.waypointNum = FpGetCurrentLeg(FlightPlan);

		}

		SetupNewDisplay();

		return true;

	}

	if (pan.state >= panActive && RctPtInRectangle(event->screenX, event->screenY, &panBackHotspot)) {

		if (pan.state == panActive) {

			pan.state = panInactive;

		} else {

			pan.state = panActive;

		}
		SetupNewDisplay();

		return true;

	}
#endif

	/*
	 * check for tap in the selection box
	 *
	 */

	if (selection && PFScreenPointInRectangle(event->screenX, event->screenY,
			&selectionInfoBox)) {

		MapSelectionFree(&selection);

		ShowOrHideEditControls(planningMode);

		UpdateDisplay();
		return true;

	}

	/*
	 * otherwise, see if user tapped on something
	 *
	 *
	 */

	/*
	 * scale the tap position...
	 *
	 */

	if (lscreen.highDensity) {

		event->screenX *= 2;
		event->screenY *= 2;

	}

	if (PanGetModeString()) {

		Int16 zoomDir = 0;

		if (PFScreenPointInRectangle(event->screenX, event->screenY,
				&zoomInRect)) {

			zoomDir = -1;

		} else if (PFScreenPointInRectangle(event->screenX, event->screenY,
				&zoomOutRect)) {

			zoomDir = 1;

		}

		if (HandleMapZoomEvent(zoomDir)) {

			SetupNewDisplay();
			return true;

		}

	}

	if (VALIDGPSFIX && PFScreenPointInRectangle(event->screenX, event->screenY, &timeFieldRect)) {

		if (mapTimeField<navUTC)
			mapTimeField++;
		else
			mapTimeField=navETE;

		UpdateDisplay();
		return true;

	}

	newSelection = MapSelectByCoord(displayMap, event->screenX, event->screenY,
			lscreen.highDensity ? 10 : 5, selection);
	MapSelectionFree(&selection);
	selection = newSelection;

	SetupSelectionInformationBox();

	/*
	 * pan the map if in pan-mode
	 *
	 */

	//TODO  - need some way to pan if the device doesn't have a 5-way navigator

#ifdef XXXX
	if (pan.state == panActive) {

		/*
		 * pan the map, if the tap was far enough away from the centre of the
		 * screen
		 *
		 */

		Int32 xdiff = event->screenX - lscreen.xcentre;
		Int32 ydiff = event->screenY - lscreen.ycentre;
		Int32 threshold = lscreen.xcentre/3;

		threshold *= threshold;
		xdiff *= xdiff;
		ydiff *= ydiff;

		if (xdiff + ydiff> threshold) {

			MapGetLatLon(displayMap, event->screenX, event->screenY, &pan.lat, &pan.lon);

			SetupNewDisplay();

		} else {

			UpdateDisplay();

		}

	} else {

		UpdateDisplay();

	}
#endif
	UpdateDisplay();

	LOGEXIT;

	return true;

}

/*
 * function : ResizeForm
 *
 * Called in response to user opening/closing the dynamic input area
 *
 */

static Boolean ResizeForm(FormPtr form) {

	Int16 ydelta, xdelta;
	Coord x, y;
	Int16 i;

	if (!CollapseResizeForm(form, false, false, &xdelta, &ydelta)) {

		return false;

	}

	for (i=0; i < FrmGetNumberOfObjects(form); i++) {

		FrmGetObjectPosition(form, i, &x, &y);

		x += xdelta/2;

		if (y > 5)
			y += ydelta;

		FrmSetObjectPosition(form, i, x, y);

	}

	return true;

}

/*
 * function : MapImportCallback
 *
 */

static void MapImportCallback(const char *s) {

	LOGENTRY;

	GUIFieldSetText(ImportStatusField, s, true);

	StrCopy(importStatus, s);

	LOGEXIT;

}

/*
 * function : MapImport
 *
 * Manages the import of the specified database, 0 = base, 1 = TFR
 *
 */

static void MapImport(Int16 dbNumber) {

	const char *dbname[] = { "base", "TFR" };
	FormPtr form;
	FormPtr oldForm = FrmGetActiveForm();

	if (FrmCustomAlert(ConfirmImportAlert,dbname[dbNumber],NULL,NULL))

		return;

	PFSafeMalloc(importStatus, 256);

	form = FrmInitForm(ImportDialog);
	FrmDrawForm(form);
	FrmSetActiveForm(form);

	AsCloseDatabase();
	if (!AsImport(dbNumber, &MapImportCallback)) {

		FrmCustomAlert(AsImportFailedAlert,importStatus, NULL,NULL);

	}

	AsOpenDatabase();

	FrmEraseForm(form);
	FrmDeleteForm(form);
	FrmSetActiveForm(oldForm);

	PFMemFree(importStatus);

	SetupNewDisplay();

}

#ifdef RUNMAPTESTS

/*
 * function : RunMapTests
 *
 */

typedef struct {

	Int32 lat,lon;

	Int32 value;

}MTMaxType;

static void MaxCheck(Int32 value, MTMaxType *mt) {

	if (value> mt->value) {

		mt->lat = GPS.posn.lat32;
		mt->lon = GPS.posn.lon32;
		mt->value = value;

	}

}

static void RunMapTests(void) {

#define UKSMALL

#ifdef UK
	const Int32 latStart= RAD_TO_INT32(DEG_TO_RAD(60));
	const Int32 lonStart= RAD_TO_INT32(DEG_TO_RAD(-5));
	const Int32 latStop= RAD_TO_INT32(DEG_TO_RAD(48));
	const Int32 lonStop= RAD_TO_INT32(DEG_TO_RAD(4));
#elif defined(US)
	const Int32 latStart= RAD_TO_INT32(DEG_TO_RAD(55));
	const Int32 lonStart= RAD_TO_INT32(DEG_TO_RAD(-130));
	const Int32 latStop= RAD_TO_INT32(DEG_TO_RAD(35));
	const Int32 lonStop= RAD_TO_INT32(DEG_TO_RAD(-70));
#elif defined(EU)
	const Int32 latStart= RAD_TO_INT32(DEG_TO_RAD(60));
	const Int32 lonStart= RAD_TO_INT32(DEG_TO_RAD(-5));
	const Int32 latStop= RAD_TO_INT32(DEG_TO_RAD(40));
	const Int32 lonStop= RAD_TO_INT32(DEG_TO_RAD(30));
#elif defined(UKSMALL)
	const Int32 latStart= RAD_TO_INT32(DEG_TO_RAD(53));
	const Int32 lonStart= RAD_TO_INT32(DEG_TO_RAD(-2));
	const Int32 latStop= RAD_TO_INT32(DEG_TO_RAD(50.5));
	const Int32 lonStop= RAD_TO_INT32(DEG_TO_RAD(0));
#endif

	const Int32 latStep = 1 * NMUnitValue; // 10 minutes
	const Int32 lonStep = 1 * NMUnitValue;
	const float headingStep = 360;

	MTMaxType maxBuild = {0,0,0};
	MTMaxType maxRender = {0,0,0};
	MTMaxType maxMem = {0,0,0};

	Int32 numTests = 0;
	Int32 overallTimer = TaskTimer(NEWTASKTIMER, false);

	char logStr[128];

	LogTag("Starting map tests");

#ifdef PROFILE

	HostProfileStart();

#endif

	ErrTry {

		for (GPS.posn.lat32 = latStart;
				GPS.posn.lat32> latStop;
				GPS.posn.lat32 -= latStep) {

			for (GPS.posn.lon32 = lonStart;
					GPS.posn.lon32 < lonStop;
					GPS.posn.lon32 += lonStep) {

				Int32 timer;
				Int32 buildTime;
				Int32 renderTime;
				Int32 memBefore, memAfter, maxContBefore, maxContAfter;

				/*
				 * clean out the existing map
				 *
				 */

				if (newMap && displayMap != newMap) {

					MapFree(newMap);

				}
				MapFree(displayMap);
				newMap = displayMap = NULL;

				/*
				 * start of test - create the map
				 *
				 */

				MemHeapFreeBytes(0, &memBefore, &maxContBefore);

				timer = TaskTimer(NEWTASKTIMER, false);
				MapSpaceConstructor(true, SysTicksPerSecond()*5);
				buildTime = TaskTimer(timer, true);

				MemHeapFreeBytes(0, &memAfter, &maxContAfter);

				/*
				 * draw the map multiple times
				 *
				 */

				timer = TaskTimer(NEWTASKTIMER, false);
				for (GPS.posn.trueHeading = 0.0;
						GPS.posn.trueHeading < 360.0;
						GPS.posn.trueHeading += headingStep) {

					UpdateDisplay();

				}
				renderTime = TaskTimer(timer, true);

				StrPrintF(logStr, "%ld,%ld Build:%ld Render:%ld MemUsed: %ld",
						GPS.posn.lat32, GPS.posn.lon32,
						buildTime, renderTime, memBefore - memAfter);

				MaxCheck(buildTime, &maxBuild);
				MaxCheck(renderTime, &maxRender);
				MaxCheck(memBefore-memAfter, &maxMem);

				LogTag(logStr);

				numTests++;

			}

		}

	}ErrCatch(errNo) {

		ErrThrow(errNo);

	}ErrEndCatch;

	StrPrintF(logStr,"Completed %ld Map Tests in %ld ticks", numTests, TaskTimer(overallTimer, true));
	LogTag(logStr);

	StrPrintF(logStr, "Max build: %ld (%ld,%ld)", maxBuild.value, maxBuild.lat, maxBuild.lon);
	LogTag(logStr);
	StrPrintF(logStr, "Max render: %ld (%ld,%ld)", maxRender.value, maxRender.lat, maxRender.lon);
	LogTag(logStr);
	StrPrintF(logStr, "Max mem: %ld (%ld,%ld)", maxMem.value, maxMem.lat, maxMem.lon);
	LogTag(logStr);

#ifdef PROFILE

	HostProfileStop();

#endif

}

#endif

/*
 * function : HandleInformationRequest
 * 
 */

static HandleMnInformationEvent(void) {

	WaypointIDType wpID= wpNotFound;

	if (selection) {

		if (MapSelectionIsWaypoint(selection)) {

			if (MapSelectionGetWaypointID(selection) != wpNotFound) {

				wpID = MapSelectionGetWaypointID(selection);

			} else {

				wpID = WDMSearchForWaypointByLocation(WPDataset,
						MapSelectionGetIdent(selection), MapSelectionGetCoords(selection)->lat, MapSelectionGetCoords(selection)->lon, 0);

			}

		} else if (MapSelectionIsAirspace(selection)) {

			AirspaceType *airspace =
					AsGetAirspace(MapSelectionGetAirspaceID(selection));
			char *notesStr = PFMalloc(1024);

			if (airspace->extra) {

				StrPrintF(notesStr, "%d.%d\n", (airspace->extra & 0xFF00) >> 8,
						airspace->extra &0xFF);

			}
			StrNCopy(notesStr, GetStringFromList(airspace->segmentCode, 1),
					PFMallocSize(notesStr));
			LOGSTR(notesStr);
			TextDialogSet(notesStr);

			DBRecordFree(airspace);
			PFMemFree(notesStr);

			FrmPopupForm(TextDialog);

			return;

		}

	} else if (!FpIsBlank(FlightPlan)) {

		const FlightPlanLegWaypointType *fpwp= FpGetCurrentWaypoint(FlightPlan);
		wpID = WDMSearchForWaypointByLocation(WPDataset, fpwp->ident,
				fpwp->lat, fpwp->lon, 0);

		if (wpID == wpNotFound) {
			// something
		}

	}

	if (wpID != wpNotFound) {

		WPInfoSetWaypointInfo(wpID);
		FrmPopupForm(WPInfoDialog);

	}

}

/*
 * function : HandleMnNewWaypoint
 * 
 */

static void HandleMnNewWaypoint(void) {

	if (MapSelectionIsWaypoint(selection)
	&& WDMGetWaypointDatabase(MapSelectionGetWaypointID(selection)) ==wdmTempDB) {

		const WorldCoords *wc = MapSelectionGetCoords(selection);

		/*
		 * deleting the mark point 
		 *
		 */

		WDMDeleteWaypoint(WPDataset, MapSelectionGetWaypointID(selection));

		MapSelectionFree(&selection);
		selection = MapSelectionNewFreePoint(wc);
		SetupSelectionInformationBox();

		SetupNewDisplay();

	} else {

		Waypoint *wp;
		char latlon[24], north[16], east[16];
		Int32 lat, lon;
		float mv;

		if (MapSelectionGetType(selection) == msFreePoint
				|| MapSelectionGetType(selection) == msAirspace) {

			lat = MapSelectionGetCoords(selection)->lat;
			lon = MapSelectionGetCoords(selection)->lon;
			mv = MapSelectionGetMagVar(selection);

		} else {

			lat = GPS.posn.lat32;
			lon = GPS.posn.lon32;
			mv = DEG_TO_RAD(GPS.posn.magVarn);

		}
		GPSCoordsToText(INT32_TO_RAD(lat), INT32_TO_RAD(lon), north, east);

		StrPrintF(latlon, "%s %s", north, east);
		wp = WDMNewWaypoint(StrMark, latlon, lat, lon, mv);
		WDMSaveWaypoint(WPDataset, wdmTempDB, wp);

		SetupNewDisplay();

	}

}

/**************************************************************************
 *
 * public functions
 *
 */

/* 
 * function : MapFormHandleEvent
 *
 *
 * Returns true if the event is handled
 *
 */

Boolean MapFormHandleEvent(EventPtr event) {
	Boolean handled = false;
	Boolean redraw = false;
	FormPtr form = FrmGetActiveForm();
	WaypointIDType wpID= wpNotFound;

	LOGENTRY;

	TapPlanStateMachineUpdate(false, false, &FlightPlan, FlightPlanStack,
			&selection, GPS, WPDataset);

	/*
	 * cancel the Map Quick Find input if the user hasn't done
	 * anything for 5 seconds
	 *
	 */

	if (mqfWaypointID[0] && PFTimerHasExpired(mqfInputTimeout,
			SysTicksPerSecond()*5)) {

		mqfWaypointID[0] = 0;

	}

	switch (event->eType) {
	case frmOpenEvent:
		planningMode = !GPSState;
		CollapseSetState(form, collapseStateUser);
		ResizeForm(form);
		ShowOrHideEditControls(planningMode);
		FrmDrawForm(form);
		MapFormInit(form);
		SetupNewDisplay();
#ifdef PROFILE
		RunMapTests();
#endif
		handled = true;
		break;

	case winDisplayChangedEvent:

		LOGTAG("DisplayChanged");
		if (ResizeForm(form) || CollapseCheckForPin10NeedToRedraw()) {

			MapFormDeinit(form);
			MapFormInit(form);
			SetupNewDisplay();

		}

		handled = true;
		break;

	case winEnterEvent:
		break;
		// TODO - remove winEnterEvent

		//		if (myWindow == event->data.winEnter.enterWindow) {
		//
		//			redraw = true;
		//			handled = true;
		//
		//		}

		break;

	case frmUpdateEvent:
		LOGTAG("FrmUpdate");
		redraw = true;
		handled = true;
		break;

	case ctlRepeatEvent:
		if (HandleCtlRepeatEvent(event)) {

			/*
			 * only Altitude Slider can generate this event, and it requires a
			 * redraw afterwards
			 *
			 */

			redraw = true;

		}

		break;

	case ctlSelectEvent:
		handled = HandleCtlSelectEvent(event);
		break;

	case evtGPSFix:

		MapSpaceConstructor(true, SysTicksPerSecond()/8);
		handled = true;
		break;

	case nilEvent:
	case evtGPSPositionUpdate:
	case evtGPSFixLost:

		/*
		 * frame counter code. Enable this if you want to see
		 * how often the screen updates
		 *
		 */

		if (0) {
			static Int16 frameCount = 0;
			//			DebugDrawChar('0'+(frameCount++ % 10),10,10);
			//			if (event->eType == nilEvent) DebugDrawChar('0'+(frameCount % 10),30,10);
		}

		if (FpGetVersion(FlightPlan, majorVersion) != fpVersion) {

			fpVersion = FpGetVersion(FlightPlan, majorVersion);
			MapUpdatePlan(displayMap, FlightPlan);
			redraw = true;

		} else {

			MapSpaceConstructor(false, SysTicksPerSecond()/8);
			redraw = true;

		}

		handled = true;
		break;

	case evtMapConfigUpdate:

		SetupNewDisplay();
		handled = true;
		break;

	case keyDownEvent:
		handled = HandleKeyEvent(event);
		break;

	case penDownEvent:

		/*
		 * any tap on the screen cancels TapPlan mode
		 *
		 */

		TapPlanCancel();

		/*
		 * check for tap in the Map number hotspot; if so then advance the map
		 * to the next one...
		 *
		 */

		if (PFScreenPointInRectangle(event->screenX, event->screenY,
				&mapNumberHotspot)) {

			if (event->screenX > mapNumberHotspot.x1 + PFGetRectangleWidth(mapNumberHotspot)/2) {

				mapPrefsNumber = (mapPrefsNumber + 1) % 4;

			} else {

				if (mapPrefsNumber)

					mapPrefsNumber --;

				else
					mapPrefsNumber = 3;

			}

			SetupNewDisplay();

			handled = true;

		}

		/*
		 * 
		 * don't respond to tap in status bar
		 *
		 */

		if ( !handled && event->screenY < pfScreen.height -pfScreen.boldHeight) {

			/*
			 * if editmode, allow the OS to handle taps in the edit
			 * buttons before we try to handle the tap ourselves
			 *
			 */

			if ((PanGetModeString() || planningMode) && FrmHandleEvent(form,
					event)) {

				handled = true;

			} else {

				if (selection && event->tapCount > 1 && !PanGetModeString()) {

					event->eType = menuEvent;
					event->data.menu.itemID = MnInformation;
					PFEventSend(event, false);
					handled = true;

				} else {

					handled = HandlePenDownEvent(event);

				}

			}

		}

		break;

	case penMoveEvent:
	case penUpEvent:
		handled = true;
		break;

	case evtFlightPlanLoaded:
	case evtCrsOverride:
		editCursor = 0;
		MapUpdatePlan(displayMap, FlightPlan);
		redraw = true;
		break;

	case menuEvent:

		switch (event->data.menu.itemID) {

		case MnImportBase:
			MapImport(0);
			handled = true;
			break;

		case MnImportTFR:
			MapImport(1);
			handled = true;
			break;

		case MnClearTrackLog:
			SetupNewDisplay();
			handled = true;
			break;

		case MnMapConfig:
			FrmPopupForm(MapDialog);
			handled = true;
			break;

		case MnNextLeg:
		case MnPreviousLeg:
		case MnFlightFlipFlop:
			MapUpdatePlan(displayMap, FlightPlan);
			redraw = true;
			handled = true;
			break;

		case MnUndoEdit:
			MapUpdatePlan(displayMap, FlightPlan);
			redraw = true;
			TapPlanCancel();
			handled = true;
			break;

		case MnEditPlan:
		case MnDivertEmergency:

			/*
			 * if activating a diversion, check to see if a waypoint is
			 * selected; if so and that waypoint is one of the route
			 * waypoints then activate a normal diversion.
			 *
			 * Emergency diversions are different, the diversion will have
			 * already been set up by the diversion manager, we only need
			 * to update the display.
			 *
			 */

			if (event->data.menu.itemID == MnEditPlan) {

				TapPlanStateMachineUpdate(true, planningMode, &FlightPlan,
						FlightPlanStack, &selection, GPS, WPDataset);

			}
			MapUpdatePlan(displayMap, FlightPlan);
			redraw = true;

			handled = true;
			break;

		case MnInformation:
			HandleMnInformationEvent();
			handled = true;
			break;

		case MnNewWaypoint:
			HandleMnNewWaypoint();
			handled = true;
			break;

		}

		break;

	case frmCloseEvent:
		MapFormDeinit(form);
		handled = false;
		break;

	default:
		break;
	}

	if (redraw)
		UpdateDisplay();

	LOGEXIT;

	return handled;
}

/*
 * MapSetSelection
 *
 * Allows external client to set the pan position while
 * the map isn't active
 *
 */

void MapSetSelection(WaypointIDType wp) {

	preselectedWaypoint = wp;

}

UInt16 MapUnitTestHead(TestActionType action, UInt16 extra, HostFILE *f) {

#ifdef UNITTEST
	UInt16 (* const moduleTests[])(HostFILE *f) = {
		MapUT1,
		MapUT2,
		MapUT3,
		MapUT4
	};

	switch (action) {

		case reportNumTests:
		return sizeof(moduleTests)/sizeof(void*);

		case executeTest:
		return moduleTests[extra](f);

		default:
		ErrFatalDisplay("Invalid test action");
		break;
	}
#else
	return 0;
#endif
}

