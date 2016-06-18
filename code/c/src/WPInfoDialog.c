/*
 * WPInfoDialog.c
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
#include <PalmOSGlue.h>
#include "GlobalTypes.h"
#include "ResourceDefines.h"
#include "WDManager.h"

#include "WPInfoDialog.h"

#include "Utils.h"
#include "MathLib.h"
#include "Modules.h"


#include "FlightPlan.h"
#include "MapForm.h"

#define ModuleID WPInfoFormModuleID

/*******************************************************************************
 * 
 * global variables
 *
 * 
 */

extern const WDMHandle  WPDataset;
extern const UserConversionType UC;
extern const FlightPlanType FlightPlan;
extern const GPSType GPS;
extern FlightPlanStackType FlightPlanStack;
extern const Boolean GPSState;

/*******************************************************************************
 * 
 * module variables
 *
 * 
 */


/*
 * pointer to the waypoint, this must be initialised by a call to
 * WPInfoSetWaypointInfo *before* the info dialog is displayed.
 *
 * The memory will be deallocated before the dialog closes.
 *
 */

static WaypointIDType waypointID;
static Waypoint *infoWp;

/*
 * indicates the current display mode, ALL, Freq or Runways.
 *
 * The sequence here should correspond to the resource Id of the 
 * corresponding pushbuttons
 *
 */

static enum {all = 0, frequencies, runways, ils} displayMode = all;

/*
 * pointer to array of pointers for the start of each line
 * that should be displayed in the listbox
 *
 */

static const char **linePtrs = NULL;

#define MAX_LINES 64

/*******************************************************************************
 *
 * private functions
 *
 * are declared here to allow easy relocation to another code section
 * 
 */

static void FormInit(void) MODALDIALOGS_SECTION;
static UInt16 SetupListLines(void) MODALDIALOGS_SECTION; 
static void FormDeinit(void) MODALDIALOGS_SECTION;
static void DrawOneLine(Int16 itemNum, PFScreenRectType *bounds) MODALDIALOGS_SECTION;

/*
 * function : FormInit
 *
 */

static void FormInit(void) {

	char    tmp[128];
	const char   *ptr = GetStringFromList(infoWp->ident,2);
	UInt16 lineCount = 0;

	LOGLINE;

	/*
	 * determine number of lines 
	 *
	 */

	while (*ptr != 0) {
		if (*ptr == '\n') lineCount++;
		ptr++;
	}


	PFSafeMalloc(linePtrs, sizeof(char*)*MAX_LINES);

	GUIObjectGroupSetValue(WPInfoPushbuttonGroup, WPInfoAllPushbutton+displayMode);
	
	/*
	 * note the call to SetupListLines!
	 *
	 */

	GUIListSetDrawFunction(WPInfoListbox, DrawOneLine);
	GUIListSetNumItems(WPInfoListbox, SetupListLines());
	GUIListSetSelection(WPInfoListbox,noListSelection);

	LOGLINE;
	
	StrPrintF(tmp, "%s  (%d%s)  %s",infoWp->ident,
			(Int16)(infoWp->elevation * UC.altitudeConv), UC.altitudeUnits,
			GetStringFromList(infoWp->ident,1));

	GUIFieldSetText(WPInfoDescField, tmp, false);

	if (!GPSState) GUIObjectHide(WPGotoButton);
	
	LOGLINE;
}

/*
 * function : SetupListLines
 *
 * Creates a list of pointers to the start of each of the lines that will be
 * displayed in the list box, according to the displayMode.
 *
 * For displayMode = all, every line of the waypoint info text is a line in the
 * list box, whereas for displayMode = frequencies or runways we have to filter out
 * the corresponding lines from the text.
 *
 * Sets up linesPtr, a pointer to an array of char * pointers, the memory must be
 * deallocated by the caller.
 *
 * Returns the number of lines in the list
 *
 * Format of strings for runways:
 * 
 * 9chars | Runways:\n
 * 27chars| Runway   LxW       Surface\n
 * 23chars| 05/23    6437x148  ASP\n
 * 23chars| 09L/27R  12742x164 GRS\n
 *
 * (where \n is char 0x0a-linefeed)
 *
 * Each record is 9 chars RwyId, 10 chars dimensions, 3 chars surface
 * and an eol marker.
 *
 *
 * Format of strings for frequencies:
 *
 * 13chars | Frequencies:\n
 * 21chars | Type Freq.      Name\n
 * ??chars | <5>  <11>       <var>\n
 * ??chars | TWR  118.9      Tower\n
 *
 */
				
static UInt16 SetupListLines(void) {
	UInt16 count = 0;
	const char   *ptr = GetStringFromList(infoWp->ident,2);
	Boolean stopAtBlank = false;

	const char frequenciesText[] = "Frequencies:\n";
	const char rwyText[]         = "Runways:\n";
	const char ilsText[]         = "ILS:\n";

	LOGLINE;

	ModErrThrowIf(!linePtrs);

	/*
	 * for frequency/runway mode, look for "Frequencies:" or "Runways:"
	 *
	 */

	if (displayMode != all) {

		switch (displayMode) {

		case frequencies:
			ptr = StrStr(ptr, frequenciesText);
			break;

		case runways:
			ptr = StrStr(ptr, rwyText);
			break;

		case ils:
			ptr = StrStr(ptr, ilsText);
			break;
			
		default:
			break;
			
		}

		if (!ptr) return 0;

		/*
		 * skip over the record headers
		 *
		 */

		SKIP_LINE(ptr);
		if (!ptr) return 0;
		SKIP_LINE(ptr);
		if (!ptr) return 0;

		stopAtBlank = true;

	}

	LOGLINE;

	while (count < MAX_LINES && ptr && !(stopAtBlank && *ptr == '\n')) {

		LOGSTR(ptr);
		linePtrs[count++] = ptr;
		SKIP_LINE(ptr);

	}

	LOGLINE;

	return count;
}


/*
 * function : FormDeinit
 *
 */

static void FormDeinit(void) {

	GUIListClearDrawFunction(WPInfoListbox);
	
	PFMemFree(linePtrs);
	PFMemFree(infoWp);

}

/*
 * function : DrawOneLine
 *
 */

static void DrawOneLine(Int16 itemNum, PFScreenRectType *bounds) {

	const char *eol=linePtrs[itemNum];

	/*
	 * width in chars of the fields of the frequency and runway information
	 * strings. Note the use of 99, this effectively allows the field to be
	 * variable length (see code below, which prevents the overflow past
	 * the end of the \n for the line).
	 *
	 */

	const Int16 columns[3][3] = { {0,27,67}, {0,47,112}, {0,33,60} };

	/*
	 * anything to draw?
	 *
	 */

	while (*eol != '\n' && *eol != '\0') eol++;

	if (eol) {

		if (displayMode == all) {

			PFDrawCharsTruncated(linePtrs[itemNum],eol-linePtrs[itemNum],
				bounds->x1, bounds->y1, PFGetRectangleWidth(*bounds));

		} else {

			/*
			 * there are 3 fields in runways, ils and frequencies
			 * so this code applies to all three
			 *
			 */

			Int16 j;
			const char  *fieldStart = linePtrs[itemNum];
			const Int16 *column;
			const char *eol = StrStr(fieldStart, "\n");

			column = columns[displayMode - 1];

			for (j=0; fieldStart && j<3 && fieldStart < eol; j++) {

				Int16 len;
				const char  *ws = fieldStart;
				Coord x;

				if (j < 2) {

					SKIP_TO_WHITESPACE(ws);

				} else {

					ws = eol;

				}

				if (!ws) break;

				len = (Int16)(ws-fieldStart);

				x = bounds->x1 + column[j];
				PFDrawCharsTruncated(fieldStart, len, x, bounds->y1, PFGetRectangleWidth(*bounds) - x);

				fieldStart = ws;
				SKIP_WHITESPACE(fieldStart);

			}

		}

	}

}


/*******************************************************************************
 *
 * public functions
 *
 */

/*
 * function : WPInfoDialogHandleEvent
 *
 */

Boolean WPInfoDialogHandleEvent(EventPtr event)
{
	Boolean handled = false;

	switch (PFEventGetType(event)) 
	{
	case frmOpenEvent:
		FormInit();
		GUIFormResize(false, false);
		GUIFormDraw();
		handled = true;
		break;

	case winDisplayChangedEvent:
		if (GUIFormResize(false, false)) GUIFormDraw();
		handled = true;
		break;

	case ctlSelectEvent:

		switch (PFEventGetControlSelectID(event)) {
		case WPOKButton:
			FormDeinit();
			GUIFormReturn();
			handled = true;
			break;

		case WPInfoAllPushbutton:
			displayMode = all;
			break;

		case WPInfoFreqPushbutton:
			displayMode = frequencies;
			break;

		case WPInfoRwyPushbutton:
			displayMode = runways;
			break;

		case WPInfoILSPushbutton:
			displayMode = ils;
			break;

		case WPPanMapButton:
			MapSetSelection(waypointID);
			FormDeinit();
			GUIFormReturn();
			GUIFormGoto(MapForm);
			handled = true;		// set here to stop GUIListSetNumItems below
			break;

		case WPGotoButton:
			if (!GPSState) {
				handled = true;
				break;
			}
			
			FpStackPush(FlightPlanStack, FlightPlan);
			if (WDMGetWaypointType(infoWp) & wpAllAirfields) {
				
				FpInit(FlightPlan);
				
			}
				
			FpSetNewFirstLeg(FlightPlan, GPS.posn.lat32, GPS.posn.lon32, "(gps)", GPS.posn.magVarn,
					RAD_TO_INT32(infoWp->latitude), RAD_TO_INT32(infoWp->longitude), 
					infoWp->ident, infoWp->magVar);
			FormDeinit();
			GUIFormReturn();
			GUIFormGoto(MapForm);
			handled = true;
			break;

		default:
			break;
		}

		if (!handled) {
			 
			GUIListSetNumItems(WPInfoListbox,SetupListLines());
			GUIListDraw(WPInfoListbox);

			handled = true;

		}

		break;

	case keyDownEvent:

		switch (PFEventGetKeyChr(event)) {
		case vchrPageUp:
			GUIListScroll(WPInfoListbox, -GUIListGetNumVisibleLines(WPInfoListbox));
			break;
		case vchrPageDown:
			GUIListScroll(WPInfoListbox, GUIListGetNumVisibleLines(WPInfoListbox));
			break;
		}
		handled = true;
		break;

	case lstSelectEvent:
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

/*
 * function : WPInfoSetWaypointInfo
 *
 */

void WPInfoSetWaypointInfo(WaypointIDType wpID) {

	waypointID = wpID;

	if (wpID != wpNotFound) {

		infoWp = WDMGetWaypoint(WPDataset, wpID);

	} else {

		infoWp = WDMNewWaypoint("_N/A","_No information available", 0,0,0.0);

	}

}
