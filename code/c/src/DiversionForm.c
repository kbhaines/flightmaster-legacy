/******************************************************************************
 *
 * DiversionForm.c
 *
 * (c) Blackhawk Systems Ltd 2003.
 */

#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif
#include "Platform.h"
#include <TextMgr.h>
#include "DiversionForm.h"
#include "ResourceDefines.h"
#include "Utils.h"
#include "GlobalTypes.h"
#include "CpInterface.h"
#include "DiversionMgr.h"
#include "MathLib.h"
#include "AvCalcs.h"
#include "WPInfoDialog.h"
#include "Modules.h"

#include "AlphaPadDialog.h"
#include "Instruments.h"
#include "FMPreferences.h"
#include "HeadingIndicator.h"

/**************************************************************************
 *
 * global variables
 *
 */

extern GPSType         GPS;
extern Boolean             GPSState;
extern DiversionType       DiversionState;
extern FMPreferencesType   Preferences;
extern IconWindowsType     IconWindows;
extern const char          *CompassText[];

extern const WDMHandle     WPDataset;
extern const FMDataset     FMDS;
extern const UserConversionType UC;
extern const SatConstType SatConst;
extern const HSIMiniPanelType HSIMiniPanel;

/**************************************************************************
 *
 * module variables
 *
 */ 

#define ModuleID DiversionFormModuleID

static /*@only@*/ /*@null@*/ WDMSearchHandle search = NULL;

static Boolean identOnly = true;
static char searchString[10] = "";

/*
 * Proximity waypoint type and Proximity waypoint list types
 *
 */

#define MAX_PROXWAYPOINTS 16
#define MAX_IDENTCHARS    10
#define MAX_NAMECHARS     40

typedef struct {

	WaypointIDType    id;
	WaypointClassType type;

	char           ident[MAX_IDENTCHARS+1];
	char           name[MAX_NAMECHARS+1];
	double         range;
	Int16          bearing;
	Int16          bearingFrom;

	/*
	 * only used for runways, indicates which of the runway icons to draw
	 *
	 */

	UInt16         runwayIcon;

} ProxWaypointType;

typedef struct {

	Int16 numWaypoints;
	ProxWaypointType waypoints[MAX_PROXWAYPOINTS];
	
} ProxWaypointListType;

/*
 * Proximity list storage
 * 
 * If newProxList points to proxList, then we're in a process of building the
 * proxList for the first time (be it just after entering the form, or after a
 * search parameter change). While this holds true, we need to update the
 * display after every call to ScanForWaypoints, so that the user gets the
 * impression that something is actually happening!
 *
 * When newProxList != proxList, we've built the first list in proxList, and
 * can display that. We don't need to update the display after calling
 * ScanForWaypoints from then on.
 * 
 */

static ProxWaypointListType proxList;
static ProxWaypointListType *newProxList;

static WaypointClassType searchMask;

/*
 * area in which the proximity waypoints' information
 * will be displayed, and the number of proximity waypoints
 * to display within that area
 *
 */

static const PFScreenRectType proxDisplayArea = 
	{ DvItemListLeft, DvItemListTop,
			DvItemListLeft + DvItemWidth,DvItemListTop + DvItemHeight*4};
		
static const UInt16 numProxWaypointsToDisplay = 4;
static const UInt16 proxRowHeight = DvItemHeight;
static Coord iconOffset = 0;

/*
 * special value for selectedWaypointID, below, for when there is no
 * entry highlighted in the list
 *
 */

#define UNSELECTED 0xFFFFFFFF

/*
 * corresponds to the setting of the filter pushbutton
 * control
 *
 */

static enum { airfield = 0,vor,ndb,intersection,all } displayFilter = airfield;

/*
 * Currently selected list index number
 *
 */

static Int16 currentSelection = noListSelection;
static Int16 topOfList = 0;

/*
 * the unique ID of the waypoint that is selected
 * in the waypoint list
 *
 */

static WaypointIDType selectedWaypointID = UNSELECTED;


/*
 * state machine variable, controlled by ResetScan, ScanForWaypoints
 * and DiversionFormHandleEvent
 * 
 */

static enum { disabled, starting, scanning, waiting } scanState = disabled;


/*************************************************************************
 *
 * private functions
 *
 * Are declared here in order to allow the code to be placed into
 * alternate code sections
 *
 */

static void ResetScan(void) DIVERSION_SECTION;
static Boolean ScanForWaypoints(UInt16 maxTicks, Boolean stopOnEvent) DIVERSION_SECTION;
static void DrawWaypoint(UInt16 itemNum)  DIVERSION_SECTION;
static void UpdateDisplay(void) DIVERSION_SECTION;
static void DiversionFormInit(void) DIVERSION_SECTION;
static void DiversionFormDeInit(void) DIVERSION_SECTION;
static Boolean HandleCtlSelectEvent(EventPtr e) DIVERSION_SECTION;
static Boolean HandleProxSelectEvent(EventPtr e) DIVERSION_SECTION;
static Boolean HandleKeyEvent(EventPtr e) DIVERSION_SECTION;
static void SetupScrollbar(void) DIVERSION_SECTION;

/*
 * function : ResetScan
 *
 * Initialises the scanning process.
 *
 * Sets up the scanner, searchMask, scanState status and gives 1/2 a second to
 * ScanForWaypoints to get the prox list going.
 *
 */

static void ResetScan(void) {

	LOGENTRY;

	proxList.numWaypoints = 0;
	currentSelection = noListSelection;
	selectedWaypointID = UNSELECTED;

	switch (displayFilter) {

	case airfield: searchMask = wpAllAirfields;
		    break;
	case vor:   searchMask = wpVOR;
		    break;
	case ndb:   searchMask = wpNDB;
		    break;
	case intersection: searchMask = wpIntersection;
			break;
	case all: searchMask = wpAny;
		    break;

	}

	/*
	 * if newProxList points to it's own storage, then we should
	 * deallocate and point newProxList to ProxList.
	 *
	 */

	if (newProxList != &proxList) {

		PFMemFree(newProxList);
		newProxList = &proxList;

	}

	if (search) WDMFreeSearch(search);
	search = NULL;

	topOfList = 0;
	scanState = starting;

	StrCopy(searchString, GUIFieldGetText(DvSearchStr));

	if (!GPSState || (GPSState && GPS.sat.fixType>1)) {

		/*
		 * if scan completed and no prox-waypoints, may as well disable
		 * scanning
		 *
		 */

		if (ScanForWaypoints(PFTicksPerSecond()/4,false) && proxList.numWaypoints == 0) {

			scanState = disabled;

		} 

		LOGINT16(proxList.numWaypoints);

	} else {

		scanState = disabled;

	}

	LOGEXIT;

}

/*
 * function : ScanForWaypoints
 *
 * Scans for proximity waypoints. maxTicks specifies how long we're allowed
 * to run for. If stopOnEvent is true then an event in the PalmOS event
 * queue can interrupt the scan.
 *
 * Returns true if the scan reached the end of its cycle
 *
 */
static Boolean ScanForWaypoints(UInt16 maxTicks, Boolean stopOnEvent) {

	UInt32                startTime = PFGetTicks();
	Waypoint             *wp;
	double                wpRange;
	double                wpBearing, wpBearingFrom;
	double         		maxRange;
	UInt16                j;
	UInt16                runwayIcon = 0;
	static UInt32         lastScanTime;
	Int16                 numScanned = 0;
	ShortWaypointType     *swp;
	
	/*
	 * scan state machine controls what we should do:
	 *
	 * scanning - obvious really!
	 *
	 * waiting  - waiting for 8 seconds between scans
	 *
	 * starting - ready to start a new scan
	 *
	 */

	LOGENTRY;

	if (scanState == waiting) {

		if (PFTimerHasExpired(lastScanTime, PFTicksPerSecond()*8)) {

			scanState = starting;

		} 

	}

	if (scanState == starting) {

		newProxList->numWaypoints = 0;

		LOGTAG("Scan starting");

		search = WDMInitProxScan(WPDataset, GPS.posn.lat32,
				GPS.posn.lon32, FMDS.noObstacles, searchMask);

		/*
		 * No waypoints to search
		 *
		 */

		if (!search) {
			
			scanState = waiting;
			lastScanTime=PFGetTicks();
			LOGEXIT;
			return true;

		}

		scanState = scanning;

	}
		
	if (scanState != scanning) {
		
		LOGEXIT;
		return false;

	}

	/*
	 * determine how far out we should look from current position. If all
	 * of the prox lists are full then the max we should look is not beyond
	 * the furthest waypoint, otherwise we default to 3000 nm.
	 *
	 */

	if (newProxList->numWaypoints == MAX_PROXWAYPOINTS) {

		maxRange = newProxList->waypoints[MAX_PROXWAYPOINTS-1].range;

	} else {
		
		if (searchString[0] == 0) {

			maxRange = 3000.0/NM_PER_RADIAN;

		} else {

			/*
			 * search string specified - search entire database
			 *
			 */

			maxRange = 0.0;

		}

	}

	do {

		//LOGINT16((Int16) (maxRange*NM_PER_RADIAN));
		//LOGINT32(RAD_TO_INT32(maxRange));

		swp = WDMGetProxWaypoint(search, RAD_TO_INT32(maxRange));

		if (swp == NULL) {

			/*
			 * end of scanning cycle
			 *
			 */

			LOGTAG("EOScan");
			LOGINT16(numScanned);

			WDMFreeSearch(search);
			search = NULL;

			if (newProxList == &proxList) {

				newProxList = PFMalloc(sizeof(*newProxList));

			} else {

				PFMemMove(&proxList, newProxList, sizeof(*newProxList));

			}

			scanState = waiting;
			lastScanTime=PFGetTicks();

			LOGEXIT;
			return true;

		}

		numScanned++;

		//LOGTAG("Considering:");
		//LOGSTR(wp->ident);

		/*
		 * discard based on ident-search
		 *
		 */

		if (searchString[0] && identOnly) {

			char ident[6];

			StrNCopy(ident, swp->extra, sizeof(ident)-1);
			ident[sizeof(ident)-1] = 0;

			if (StrNCaselessCompare(ident, searchString, StrLen(searchString)) != 0) {

				continue;

			}

		}

		wp = WDMGetWaypoint(WPDataset, swp->wpId);

		/*
		 * discard based on search string
		 *
		 */

		if (searchString[0]) {
			
			char s[20], d[100];
			char *subStr;

			StrToLower(d, wp->ident);
			StrToLower(s, searchString);

			subStr = StrStr(d,s);
/*
			if (identOnly && subStr != d) {

				PFMemFree((void*)wp);
				continue;

			}
			*/

			if (!subStr) {

				StrToLower(d, GetStringFromList(wp->ident,1));

				if (!StrStr(d,s)) {

					PFMemFree(wp);
					continue;

				}

			}

		}

		/*
		 * runway surface and dimension checks - discard any airfield
		 * that doesn't meet these criteria
		 *
		 * NB We don't discard a waypoint based on runway dimensions
		 * if a search string is specified.
		 *
		 */

		if ( WDMGetWaypointType(wp) & wpAllAirfields ) {

			CpRunwayInfoType *runways;
			UInt16            numRunways;
			Boolean           found = false;

			runways = CpGetRunwayInfo(GetStringFromList(wp->ident,2),&numRunways);
			if (runways) {

				UInt16 k;

				/*
				 * there are 8 icons for the runways, to represent 180
				 * degrees, which gives 22.5 degrees of coverage per
				 * icon. Quadrupling everything allows us to use whole
				 * numbers for the calculation.
				 *
				 */

				runwayIcon = ((runways[0].heading*10 + 5)*4 + 45) / 90;
				WRAPMAX(runwayIcon, 7);

				for (k=0;k<numRunways && runways[k].length >= Preferences.runwayLength;k++) {

					/*
					 * if this runway's width and surface are appropriate, then
					 * we'll use it's heading to determine which icon to
					 * display
					 *
					 */

					if (runways[k].width >= Preferences.runwayWidth &&
					  ( Preferences.runwaySurface == surfEither ||
					  ((Preferences.runwaySurface == surfHard && runways[k].hardSurface) || 
					  (Preferences.runwaySurface == surfGrass && !runways[k].hardSurface)))) {

						runwayIcon = ((runways[0].heading*10 + 5)*4 + 45) / 90;
						WRAPMAX(runwayIcon, 7);
						found = true;
						break;

					}

				}

				PFMemFree(runways);

			} else {

				runwayIcon = 8;

				if (Preferences.runwayWidth == 0 && Preferences.runwayLength == 0) found = true;

			}

			 /*
			  * NB We don't discard a waypoint based on runway dimensions if a
			  * search string is specified.
			  *
			  */
		
			if (!found && !searchString[0] && displayFilter != all) { 

				/*
				 * discard 
				 *
				 */

				PFMemFree(wp);
				continue;

			}

		}

		wpBearingFrom = AvCalcGreatCircleCourse(wp->latitude, wp->longitude,
				GPS.posn.latitude, GPS.posn.longitude, &wpRange); 

		wpBearing = AvCalcGreatCircleCourse(GPS.posn.latitude, GPS.posn.longitude,
				wp->latitude, wp->longitude,
				&wpRange);

		/*
		 * if we're using magnetic headings, convert the bearing
		 * to use local magnetic variation
		 *
		 */

		if (Preferences.useMagnetic) {

			wpBearing += DEG_TO_RAD((double)GPS.posn.magVarn);
			wpBearingFrom += wp->magVar;

			WRAPMAX(wpBearing,2*PI);
			WRAPMAX(wpBearingFrom,2*PI);

		}

		/*
		 * look for the insertion position if necessary
		 *
		 */

		if (newProxList->numWaypoints > 0) {

			/*
			 * it's a good idea to look backwards through this
			 * list, as waypoints are most likely to be added
			 * at the end
			 *
			 */

			for (j=newProxList->numWaypoints;j>0 && wpRange < newProxList->waypoints[j-1].range;j--);
		
			/*
			 * if this triggers then we haven't found a place to
			 * insert the waypoint - its range is beyond that of
			 * the last waypoint in the list
			 *
			 */

			if(j==newProxList->numWaypoints &&
				newProxList->numWaypoints==MAX_PROXWAYPOINTS) {

				PFMemFree(wp);
				continue;

			}

		} else {

			j = 0;

		}

		/*
		 * j is pointing to the insertion position 
		 *
		 * Don't bother shuffling other records if we're adding to the
		 * end of the list
		 *
		 */

		if (j<newProxList->numWaypoints) {

			/*
			 * move the waypoints down the list , set the change
			 * flag to mark them as having been updated.
			 *
			 */

			PFMemMove(&newProxList->waypoints[j+1], &newProxList->waypoints[j], 
					sizeof(ProxWaypointType)*(MAX_PROXWAYPOINTS-1-j));

		} 

		LOGTAG("Adding:");
		LOGSTR(wp->ident);
		newProxList->waypoints[j].id=swp->wpId;
		newProxList->waypoints[j].type=WDMGetWaypointType(wp);
		newProxList->waypoints[j].range=wpRange;
		newProxList->waypoints[j].bearing=(Int16) round(RAD_TO_DEG(wpBearing));
		newProxList->waypoints[j].bearingFrom=(Int16) round(RAD_TO_DEG(wpBearingFrom));
		newProxList->waypoints[j].runwayIcon = runwayIcon;
		StrCopy(newProxList->waypoints[j].ident,wp->ident);
		StrNCopy(newProxList->waypoints[j].name, GetStringFromList(wp->ident,1), MAX_NAMECHARS-1);
		newProxList->waypoints[j].name[MAX_NAMECHARS-1] = 0;

		if (newProxList->numWaypoints<MAX_PROXWAYPOINTS) {

			/*
			 * we haven't filled the list, so this waypoint *must*
			 * be a new entry in the list.
			 *
			 */

			newProxList->numWaypoints++;

		}
		
		PFMemFree(wp);

	} while (!(stopOnEvent && PFEventAvailable()) && !PFTimerHasExpired(startTime, maxTicks));

	LOGINT16(numScanned);

	LOGEXIT;

	return false;

}


/*
 * function : DrawWaypoint
 *
 * Draws the specified waypoint, where listNum is 0-3 representing the screen
 * position to draw. The actual waypoint is topOfList+listNum.
 *
 */

static void DrawWaypoint(UInt16 listNum) {

	PFScreenRectType     tmpRect;
	PFScreenRectType     bounds;
	UInt16            iconIndex  = 0;
	Waypoint         *proxWp;

	Coord 		  right;
	Coord         iconDim = IconWindows.iconDim;
	Coord         y;

	double range;
	char                str[40];

	PFScreenRectangleSet(&bounds, proxDisplayArea.x1, 
			(listNum*proxRowHeight)+proxDisplayArea.y1,
			proxDisplayArea.x2, proxDisplayArea.y1 + (listNum+1)*proxRowHeight);

	PFDrawStatePush();

	PFEraseRectangle(&bounds,0);

	PFSetTextColour(GUIGetSystemColour(GUIObjectForeground));
	PFSetBackColour(GUIGetSystemColour(GUIFormFill));

	listNum += topOfList;

	if (listNum >= proxList.numWaypoints) {

		PFDrawStatePop();
		return;

	}

	/*
	 * draw the icon
	 *
	 * the icons are stored in a strip in a single bitmap resource,
	 * we need to copy a small portion of the strip to get the
	 * icon we want
	 *
	 */

	proxWp = WDMGetWaypoint(WPDataset, proxList.waypoints[listNum].id);

	if (WDMGetWaypointType(proxWp) & wpAllAirfields) {

		iconIndex = proxList.waypoints[listNum].runwayIcon;
		
	} else {
		
		switch (proxList.waypoints[listNum].type) {

		case wpVOR:
			iconIndex = 9;
			break;
		case wpVORDME:
			iconIndex = 10;
			break;

		case wpVORTAC:
			iconIndex = 13;
			break;

		case wpDME:
			iconIndex = 11;
			break;

		case wpNDBDME:
		case wpNDB:
			iconIndex = 12;
			break;

		case wpObstacle:
			iconIndex = 15;
			break;

		case wpLightObstacle:
			iconIndex = 16;
			break;

		case wpObstacles:
			iconIndex = 17;
			break;

		case wpLightObstacles:
			iconIndex = 18;
			break;

		case wpIntersection:
			iconIndex = 14;
			break;

		case wpTown:
			iconIndex = 20;
			break;

		case wpVRP:
			iconIndex = 22;
			break;

		case wpDisused:
			iconIndex = 21;
			break;

		case wpMicrolight:
			iconIndex = 24;
			break;

		case wpGlider:
			iconIndex = 25;
			break;

		case wpParachute:
			iconIndex = 26;
			break;

		default:
			iconIndex = 23;
			break;

		}
			
	} 
		
	PFScreenRectangleSetRel(&tmpRect,(iconIndex & 0x7) * iconDim,
			((iconIndex & 0xfff8) >> 3) * iconDim,
			iconDim, iconDim);

	PFCopyRectangle(IconWindows.icons, &tmpRect, 
			bounds.x1+iconOffset,
			bounds.y1+(proxRowHeight-iconDim)/2,winOverlay);

	/*
	 * draw first line:
	 *
	 */

	y = bounds.y1;

	FntSetFont(stdFont);

	StrPrintF(str,"%03d",proxList.waypoints[listNum].bearing);
	right = 4+DrawLargeNumberSmallText(str, UC.heading, 
			bounds.x1+iconDim+3, y, ALIGNLEFT,largeBoldFont);

	FntSetFont(boldFont);
	DrawAlignedChars(proxList.waypoints[listNum].ident, ALIGNLEFT,
			bounds.x1+iconDim+3+right, y);

	/*
	 * 2nd+3d line:
	 *
	 */

	y = bounds.y1+(proxRowHeight/2);
	range = proxList.waypoints[listNum].range * UC.distanceConv;
	right = 4 + DrawLargeNumberSmallText( 
			range<100?DoubleToStr(range,1):DoubleToStr(range,0), 
			UC.distanceUnits, bounds.x1+iconDim+2, 
			y, ALIGNLEFT,largeBoldFont);

	StrPrintF(str,"%03d\260 (%s)",proxList.waypoints[listNum].bearingFrom, 
			CompassText[(((proxList.waypoints[listNum].bearingFrom)*4+45)%1440)/90]);

	FntSetFont(boldFont);
	y-=12;
	right+=bounds.x1+iconDim+2;
	
	DrawAlignedChars(str, ALIGNLEFT, right, y);

	y+=pfScreen.boldHeight;
	FntSetFont(stdFont);
	PFDrawCharsTruncated(proxList.waypoints[listNum].name, 
			StrLen(proxList.waypoints[listNum].name), right, y, 
			(bounds.x2)-right);

	y = bounds.y2 - 1;

	PFDrawLine(bounds.x1, y, bounds.x2, y);

	PFDrawStatePop();

	PFMemFree(proxWp);

}

/*
 * function : UpdateDisplay
 *
 */

static void UpdateDisplay(void) {

	UInt16 j;
	const char timer[] = "\024";

	PFScreenLock(false);
	
	GUIFormDraw();
	/*
	 * make sure that the selected waypoint is kept high-lighted i.e.
	 * move the list selector if the waypoint moves position in the list
	 *
	 */


	if (selectedWaypointID != UNSELECTED && proxList.waypoints[currentSelection].id != selectedWaypointID) {

		/*
		 * look for the waypoint ID in the list
		 *
		 */

		for (j=0;j<proxList.numWaypoints;j++) {
			if (proxList.waypoints[j].id == selectedWaypointID) {
				currentSelection = j;
				break;
			}
		}

		/*
		 * didn't find it? Clear the list selection
		 *
		 */

		if (j == proxList.numWaypoints) {
			currentSelection = noListSelection;
			selectedWaypointID = UNSELECTED;
		}
	}
	if (GUIMenuIsDisplayed()) return;

	SetupScrollbar();

	for (j=0;j<numProxWaypointsToDisplay;j++) {

		DrawWaypoint(j);

	}
	
	if (newProxList == &proxList) {
		
		FntSetFont(symbolFont);
		PFDrawChars(timer,1, StandardPageWidth-20,0);

	} else {

		FntSetFont(stdFont);
		PFDrawChars("    ",4, StandardPageWidth-20,0);
		
	}
	
	if (HSIMiniPanel) HSIMiniPanelDraw(HSIMiniPanel);

	PFScreenUnlock();


#ifdef ALTERNATE

	if (newProxList == &proxList) {

		PFScreenRectType r;
		FormPtr       f = FrmGetActiveForm();

		FrmGetObjectBounds(f, FrmGetObjectIndex(f, displayFilter + DvAirfieldPushbutton),
				&r);

		DrawAlignedChars(timer, ALIGNCENTRE, r.x1+r.extent.x/2,
				r.y1+r.extent.y/2 - FntLineHeight()/2);

	} else {

		CtlDrawControl(GetObjectPtr(FrmGetActiveForm(), displayFilter+DvAirfieldPushbutton));

	}

#endif
	PFSendSimpleEvent(evtScreenRedrawn);

}

/*
 * function : DiversionFormInit
 *
 */

static void DiversionFormInit(void) {

	LOGENTRY;

	/*
	 * Set the display filter pushbutton group 
	 *
	 */

	GUIObjectGroupSetValue(DvPushbuttonGroup,(UInt16)displayFilter+DvAirfieldPushbutton);

	selectedWaypointID = UNSELECTED;

	GUIFieldSetText(DvSearchStr, searchString, false);
	
	// TODO - investigate focus bug
	
//	GUIFocusSet( DvSearchStr);

	GUIObjectSetValue(DvIdentCheckbox, (Int16)identOnly);

	/*
	 * initialise proximity list
	 *
	 */

	newProxList = &proxList;
	ResetScan();

	LOGEXIT;

}

/* 
 * function : DiversionFormDeInit
 *
 * Deinitialises the form as it closes
 *
 */
static void DiversionFormDeInit(void) 
{

	if (newProxList != &proxList)
		PFMemFree(newProxList);

	if (search) WDMFreeSearch(search);
	search = NULL;

	StrCopy(searchString, GUIFieldGetText(DvSearchStr));

}


/* 
 * function : HandleCtlSelectEvent
 *
 * Returns true if the event was handled by the function
 *
 */

static Boolean HandleCtlSelectEvent(EventPtr event) {
	Boolean handled = false;
	
	switch (PFEventGetControlSelectID(event)) {
	
	case DvAirfieldPushbutton:
	case DvVORPushbutton:
	case DvNDBPushbutton:
	case DvIntPushbutton:
	case DvAnyPushbutton:

		/*
		 * new display filter setting
		 *
		 * Setup a new list of waypoints
		 *
		 */

		displayFilter = PFEventGetControlSelectID(event) - DvAirfieldPushbutton;

		currentSelection = noListSelection;
		selectedWaypointID = UNSELECTED;

		ResetScan();
		UpdateDisplay();

		handled = true;
		break;
		

	case DvIdentCheckbox:

		identOnly = GUIObjectGetValue(DvIdentCheckbox);
		ResetScan();
		UpdateDisplay();
		handled = true;
		break;

	}

	return handled;
}


/*
 * function : HandleProxSelectEvent
 *
 */

static Boolean HandleProxSelectEvent(EventPtr event) {
	
	UInt16 newSelection = (PFEventGetY(event)*2 - proxDisplayArea.y1) / proxRowHeight + topOfList;

	if (newSelection < proxList.numWaypoints) {

		WPInfoSetWaypointInfo(proxList.waypoints[newSelection].id);

		GUIFormPopup(WPInfoDialog);

	}

	return true;

}

/*
 * function : HandleKeyEvent
 *
 */
static Boolean HandleKeyEvent(EventPtr event) {
	
	Boolean handled = false;

	switch (PFEventGetKeyChr(event)) {
	case vchrPageUp:

		if (topOfList > 0) {

			topOfList = MAX(0, topOfList-4);
			
		} else {

			SndPlaySystemSound(sndError);

		}

		UpdateDisplay();
		handled = true;
		break;

	case vchrPageDown:

		if (topOfList < proxList.numWaypoints-4) {

			topOfList = MIN(topOfList+4, MAX(0,proxList.numWaypoints - 4));

		} else {

			SndPlaySystemSound(sndError);

		}
		UpdateDisplay();
		handled = true;
		break;

	default:
		break;

	}
	return handled;
}


/*
 * function : SetupScrollbar
 *
 * Does what it says ;-)
 *
 */

static void SetupScrollbar(void) {

	GUIScrollBarSet(DvScrollbar, topOfList, 0, MAX(0, proxList.numWaypoints - 4), 4);

}
	


/**************************************************************************
 *
 * public functions
 *
 */

/* 
 * function : DiversionFormHandleEvent
 *
 * Handles all events directed at the form.
 *
 * Returns true if the event is handled
 */
Boolean DiversionFormHandleEvent(EventPtr event)
{
	Boolean handled = false;
	char *alphaInput;

	switch (PFEventGetType(event)) 
	{
	case frmOpenEvent:
		DiversionFormInit();
		GUIFormResize(false, false);
		UpdateDisplay();

		handled = true;
		break;
			
	case winDisplayChangedEvent:

		if (GUIFormResize(false, false)) UpdateDisplay();
		handled = true;
		break;
		

	case evtGPSFix:
	case evtGPSFixLost:
	case evtGPSPositionUpdate:
	case nilEvent:

		/*
		 * turn scanning on/off according to state of GPS
		 *
		 */

		if (GPSState) {

			if ( (scanState != disabled && GPS.sat.fixType < 2) ||
				(scanState == disabled && GPS.sat.fixType > 1)) {

				ResetScan();
				if (!GUIMenuIsDisplayed()) UpdateDisplay();

			} else if (GUIFieldIsDirty(DvSearchStr)) {
				
				LOGTAG("Dirty field");
				GUIFieldClean(DvSearchStr);

				ResetScan();
				if (!GUIMenuIsDisplayed()) UpdateDisplay();

			} else if (scanState != disabled) {

				/*
				 * update the display if the scan is completed or while
				 * we're still searching
				 *
				 */

//				if (ScanForWaypoints(PFTicksPerSecond()*3/4,true) || newProxList == &proxList)

				(void)ScanForWaypoints(PFTicksPerSecond()*3/4,true);
				if (!GUIMenuIsDisplayed()) UpdateDisplay();

			}

		} else {

			/*
			 * if (FpIsBlank(FlightPlan)) {
			 *
			 * GUIObjectHide( MapPanToWaypointButton);
			 *
			 * }
			 *
			 * No GPS on, we're using our reference position
			 *
			 */

			if (ScanForWaypoints(PFTicksPerSecond()*3/4,true) || newProxList == &proxList)

				if (!GUIMenuIsDisplayed()) UpdateDisplay();


		}

		handled = true;
		break;

	case keyDownEvent:
		handled = HandleKeyEvent(event);
		break;

	case menuEvent:
		
		switch (PFEventGetMenuID(event)) {

		case MnDivertEmergency:
			GUIFormGoto(MapForm);
			handled = true;
			break;

		case MnInformation:

			if (selectedWaypointID != UNSELECTED) {

				/*
				 * waypoint is locked here, but will be unlocked by
				 * the WPInfoDialog
				 *
				 */

				WPInfoSetWaypointInfo(selectedWaypointID);
				GUIFormPopup(WPInfoDialog);

			}

			handled = true;
			break;

		}

		break;

	case ctlSelectEvent:
		handled = HandleCtlSelectEvent(event);
		break;

	case sclEnterEvent:
	case sclRepeatEvent:
	case sclExitEvent:
		handled = true;
		break;

	case penDownEvent:
		if (PFScreenPointInRectangle(PFEventGetX(event)*2, PFEventGetY(event)*2,
					&proxDisplayArea)) {
			handled = HandleProxSelectEvent(event);
		} 
		break;

	case fldEnterEvent:
		AlphaPadDialogInit(GUIFieldGetText(DvSearchStr));
		GUIFormPopup(AlphaPadDialog);
		handled = true;
		break;
		
	case evtAlphaInput:
		alphaInput = AlphaPadGetInput();
		GUIFieldSetText(DvSearchStr, alphaInput, true);
		PFMemFree(alphaInput);

		ResetScan();
		UpdateDisplay();
		handled = true;
		break;

	case frmCloseEvent:
		DiversionFormDeInit();
		handled = false;
		break;

	case evtWaypointInfoReq:
		
		/*
		 * this is sent by the command popup dialog
		 *
		 */


	default:
		break;
	}	
	return handled;
}

