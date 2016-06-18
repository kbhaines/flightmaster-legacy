/*
 *
 * MapIcon.c
 *
 * ADT about a map icon
 *
 */

#include "Platform.h"
#include "MapIcon.h"
#include "GlobalTypes.h"
#include "Utils.h"
#include "MathLib.h"
#include "WDManager.h"
#include "CpInterface.h"
#include "Modules.h"

#include "FMPreferences.h"

#define ModuleID IconModuleID

/*****************************************************************************
 *
 * global variables
 *
 */

extern FMPreferencesType Preferences;
extern AppColourPrefsType AppColourPrefs;
extern WDMHandle WPDataset;

extern const UserConversionType UC;

/*****************************************************************************
 *
 * module variables
 *
 */

/*
 * shortcut to map settings in Preferences...
 *
 */

#define mapPrefs Preferences.mapSetting[Preferences.mapNumber]
#define mapPrefsNumber Preferences.mapNumber
#define mapPrefsScale Preferences.mapSetting[Preferences.mapNumber].scale

/*
 * sin and cosine plots, from 0 to 45 degrees in increments of 5 degrees (i.e.
 * 10 values)
 *
 */

#define ATZ_POINTS 10
#define ATZ_STEP   5

static Int32 runwayLength, runwayLength9;
static Int16 atzSinPlot[ATZ_POINTS];
static Int16 atzCosPlot[ATZ_POINTS];

static Boolean drawMask = true;

/*****************************************************************************
 *
 * private functions
 *
 */

static void AtzPlotInit(Coord radius) MAP_SECTION;
static void DrawRunways(MapIconType *i, Waypoint *wp, Coord ix, Coord iy, Int16 rotation) MAPICON_SECTION;

/*******************************************************************************/
/*
 * function : AtzPlotInit
 *
 * Sets up the atzSinPlot and atzCosPlot arrays according
 * to the settings in mapPrefsScale.
 *
 */

static void AtzPlotInit(Coord radius) {

	UInt16 j;
	Int16  angle = 0;

	LOGINT16(radius);

	for (j=0;j<ATZ_POINTS;j++) {

		Int32 s,c;

		GetSinCos(angle, &s, &c);

		LOGINT16(s);

		atzSinPlot[j] = (Int16) (( s * (Int32)radius) / COSINEZERO); 
		atzCosPlot[j] = (Int16) (( c * (Int32)radius) / COSINEZERO);

		LOGINT16(atzSinPlot[j]);

		angle += ATZ_STEP;

	}

}

/*
 * function : DrawRunways
 *
 * Draws the runways of a selected airfield
 *
 */

static void DrawRunways(MapIconType *i, Waypoint *wp, Coord ix, Coord iy, Int16 rotation) {

	UInt16 j;
	Int16 hdg;
	Int16 angle;
	Int16 x1,y1;
	Int16 x2,y2;
	Int16 x3,y3;
	Int16 lx, ly;
	char tmp[30];
	UInt16 len;
	Int32 sn, cs;
	Int16 numRunways;
	Int16 magVar = (Int16) round((double)wp->magVar*DEG_PER_RAD);
	CpRunwayInfoType *runways = CpGetRunwayInfo(GetStringFromList(wp->ident,2), &numRunways);

	for (j=0;j<numRunways;j++) {

		LineType wl[6];

		hdg = runways[j].heading;
		if (hdg > 17) hdg -= 18;

		angle = rotation + (hdg*10 + magVar) - 5;
		WRAPMAX(angle,360);

		GetSinCos(angle, &sn, &cs);
		x1 = (Int16) (runwayLength*sn/COSINEZERO);
		y1 = (Int16) (runwayLength*cs/COSINEZERO);

		angle +=5; WRAPMAX(angle, 360);
		GetSinCos(angle, &sn, &cs);
		x2 = (Int16) (runwayLength9*sn/COSINEZERO);
		y2 = (Int16) (runwayLength9*cs/COSINEZERO);

		lx = (x2*16/12);
		ly = (y2*16/12);
		
		angle +=5; WRAPMAX(angle, 360);
		GetSinCos(angle, &sn, &cs);
		x3 = (Int16) (runwayLength*sn/COSINEZERO);
		y3 = (Int16) (runwayLength*cs/COSINEZERO);

		len = StrPrintF(tmp,"%02d",hdg);
		PFPaintChars(tmp,len,ix-lx-FntCharsWidth(tmp,len)/2,iy+ly-FntCharHeight()/2);

		hdg += 18;
		len = StrPrintF(tmp,"%02d",hdg);
		PFPaintChars(tmp,len,ix+lx-FntCharsWidth(tmp,len)/2,iy-ly-FntCharHeight()/2);

		wl[0].x1 = ix+x1; wl[0].y1 = iy-y1; wl[0].x2 = ix-x1; wl[0].y2 = iy+y1;
		wl[1].x1 = ix-x1; wl[1].y1 = iy+y1; wl[1].x2 = ix-x2; wl[1].y2 = iy+y2;
		wl[2].x1 = ix-x2; wl[2].y1 = iy+y2; wl[2].x2 = ix-x3; wl[2].y2 = iy+y3;

		wl[3].x1 = ix-x3; wl[3].y1 = iy+y3; wl[3].x2 = ix+x3; wl[3].y2 = iy-y3;
		wl[4].x1 = ix+x3; wl[4].y1 = iy-y3; wl[4].x2 = ix+x2; wl[4].y2 = iy-y2;
		wl[5].x1 = ix+x2; wl[5].y1 = iy-y2; wl[5].x2 = ix+x1; wl[5].y2 = iy-y1;
		
		PFDrawThickLines(wl, 6, GUIGetSystemColour(GUIObjectForeground), AppColourPrefs.white, 2);

	}

	if (runways) PFMemFree(runways);

}

/*****************************************************************************
 *
 * public functions
 *
 */


/*
 * function : MapIconSetScale
 *
 */

void MapIconSetScale(Int32 scale, Coord atz) {

	runwayLength = RAD_TO_INT32(NM_TO_RAD(8)) / scale;
	runwayLength9= runwayLength * 9 / 10;

	AtzPlotInit(atz);

	drawMask = GUIGetSystemColour(GUIFormFill) ? false : true;
 
}

/*
 * function : MapIconNew
 *
 */

void MapIconNew(MapIconType *ptr, Coord x, Coord y, const ShortWaypointType *wp) {

	LOGLINE;

	ptr->x = x;
	ptr->y = y;
	ptr->id = wp->wpId;
	ptr->type = wp->type;


	switch (wp->type & wpClassMask) {

	case wpAirfield:
	case wpLargeAirfield:
		switch (wp->type) {

		case wpDisused:
			ptr->icon = 21;
			StrNCopy(ptr->label, wp->extra, 5);
			break;

		case wpMicrolight:
			ptr->icon = 24;
			StrNCopy(ptr->label, wp->extra, 5);
			break;

		case wpGlider:
			ptr->icon = 25;
			StrNCopy(ptr->label, wp->extra, 5);
			break;

		default:
			ptr->icon = wp->type & wpSubClassMask;
			break;

		}

		StrNCopy(ptr->label, wp->extra, 5);
		break;

	case wpVOR:
		if (wp->type == wpTACAN) 
			
			ptr->icon = 13;

		else

			ptr->icon = 9 + (wp->type & wpSubClassMask);

		StrNCopy(ptr->label, wp->extra, 5);
		break;

	case wpNDB:
		ptr->icon = 12;
		StrNCopy(ptr->label, wp->extra, 5);
		break;

	case wpAnyObstacle:

		/*
		 * label obstacles by their altitude in hundred's of feet/metres
		 *
		 */

		ptr->icon = 14 + (wp->type & wpSubClassMask);
		ptr->obstacleAltitude = wp->extra[1]*256+wp->extra[2];
		StrPrintF(ptr->label,"%s", 
				FloatToStr((UC.altitudeConv*ptr->obstacleAltitude)/100 ,0));

		break;

	case wpIntersection:
		ptr->icon = 14;
		StrNCopy(ptr->label, wp->extra, 5);
		break;

	case wpOther:
		switch (wp->type) {

		case wpTown:
			ptr->icon = 20;
			StrNCopy(ptr->label, wp->extra, 5);
			break;

		case wpVRP:
			ptr->icon = 22;
			StrNCopy(ptr->label, wp->extra, 5);
			break;

		case wpParachute:
			ptr->icon = 26;
			StrNCopy(ptr->label, wp->extra, 5);
			break;

		case wpMarker:
			ptr->icon = 27;
			StrNCopy(ptr->label, wp->extra, 5);
			break;

		default:
			ptr->icon = 23;
			StrNCopy(ptr->label, wp->extra, 5);
			break;

		}
		break;

	default:
		ptr->icon = 23;
		StrNCopy(ptr->label, wp->extra, 5);
		break;

	}

	ptr->label[5] = 0;
	LOGLINE;

}

/*
 * function : MapIconDraw
 *
 */


void MapIconDraw(const IconWindowsType *iconSet, MapIconType *this, Boolean label, Coord x, Coord y, Int16 rotation) {

#define ADDLINE(xx1,yy1,xx2,yy2) wl[nl].x1 = xx1; wl[nl].y1 = yy1; wl[nl].x2 = xx2; wl[nl++].y2 = yy2;

	PFScreenRectType r;
	UInt16        labelLen;

	if ((this->type & aptFilter) && mapPrefs.showZones && mapPrefsScale < 8) {

		UInt16 j;
		Int16 nl = 0;
		LineType *wl = PFMalloc(sizeof(LineType)*ATZ_POINTS*8);

		ModErrThrowIf(!wl);

		PFSetForeColour(AppColourPrefs.atz);

		for (j=0;j<ATZ_POINTS;j+=2) {

			/*
			 * sequence is top, bottom, right, left *and*
			 * the clockwise & anti-clockwise components of
			 * each (e.g. first two draws are top clockwise
			 * and top anti-clockwise)
			 *
			 */

			ADDLINE( x+atzSinPlot[j], y-atzCosPlot[j],
					x+atzSinPlot[j+1],
					y-atzCosPlot[j+1]);
			ADDLINE( x-atzSinPlot[j], y-atzCosPlot[j],
					x-atzSinPlot[j+1],
					y-atzCosPlot[j+1]);
			ADDLINE( x+atzSinPlot[j], y+atzCosPlot[j],
					x+atzSinPlot[j+1],
					y+atzCosPlot[j+1]);
			ADDLINE( x-atzSinPlot[j], y+atzCosPlot[j],
					x-atzSinPlot[j+1],
					y+atzCosPlot[j+1]);
			ADDLINE( x+atzCosPlot[j], y-atzSinPlot[j],
					x+atzCosPlot[j+1],
					y-atzSinPlot[j+1]);
			ADDLINE( x+atzCosPlot[j], y+atzSinPlot[j],
					x+atzCosPlot[j+1],
					y+atzSinPlot[j+1]);
			ADDLINE( x-atzCosPlot[j], y-atzSinPlot[j],
					x-atzCosPlot[j+1],
					y-atzSinPlot[j+1]);
			ADDLINE( x-atzCosPlot[j], y+atzSinPlot[j],
					x-atzCosPlot[j+1],
					y+atzSinPlot[j+1]);

		}

		PFDrawLines(nl, wl);

		PFMemFree(wl);
			
	}

	/*
	 * draw the label
	 *
	 */

	PFSetTextColour(GUIGetSystemColour(GUIObjectForeground));

	FntSetFont( (this->type & aptFilter) ? boldFont:stdFont);
	labelLen = StrLen(this->label);

	if ( label ) {

		PFDrawOutlineChars(this->label, ALIGNCENTRE,
				x,y-iconSet->iconDim/2-FntCharHeight() );

	}

	/*
	 * draw the icon
	 *
	 * if an airport icon that shows a runway then we might need to rotate it
	 * 
	 */

	if (rotation != 0 && this->icon < 8 && ( (this->type & wpClassMask) & wpAllAirfields ) ) {

		/*
		 * need to apply further rotation to the airport icon
		 *
		 * (everything multiplied by 4 to keep it integer)
		 *
		 */

		Int16 i = this->icon + (rotation*4 - 45 ) / 90;

		i &= 7;
		PFScreenRectangleSetRel(&r,(i & 0x7) * iconSet->iconDim,
				((i & 0xfff8) >> 3) * iconSet->iconDim,
				iconSet->iconDim, iconSet->iconDim);

	} else {
		
		PFScreenRectangleSetRel(&r,(this->icon & 0x7) * iconSet->iconDim,
				((this->icon & 0xfff8) >> 3) * iconSet->iconDim,
				iconSet->iconDim, iconSet->iconDim);

	}

	x -= iconSet->iconDim/2;
	y -= iconSet->iconDim/2;
	if (drawMask) PFCopyRectangle(iconSet->iconMasks, &r, x, y, winMask);
	PFCopyRectangle(iconSet->icons, &r,  x, y, winOverlay);

#undef ADDLINE

}

/*
 * function : MapIconDrawSelected
 *
 */

void MapIconDrawSelected(const IconWindowsType *iconSet, MapIconType *this, Coord x, Coord y, Int16 rotation) {

	char *freq;
	Waypoint *wp; 
	PFScreenRectType r;

	FntSetFont(boldFont);

	PFSetForeColour(GUIGetSystemColour(GUIObjectForeground));
	PFSetBackColour(GUIGetSystemColour(GUIFormFill));
	PFSetTextColour(GUIGetSystemColour(GUIObjectForeground));
	PFSetDrawMode(blockMode);

	switch (this->type & wpClassMask) {
		
	case wpAirfield:
	case wpLargeAirfield:
		wp = WDMGetWaypoint(WPDataset, this->id);
		DrawRunways(this, wp, x,y, rotation);
		PFMemFree(wp);
		break;

	case wpVOR:
	case wpNDB:

		wp = WDMGetWaypoint(WPDataset, this->id);
		freq = CpGetNavFrequency(wp);
		if (freq) {

			PFDrawOutlineChars(freq, ALIGNCENTRE, x, y+iconSet->iconDim/2);
			PFMemFree(freq);

		}
		PFMemFree(wp);

		break;

	default:

		break;

	}

	//PFDrawOutlineChars(icon->label, ALIGNCENTRE, x, y-IconWindows.iconDim/2-FntCharHeight());

	PFScreenRectangleSetRel(&r, x-11, y-11, 21, 21);
	PFSetForeColour(AppColourPrefs.black);
	PFPaintRectangleFrame(&r,boldRoundFrame);

	MapIconDraw(iconSet, this, true, x, y, rotation);

}

