/*
 * HeadingIndicator.c
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
#include <PalmOSGlue.h>
#include <Rect.h>
#include "HeadingIndicator.h"
#include "MathLib.h"
#include "ResourceDefines.h"
#include "Utils.h"
#include "Polygon.h"
#include "Fixed.h"
#include "GlobalTypes.h"
#include "Modules.h"


#define ModuleID HeadingIndicatorModuleID

/*
 * 
 * Global variables
 *
 */

extern const AppColourPrefsType AppColourPrefs;

/****************************************************************************
 *
 * Module variables
 *
 */

#define ModuleID HeadingIndicatorModuleID

#define NUM_POINTER_POINTS 15
#define NUM_BUG_POINTS 3
#define NUM_POINTS (NUM_POINTER_POINTS + NUM_BUG_POINTS)

struct HSITypeStruct {

	Coord x,y;		// centre of HSI
	Coord radius;	// radius of HSI

	Int16 detail;	// 0 = minimum, 3 = maximum

	BitmapType *dialBmp;

	PointType points[NUM_POINTS+8];  // extra points for the CDI dots

	Int16 maxDeflection;

	FontID font;

};

/********************************************************************************
 *
 * Private Functions
 *
 * are declared here to allow easy relocation to another code section
 *
 */

static void DrawPointer(HSIType, Int16 course, float trkError, float maxTrackError)  HSI_SECTION;
static void DrawHeadingDisc(HSIType hsi, Int16 heading, Int16 bearing) HSI_SECTION;

/*
 * function : DrawPointer
 *
 * Draws the pointer with a deflected the centre section and
 * the CDI dots
 *
 * bearing - 0-359 direction that pointer is drawn
 *
 */

static void DrawPointer(HSIType hsi, Int16 course, float trkError, float maxTrkError) {

	PolyPointType np[NUM_POINTS+8];	// allow up to 8 CDI dots

	Int16         j;
	double angle  = ((double)course /180) *PI;
	Int16  deflection;

	FP14   sina, cosa;
	FP14   xCentre = Int2FP(hsi->x);
	FP14   yCentre = Int2FP(hsi->y);

	Int16 numCDIDots = 0;
	PFScreenRectType r;
	Int16 dotSize;

	LOGENTRY;

	trkError = MIN(trkError, maxTrkError);
	trkError = MAX(trkError, -maxTrkError);

	deflection = (Int16) ( (trkError / maxTrkError) * (float)hsi->maxDeflection);

	/*
	 * work out position of CDI points. These points are added to the end of
	 * the hsi->points storage, starting at NUM_POINTS. NB this is *after* the
	 * bearing-bug points.
	 *
	 */

	if (hsi->detail > 1 && maxTrkError >= 1.0) {

		float cdi;

		/*
		 * 1.0 = 3 dots
		 * 1.5 = 3 dots
		 * 2.0 = 5 dots
		 * 2.5 = 5 dots
		 * 3.0 = 7 dots
		 *
		 * i.e. there's always an odd number of dots
		 *
		 */

		numCDIDots = (Int16)(maxTrkError * 2);
		if (! (numCDIDots & 1)) numCDIDots++;
		
		cdi = - numCDIDots/2;
		for (j=0; j < numCDIDots; j++, cdi+=1.0) {

			hsi->points[NUM_POINTS+j].x = (Coord) ( (cdi / maxTrkError) * (float)hsi->maxDeflection);
			hsi->points[NUM_POINTS+j].y = 0;

		}

	}

	/*
	 * rotate
	 *
	 */

	sina = Double2FP(sin(angle));
	cosa = Double2FP(cos(angle));
	for (j=0; j < NUM_POINTS+numCDIDots; j++) {

		FP14 x,y;
		FP14 xtmp, ytmp;

		/*
		 * skip over the bug points, go to the CDI points
		 *
		 */

		if (j == NUM_POINTER_POINTS) j = NUM_POINTS;

		if (j > 6 && j < 11) {

			/*
			 * deflect centre section (CDI)
			 *
			 */


			x=Int2FP(hsi->points[j].x + deflection);

		} else {

			x=Int2FP(hsi->points[j].x);

		}
		y=Int2FP(hsi->points[j].y);

		
		/*
		 * do the maths, rounding the answer up or down
		 * according to the sign of the result
		 *
		 */

		xtmp = FPAdd(FPMul(-x,cosa), FPMul(y,sina));
		ytmp = FPAdd(FPMul(x,sina),FPMul(y,cosa));

		np[j].x = FPAdd(xCentre, xtmp);
		np[j].y = FPSub(yCentre,ytmp);

	}

	PFSetForeColour(AppColourPrefs.pointer);
	DrawPolygon(&np[0], 3);
	DrawPolygon(&np[3], 4);
	DrawPolygon(&np[7], 4);
	DrawPolygon(&np[11], 4);

	/*
	 * draw CDI dots
	 *
	 */

	dotSize = hsi->detail * 3;
	PFSetForeColour(GUIGetSystemColour(GUIObjectForeground));
	PFScreenRectangleSetRel(&r, 0,0, dotSize, dotSize);
	dotSize/=2;
	for (j = NUM_POINTS; j<NUM_POINTS+numCDIDots; j++) {
		Coord x1, y1;

		x1 = FP2Int(np[j].x) - dotSize;
		y1 = FP2Int(np[j].y) - dotSize;
		
		PFScreenRectangleSet(&r, x1, y1, x1+dotSize, y1+dotSize);
		PFPaintRectangle(&r, dotSize);

	}

	LOGEXIT;
}

/*
 * function : DrawHeadingDisc
 *
 * Draws the heading disc!!
 *
 */

static void DrawHeadingDisc(HSIType hsi, Int16 heading, Int16 bearing)  {

	Int16 j;
	Int16 jStep;
	Int16 angle;
	Int16 angleStep;

	LineType *lines = PFMalloc(sizeof(LineType)*36);
	LineType *line   = lines;

	char label[] = "N E S W   ";
	Int16 length = 1;
	Int16 fontHeight;

	double bugAngle;
	FP14   sina, cosa;
	FP14   xCentre = Int2FP(hsi->x);
	FP14   yCentre = Int2FP(hsi->y);
	PolyPointType np[NUM_BUG_POINTS];
	PointType *point;


	FntSetFont(hsi->font);
	fontHeight = FntCharHeight();
			
	/*
	 * use 4-way symmetry to draw the lines
	 *
	 */

	jStep     = hsi->detail > 1 ? 1 : 3;
	angleStep = jStep * 10;

	for (angle=360-heading, j=0; j < 9; angle+=angleStep, j += jStep) { 
		
		Int32 sn,cs;	// sine, cosine
		Coord x,y;		// edge, relative to hsi->x and hsi->y
		Coord xs, ys;	// start of spoke
		Coord xl, yl;	// label position

		WRAPMAX(angle, 360);

		GetSinCos(angle, &sn, &cs);
		x = (Coord) ((sn * hsi->radius)/COSINEZERO);
		y = (Coord) ((cs * hsi->radius)/COSINEZERO);
		xs = (Coord) ((sn * (hsi->radius-fontHeight/2))/COSINEZERO);
		ys = (Coord) ((cs * (hsi->radius-fontHeight/2))/COSINEZERO);
		
		// 0 - 90 degrees
		line->x1 = hsi->x + x; line->y1 = hsi->y - y;
		line->x2 = hsi->x + xs; line->y2= hsi->y - ys;
		line++;

		// 90-180 degress
		line->x1 = hsi->x + y; line->y1 = hsi->y + x;
		line->x2 = hsi->x + ys; line->y2= hsi->y + xs;
		line++;

		// 180-270 degrees
		line->x1 = hsi->x - x; line->y1 = hsi->y + y;
		line->x2 = hsi->x - xs; line->y2= hsi->y + ys;
		line++;

		// 270-360 degrees
		line->x1 = hsi->x - y; line->y1 = hsi->y - x;
		line->x2 = hsi->x - ys; line->y2= hsi->y - xs;
		line++;

		/*
		 * label 0,30, and 60 degree spokes, and their "mirrors"
		 *
		 */

		if (j == 0 || (hsi->detail > 1 && (j == 3 || j == 6))) {

			xl= (Coord) ((sn * (hsi->radius - fontHeight))/COSINEZERO);
			yl= (Coord) ((cs * (hsi->radius - fontHeight))/COSINEZERO);

			/*
			 * 'label' is preset correctly for j == 0
			 *
			 */

			if (j > 0) {

				length = 2;
				StrPrintF(label, "%02d%02d%02d%02d", j, j+9, j+18, j+27);
			}

			PFDrawChars(&label[0],length, hsi->x+xl-FntCharsWidth(&label[0],length)/2, hsi->y-yl-fontHeight/2);
			PFDrawChars(&label[2],length, hsi->x+yl-FntCharsWidth(&label[2],length)/2, hsi->y+xl-fontHeight/2);
			PFDrawChars(&label[4],length, hsi->x-xl-FntCharsWidth(&label[4],length)/2, hsi->y+yl-fontHeight/2);
			PFDrawChars(&label[6],length, hsi->x-yl-FntCharsWidth(&label[6],length)/2, hsi->y-xl-fontHeight/2);

		}

	}
		
	PFDrawLines(36, lines);
	PFMemFree(lines);

	/*
	 * draw bearing bug
	 *
	 */

	bearing = bearing - heading;
	WRAPMAX(bearing,360);
	bugAngle = DEG_TO_RAD(bearing);

	sina = Double2FP(sin(bugAngle));
	cosa = Double2FP(cos(bugAngle));
	point = &hsi->points[NUM_POINTER_POINTS];
	for (j=0; j < NUM_BUG_POINTS; j++, point++) {

		FP14 x,y;
		FP14 xtmp, ytmp;

		x=Int2FP(point->x);
		y=Int2FP(point->y);
		
		/*
		 * do the maths, rounding the answer up or down
		 * according to the sign of the result
		 *
		 */

		xtmp = FPAdd(FPMul(-x,cosa), FPMul(y,sina));
		ytmp = FPAdd(FPMul(x,sina),FPMul(y,cosa));

		np[j].x = FPAdd(xCentre, xtmp);
		np[j].y = FPSub(yCentre,ytmp);

	}

	PFSetForeColour(AppColourPrefs.pointer);
	DrawPolygon(np, NUM_BUG_POINTS);

	/*
	 * draw bitmap if it is available, otherwise draw a simple circle.
	 *
	 */

	if (hsi->dialBmp) {

		PFSetDrawMode(blockMode);
		PFBitmapPaint(hsi->dialBmp, hsi->x - hsi->radius, hsi->y - hsi->radius);

	} else {

		LineType *c;
		Int16 numLines;

		PFSetForeColour(GUIGetSystemColour(GUIObjectForeground));
		c = DrawCircle(hsi->x,  hsi->y, hsi->radius, &numLines);

		PFDrawLines(numLines, c);

	}


}

/********************************************************************************
 *
 * Public functions
 *
 */

/*
 * function : HeadingIndicatorInit
 *
 */

HSIType HSINew(Coord x, Coord y, Coord radius){

	UInt16 		j;
	Coord       tmp;

	HSIType      hsi;
	const PointType srcPoints[NUM_POINTS+NUM_BUG_POINTS] = { 

		// pointer tip (0-2)
		// NB code below assumes top of tip in point[1]
		{ -3,16 }, { 0, 24 }, { 3,16 },

		// box below tip (3-6)
		{ -1,17 }, { 1,17 }, { 1,8 }, {-1,8},

		// centre section, CDI (7-10)
		{ -1, 6 }, { 1,6 }, { 1, -6 }, { -1, -6 },

		// bottom section (11-14)
		{ -1, -8}, { 1,-8 }, { 1, -20 }, {-1, -20},

		// bearing bug (15-17)
		{ -4, 26 }, { 0, 34 }, { 4,26 }
		
	};
	float scale;
		
	hsi = PFMalloc(sizeof(struct HSITypeStruct));

	/*
	 * try and find a suitable sized bitmap for the specified radius
	 *
	 */

	hsi->dialBmp = NULL;
	if (radius == HSIStandardRadius) {

		hsi->dialBmp=PFBitmapLoad(DialBitmap, &tmp, NULL);
		ModErrThrowIf(tmp!=radius*2);

	}

	if (radius < 50) {

		hsi->detail = 0;
		hsi->font = stdFont;

	} else if (radius < 90) {

		hsi->detail = 1;
		hsi->font = boldFont;

	} else if (radius < 120) {

		hsi->detail = 2;
		hsi->font = largeFont;

	} else {

		hsi->detail = 3;
		hsi->font = largeBoldFont;

	}

	hsi->x = x + radius;
	hsi->y = y + radius;
	hsi->radius = radius;

	/*
	 * create the pointer for this HSI, based on its radius
	 *
	 * The points in srcPoints define a pointer based on a gauge radius of 40
	 * pixels
	 *
	 */

	scale = (float)hsi->radius / 40.0;

	for (j = 0; j < NUM_POINTS; j++) {

		hsi->points[j].x = (Coord) (scale * ((float)srcPoints[j].x + (srcPoints[j].x>0? 0.5:-0.5)));
		hsi->points[j].y = (Coord) (scale * ((float)srcPoints[j].y + (srcPoints[j].y>0? 0.5:-0.5)));

	}

	/*
	 * assume tip of pointer in points[1]
	 *
	 */

	hsi->maxDeflection = hsi->points[1].y;

	return hsi;

}

/*
 * function : HSIFree
 *
 */

void HSIFree(HSIType hsi) {

	if (hsi->dialBmp) PFBitmapFree(hsi->dialBmp);

	PFMemFree(hsi);

}

/* 
 * function : HSIDraw
 *
 */

void HSIDraw(const HSIType hsi, Int16 heading, Int16 bearing, 
		Int16 course, float trkError, float maxTrkError) {

	Int16		pointerAngle;

	LOGENTRY;

	PFDrawStatePush();

	pointerAngle = course - heading;
	WRAPMAX(pointerAngle,360);

	DrawHeadingDisc(hsi, heading, bearing);
	DrawPointer(hsi, pointerAngle, trkError, maxTrkError);

	PFDrawStatePop();

	LOGEXIT;

}

#include "NavManager.h"

struct HSIMiniPanelTypeStruct {
	
	HSIType hsi;
	
};

HSIMiniPanelType HSIMiniPanelNew(Coord x, Coord y, Coord width, Coord height) {

	HSIMiniPanelType hsimp = PFMalloc(sizeof(struct HSIMiniPanelTypeStruct));
	
	if (width > height) {

		hsimp->hsi = HSINew((x+width/2) - HSIStandardRadius, y, HSIStandardRadius);
		
	} else {
		
		hsimp->hsi = NULL;
		
	}
	
	return hsimp;
	
}

void HSIMiniPanelFree(HSIMiniPanelType hsimp) {
	
	if (hsimp->hsi) HSIFree(hsimp->hsi);
	
	PFMemFree(hsimp);
	
}

void HSIMiniPanelDraw(const HSIMiniPanelType hsimp) {

	if (hsimp->hsi) {
		
		HSIDraw(hsimp->hsi, NavGetIntValue(navTrack), NavGetIntValue(navBearingTo),
				NavGetIntValue(navCourse), NavGetFloatValue(navXTRK), 2.0);
		
	}
	
}
