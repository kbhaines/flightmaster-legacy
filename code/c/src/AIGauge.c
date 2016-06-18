/*
 * AIGauge.c
 *
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
#include "Constants.h"
#include "AIGauge.h"
#include "MathLib.h"
#include "ResourceDefines.h"
#include "Utils.h"
#include "GlobalTypes.h"
#include "Modules.h"
#include "Polygon.h"


extern const AppColourPrefsType AppColourPrefs;

/****************************************************************************
 *
 * Module variables
 *
 */

#define AIErrThrow  ErrThrow(AIGaugeModuleID | __LINE__)

/*
 * offscreen windows holding bitmap images of the outer dial
 *
 */

static BitmapType *dialBmp;

static Coord     dialDiameter;

#define NUM_AI_POINTS 23

/*
 *
 * The dynamic part of the AI consists of 23 points (shown in 2 separate
 * parts below for clarity):
 *
 *
 * 0 ----4----- 1
 * |    / \     |
 * |   /   \    |
 * |   5    6   | 
 * 3------------2
 *
 *  13-----14
 *    11-12
 *   9-----10
 *     7-8
 *0-----4-----1
 *    15-16
 *  17-----18
 *    19-20
 *  21-----22
 *  
 */

static PointType aiPoints[NUM_AI_POINTS];

/*
 * pixelsPerDegree determines how many y-pixels are used to represent a degree 
 * of pitch on the AI
 *
 */

static Coord pixelsPerDegree = 3;


/********************************************************************************
 *
 * Private Functions
 *
 * are declared here to allow easy relocation to another code section
 *
 */


/********************************************************************************
 *
 * Public functions
 *
 */

/*
 * function : AIGaugeInit
 *
 */
Err AIGaugeInit(Coord *radius){
	UInt16 		j,k;

	/*
	 * load the bitmaps, create the mask
	 *
	 */

	FTRACE;

	dialBmp=PFBitmapLoad(AIDialBitmap, &dialDiameter, NULL);	// assumed square!!
	*radius = dialDiameter / 2;

	/*
	 * initialise the points of the dynamic portion of the 
	 * AI display
	 *
	 */

	aiPoints[0].x = - *radius; aiPoints[0].y = 0;
	aiPoints[1].x = *radius; aiPoints[1].y = 0;
	aiPoints[2].x = *radius; aiPoints[2].y = - *radius * 2;
	aiPoints[3].x = -*radius; aiPoints[3].y = - *radius * 2;

	aiPoints[4].x = 0; aiPoints[4].y = 0;
	aiPoints[5].x = -*radius*2*cos(DEG_TO_RAD(30)); aiPoints[5].y = -*radius*2*sin(DEG_TO_RAD(30));
	aiPoints[6].x = *radius*2*cos(DEG_TO_RAD(30)); aiPoints[6].y = -*radius*2*sin(DEG_TO_RAD(30));

	/*
	 * each line is 5 degrees...
	 *
	 */

	for (j=7, k=5*pixelsPerDegree;j<15;j+=2,k+=5*pixelsPerDegree) {

		/*
		 * lines 7-8 & 11-12 are shorter...
		 *
		 */

		aiPoints[j].x = ( (j==7) || (j==11)) ? -8:-16;

		aiPoints[j].y = k;

		aiPoints[j+1].x = -aiPoints[j].x;
		aiPoints[j+1].y = aiPoints[j].y;
	}
	
	/*
	 * points 15-22 are a mirror image about the x axis of points 7-14
	 * 
	 */

	for (j=15,k=7;j<NUM_AI_POINTS;j++,k++) {
		aiPoints[j].x = aiPoints[k].x;
		aiPoints[j].y = -aiPoints[k].y;
	}

	FTRACE;

	return errNone;
}

/* 
 * function : AIGaugeDraw
 *
 */

void AIGaugeDraw(Coord gaugeX, Coord gaugeY, double roll, double pitch) {

	FP14 		centreX, centreY;
	PFScreenRectType   aiRect;
	UInt16          j;
	PolyPointType   newAiPoints[NUM_AI_POINTS];
	Coord           translate;
	FP14            sinr, cosr;
	FP14            x,y;
	
	FTRACE;

	PFDrawStatePush();

	PFScreenRectangleSet(&aiRect, gaugeX, gaugeY, gaugeX + dialDiameter, gaugeY + dialDiameter);
	centreX = Int2FP(gaugeX+dialDiameter/2);
	centreY = Int2FP(gaugeY+dialDiameter/2);

	/*
	 * need to set the clipping rectangle to prevent drawing the
	 * lines outside of the bounding rectangle of the gauge. The
	 * gauge bitmap has transparency in the centre allowing the lines
	 * to show through, while blocking out the rest of the lines that
	 * fall outside the bounds of the gauge.
	 *
	 */

	PFSetClippingRectangle(&aiRect);

	/*
	 * setup the rotated AI lines/features
	 *
	 * The transformation translates and then rotates.
	 *
	 */

	TIME_START;

	translate = RAD_TO_DEG(pitch) * pixelsPerDegree;

	sinr = Double2FP(sin(roll));
	cosr = Double2FP(cos(roll));

	for (j=0;j<NUM_AI_POINTS;j++) {
		x = Int2FP(aiPoints[j].x);
		y = Int2FP(aiPoints[j].y - translate);

		newAiPoints[j].x = FPAdd( centreX, FPAdd(FPMul(-x,cosr), FPMul(y, sinr)));
		newAiPoints[j].y = FPSub( centreY, FPAdd(FPMul(x,sinr), FPMul(y, cosr)));
		
	}

	/*
	 * draw the sky and the ground 
	 *
	 */

	PFSetForeColour(AppColourPrefs.sky);
	PFPaintRectangle(&aiRect,0);
	PFSetForeColour(AppColourPrefs.ground);
	DrawPolygon(&newAiPoints[0],4);

	TIME_STOP(99,90);

	/*
	 * draw the AI lines
	 *
	 */

	PFSetForeColour(AppColourPrefs.aaLines);
	PFDrawLine(FP2Int(newAiPoints[0].x), FP2Int(newAiPoints[0].y), FP2Int(newAiPoints[1].x), FP2Int(newAiPoints[1].y));
	PFDrawLine(FP2Int(newAiPoints[4].x), FP2Int(newAiPoints[4].y), FP2Int(newAiPoints[5].x), FP2Int(newAiPoints[5].y));
	PFDrawLine(FP2Int(newAiPoints[4].x), FP2Int(newAiPoints[4].y), FP2Int(newAiPoints[6].x), FP2Int(newAiPoints[6].y));
	for (j=7;j<NUM_AI_POINTS;j+=2) {
		PFDrawLine(FP2Int(newAiPoints[j].x),FP2Int(newAiPoints[j].y), FP2Int(newAiPoints[j+1].x), FP2Int(newAiPoints[j+1].y));
	}
	
	/* 
	 * draw the face
	 *
	 */

	PFSetDrawMode(blockMode);
	PFBitmapPaint(dialBmp, gaugeX, gaugeY);

	PFResetClipping();

	PFDrawStatePop();
	FTRACE;
}

/*
 * function : AIGaugeDeInit
 */
void AIGaugeDeInit(void) {
	
	PFBitmapFree(dialBmp);
	
}
