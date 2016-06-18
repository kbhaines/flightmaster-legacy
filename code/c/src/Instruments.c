/*
 * Instruments.c
 *
 * (c)2005 Blackhawk Systems
 *
 */

#include "Platform.h"
#include "Utils.h"
#include "GlobalTypes.h"
#include "Instruments.h"
#include "Modules.h"
#include "Fixed.h"
#include "Polygon.h"
#include "Gps.h"
#include "NavManager.h"

#define ModuleID InstrumentModuleID

extern const IconWindowsType IconWindows;
extern const AppColourPrefsType AppColourPrefs;

/*******************************************************************************
 *
 * module variables
 * 
 *
 */

struct VSITypeStruct {

	Coord x, y, width, height; // y is centre of VSI, height is 1/2 height

	float scale;
	PolyPointType point[5];

	Boolean leftHanded;

};


struct GSITypeStruct { 
	
	Coord x,y, width, height;	// y is centre of GSI, height is 1/2 height

	float scale;

	Int32 maxDeflection;
	Int32 tickInterval;

};

struct SatConstTypeStruct {

	PFScreenRectType position;
	GPSType   *gps;

	Coord radius;	// radius of constellation

	Coord barWidth, barHeight;	// signal bars
	Coord barx, bary;			// x & y position of *bottom* of first bar

};

/*******************************************************************************
 *
 * private functions
 *
 */


/*******************************************************************************
 *
 * global functions
 *
 */

  
/*
 * VSINew
 *
 */

VSIType VSINew(Coord x, Coord y, Coord width, Coord height, Boolean leftHanded, Int32 maxDeflection) {

	const float barHeight = height/2;
	const Coord yCentre = y + barHeight;
	Coord barWidth  = width;
	Coord barLeft, barRight;

	VSIType vsi = PFMalloc(sizeof(struct VSITypeStruct));

	ModErrThrowIf(!vsi);

	width--;
	barWidth--;

	if (leftHanded) {

		barLeft = x;
		barRight= x + barWidth;

	} else {
		
		barLeft = x;
		barRight = x + barWidth;

	}

	vsi->leftHanded = leftHanded;
	vsi->scale = (float) barHeight / (float) maxDeflection;
	vsi->x = x;
	vsi->y = y + height/2;
	vsi->width = width;
	vsi->height = height/2;

	vsi->point[0].x = Int2FP(barLeft); 
	vsi->point[0].y = Int2FP(yCentre);
	
	vsi->point[1].x = Int2FP(barLeft);
	vsi->point[2].y = Int2FP(yCentre-4);

	vsi->point[2].x = Int2FP(barLeft+barWidth/2);
	vsi->point[2].y = Int2FP(yCentre-6);

	vsi->point[3].x = Int2FP(barRight);
	vsi->point[3].y = Int2FP(yCentre-4);

	vsi->point[4].x = Int2FP(barRight);
	vsi->point[4].y = Int2FP(yCentre);

	return vsi;

}

/*
 * VSIFree
 *
 */

void VSIFree(VSIType this) {

	PFMemFree(this);

}

/*
 * VSIDraw
 *
 */

void VSIDraw(VSIType this, Int32 vs, Int32 requiredVS) {
	
	LOGENTRY;

	/*
	 * Draw VSI tape if required
	 *
	 */

	PFSetDrawMode(blockMode);
	if (vs > 15 || vs < -15) {

		Boolean descent = (vs < 0);
		Int16 vsiLength;
		char vsStr[maxStrIToALen];
		Coord vsiNumber;
		PolyPointType *vsiPoints = this->point;

		if (descent) vs = -vs;

		LOGINT32(vs);

		vsiLength = MIN( (Int16)(vs*this->scale), this->height);

		LOGINT16(vsiLength);

		StrIToA(vsStr, (Int32)((vs + 50)/100));

		LOGSTR(vsStr);

		/*
		 * VSI points look like this:
		 *
		 *
		 * 			2
		 *		  /	  \
		 * 		1		3
		 *		|		|
		 *		0-------4
		 * 		
		 * So we can calculate y-values of points 1,2 and 3 relative to 0 and 4
		 *
		 */

		FntSetFont(largeFont);
		if (descent) {
			
			vsiPoints[1].y = FPAdd(vsiPoints[0].y , Int2FP(vsiLength));
			vsiPoints[2].y = FPAdd(vsiPoints[1].y, Int2FP(6));
			vsiPoints[3].y = vsiPoints[1].y;
			vsiNumber=FP2Int(vsiPoints[2].y) - 1;

			PFSetForeColour(AppColourPrefs.vsiDescend);

		} else {
			
			vsiPoints[1].y = FPSub(vsiPoints[0].y, Int2FP(vsiLength));
			vsiPoints[2].y = FPSub(vsiPoints[1].y, Int2FP(6));
			vsiPoints[3].y = vsiPoints[1].y;
			vsiNumber=FP2Int(vsiPoints[2].y) + 2 - FntLineHeight();

			PFSetForeColour(AppColourPrefs.vsiClimb);
		}

		LOGTAG("DrawVSI");
		DrawPolygon(vsiPoints, 5);
		LOGTAG("DrawVSI");

		PFSetTextColour(GUIGetSystemColour(GUIObjectForeground));
		PFDrawOutlineChars(vsStr,ALIGNCENTRE,this->x+this->width/2, vsiNumber);

	}

	/*
	 * draw required vertical speed indication
	 *
	 */

	if (requiredVS > 15 || requiredVS < -15) {

		Boolean descent = (requiredVS < 0);
		Int16   vsiLength;
		Coord xpos, ypos;
		char vsStr[maxStrIToALen];
		char marker[]="\002";

		LOGINT32(requiredVS);

		if (descent) requiredVS = -requiredVS;

		vsiLength = MIN( (Int16)(requiredVS*this->scale), this->height);

		if (requiredVS > 5000) {

			StrCopy(vsStr, "**");

		} else { 

			StrIToA(vsStr, (Int32)((requiredVS + 50)/100));

		}

		LOGINT16(vsiLength);

		FntSetFont(boldFont);

		if (this->leftHanded) {

			xpos   = this->x+this->width - 2;
			marker[0] = 3;

		} else {

			xpos   = this->x - FntCharWidth('9') + 2;
			marker[0] = 2;

		}

		ypos = this->y + (descent ? vsiLength : -vsiLength);

		PFSetTextColour(GUIGetSystemColour(GUIObjectForeground));
		PFSetForeColour(GUIGetSystemColour(GUIObjectForeground));

		PFDrawOutlineChars(vsStr, ALIGNRIGHT, xpos+4, ypos - FntLineHeight()/2);

		FntSetFont(symbol11Font);
		PFDrawOutlineChars(marker, ALIGNLEFT,this->x+1, ypos - FntLineHeight()/2);
		//PFDrawLine(vsi->x+2, vsi->y, vsi->x+2, ypos);

	}

	PFSetForeColour(GUIGetSystemColour(GUIObjectForeground));
	PFDrawLine(this->x, this->y, this->x+this->width, this->y);

	LOGEXIT;

}


/*
 * GSINew
 *
 */

GSIType GSINew(Coord x, Coord y, Coord width, Coord height, Int32 maxDeflection, Int32 tickInterval) {

	GSIType gsi = PFMalloc(sizeof(struct GSITypeStruct));

	ModErrThrowIf(!gsi);

	gsi->x = x;
	gsi->y = y + height/2;
	gsi->width = width;
	gsi->height = height / 2;

	gsi->scale = (float) gsi->height / (float) maxDeflection;

	gsi->maxDeflection = maxDeflection;
	gsi->tickInterval  = tickInterval;

	return gsi;

}

/*
 * GSIFree
 *
 */

void GSIFree(GSIType this) {

	PFMemFree(this);

}

/*
 * GSIDraw
 *
 */

void GSIDraw(GSIType this, Int32 altError) {

	Coord deflection;
	Boolean above = altError > 0;
	Int32 tick;
	const char *gsichar = "\002";

	LOGENTRY;

	LOGINT32(altError);

	/*
	 * draw the GSI scale & tick marks
	 *
	 */

	PFSetDrawMode(blockMode);
	PFSetForeColour(GUIGetSystemColour(GUIObjectForeground));
	PFSetTextColour(GUIGetSystemColour(GUIObjectForeground));
	PFDrawLine(this->x, this->y - this->height, this->x, this->y+this->height);
	PFDrawLine(this->x, this->y, this->x+this->width, this->y);

	for (tick = -this->maxDeflection; tick <= this->maxDeflection; tick+=this->tickInterval) {

		deflection = (Int16)(tick * this->scale);
		PFDrawLine(this->x, this->y+deflection, this->x+this->width/2, this->y+deflection);

	}

	/*
	 * work out the deflection, then draw it against the scale
	 *
	 */

	if (!above) altError = -altError;
	deflection = MIN( (Int16)(altError*this->scale), this->height);

	LOGINT16(deflection);

	FntSetFont(symbol11Font);
	PFDrawOutlineChars(gsichar, ALIGNLEFT, this->x + this->width/2, this->y + (above ? deflection : -deflection)-FntLineHeight()/2);
	
	LOGEXIT;

}

/*
 * function : SatConstNew
 *
 */

SatConstType SatConstNew(GPSType *gps, Coord x, Coord y, Coord w, Coord h) {

	SatConstType sc = PFMalloc(sizeof(struct SatConstTypeStruct));

	ModErrThrowIf(!sc);

	PFScreenRectangleSetRel(&sc->position, x,y,w,h);
	sc->gps = gps;

	sc->radius = MIN(w,h) / 2;

	/*
	 * In both modes, the bars have 10px below them for the satellite ident 
	 *
	 */

	if (w < h) {

		/*
		 * portrait mode (Constellation, not the screen!!)
		 *
		 * Bars have 2 px between and start underneath constellation
		 *
		 */

		sc->barHeight = h/4 - 10;
		sc->barWidth  = MIN(((w-12) / 3), 20);

		sc->barx = x;
		sc->bary = y + sc->radius*2 + sc->barHeight+5;

	} else {

		/*
		 * landscape mode
		 *
		 * Bars have 2 px between, and start 5 px right of constellation
		 *
		 */

		sc->barHeight = (h/2) - 20;
		sc->barWidth = MIN( (w/2 - 24) / 3, 20);

		sc->barx = x + sc->radius*2+10;
		sc->bary = y + sc->barHeight;

	}

	return sc;

}

/*
 * function : SatConstFree
 *
 */

void SatConstFree(SatConstType this) {

	PFMemFree(this);

}

/*
 * function : SatConstDraw
 *
 */

void SatConstDraw(SatConstType this) {

	Int16         j;
	PFScreenRectType satRect;

	const Coord   x         = this->position.x1;
	const Coord   y         = this->position.y1;
	const Coord   radius	= this->radius;
	const Coord   barWidth  = this->barWidth;
	const Coord   barHeight = this->barHeight;
	Coord   barx      = this->barx;
	Coord   bary      = this->bary;

	Int16         numBars   = 0;
	Int16         iconIndex;

	PFDrawStatePush();

	PFEraseRectangle(&this->position,0);

	/*
	 * draw outer and inner constellation rings
	 *
	 */

	PFScreenRectangleSetRel(&satRect, x,y, radius*2, radius*2);

	PFSetForeColour(AppColourPrefs.satConstOuter);
	PFPaintRectangle(&satRect, radius);

	PFSetForeColour(AppColourPrefs.satConstInner);
	PFScreenRectangleSetRel(&satRect, x+radius/2,y+radius/2, radius,radius);
	PFPaintRectangle(&satRect, radius/2);

	if (this->gps->sat.fixType == 0) {
		
		PFDrawStatePop();
		return;

	}

	/*
	 * draw the aircraft icon
	 *
	 */

	iconIndex = ( (UInt16)(this->gps->posn.trueHeading*8) + 45) / 90;
	WRAPMAX(iconIndex, 32);

	PFScreenRectangleSetRel(&satRect, (IconWindows.aircraftDim*(iconIndex & 7)),
			IconWindows.aircraftDim*((iconIndex & 0x18)/8),
			IconWindows.aircraftDim,
			IconWindows.aircraftDim);
	
	PFCopyRectangle(IconWindows.aircraft, &satRect,
			this->position.x1+radius-IconWindows.aircraftDim/2, 
			this->position.y1+radius-IconWindows.aircraftDim/2,
			winOverlay);

	if (GPSSimulating()) {
		
	}

	PFSetDrawMode(blockMode);
	for (j=1;j < GPSNumSats; j++) {

		Int16 stnr = this->gps->sat.stnr[j];
		Int16 elevation = this->gps->sat.elevation[j];
		Int16 azimuth = this->gps->sat.azimuth[j];
		Coord sx, sy;
		Int32 sn, cs;
		char sid[3];

		/*
		 * invalid satellite?
		 *
		 */

		if (GPSSimulating() && j<13) {
			
			elevation = 20;azimuth=j*360/12;stnr=35;

		}

		if (elevation == -1 || azimuth == -1 || stnr == -1) continue;

		LOGINT16(j);
		LOGINT16(elevation);
		LOGINT16(azimuth);
		LOGINT16(stnr);

		/*
		 * draw the tag
		 *
		 */

		PFSetTextColour(stnr > 0 ? (stnr > 30 ? AppColourPrefs.green:AppColourPrefs.yellow) : AppColourPrefs.red);
		
		GetSinCos(elevation, &sn, &cs);
		sy = (Coord) ( ((radius-FntLineHeight()/2) * cs)/COSINEZERO);

		LOGINT16(sy);

		GetSinCos(azimuth, &sn, &cs);
		sx = (Coord) ( ((Int32)sy*sn)/COSINEZERO);
		sy = (Coord) ( ((Int32)sy*cs)/COSINEZERO);

		StrPrintF(sid, "%02d", j);
		FntSetFont(boldFont);
		PFDrawOutlineChars(sid, ALIGNCENTRE, x+radius+sx, y+radius-sy-FntLineHeight()/2);

		/*
		 * draw the strength bar
		 *
		 */

		if (stnr > 0) {

			/*
			 * height clipped to max 50 STNR
			 *
			 */

			Coord h = (MIN(stnr, 50) * barHeight) / 50;

			/*
			 * background rectangle
			 *
			 */

			PFScreenRectangleSetRel(&satRect, barx+2, bary-barHeight, barWidth-4, barHeight+2);
			PFSetForeColour( stnr > 50 ? AppColourPrefs.green:AppColourPrefs.satConstOuter);
			PFPaintRectangle(&satRect,2);

			/*
			 * bar and satellite PRN
			 *
			 */

			PFSetForeColour(stnr > 30 ? AppColourPrefs.green:AppColourPrefs.yellow);
			PFSetTextColour(AppColourPrefs.satConstOuter);
			PFScreenRectangleSetRel(&satRect, barx+6, bary-h, barWidth-12, h);
			PFPaintRectangle(&satRect,0);
			PFSetForeColour(AppColourPrefs.background);
			PFPaintRectangleFrame(&satRect, simpleFrame);
			FntSetFont(stdFont);
			PFPaintChars(sid, 2, barx, bary);

			/*
			 * move along to next bar drawing location, down onto
			 * next line if we've drawn six already
			 *
			 */

			barx += barWidth + 2;
			if ( ++numBars == 6) {
				
				barx -= (barWidth+2)*6;
				bary += barHeight + 20;

			}

		}
			
	}

	PFDrawStatePop();
	return;
		
}

/*
 * function : FlightDirectorDraw
 *
 */

void FlightDirectorDraw(Coord centreX, Coord centreY, Coord spacing) {

	/*
	 * aob = Angle of Bank
	 *
	 * fd = Flight Director
	 *
	 */

	Int16  aobDir;
	Coord  aobX, aobY;
	char   aobChar;		// character in fontset to use (L or R flag)
	Coord  aobH;		// aob indicator height
	float  aobMagnitude = NavGetFloatValue(navBankAngle);
	Int16  aobLength;

	float  fdMagnitude  = NavGetFloatValue(navFlightDirectorRoll);
	Int16  fdLength;
	Int16  fdMax;
	Int16  fdDir;
	Coord  fdX, fdY;
	Coord  fdH;			// fd bar hieght
	PFScreenRectType fdRect;

	Int16  j;
	IndexedColorType colour;
	
	LOGENTRY;

	PFDrawStatePush();

	FntSetFont(symbol11Font);
	aobH = FntLineHeight();
	fdH  = aobH / 3;
	aobChar = 3;

	aobDir = FntCharWidth(aobChar);
	//aobDir -= aobDir/3;

	aobX= centreX + spacing;
	aobY= centreY - aobH/2;

	fdMax  = aobDir * 4;	// FD max 40 degrees

	/*
	 * set up Flight Director, but don't draw it yet
	 * position at bottom of AOB indicator
         *
	 */

	fdY = aobY+aobH - fdH;
	if (fdMagnitude > 0.9) {
		
		fdDir = aobDir;
		fdLength = fdMax * (fdMagnitude / 40);
		fdLength = MIN(fdMax, fdLength);
		fdX = centreX + spacing;
		PFScreenRectangleSetRel(&fdRect, fdX, fdY, fdLength, fdH);

	} else if (fdMagnitude < -0.9) {

		fdDir = -aobDir;
		fdLength = fdMax * (-fdMagnitude / 40);
		fdLength = MIN(fdMax, fdLength);
		fdX = centreX - spacing;
		PFScreenRectangleSetRel(&fdRect, fdX - fdLength, fdY, fdLength, fdH);

	} else {

		fdDir = 0;
		fdLength = 0;
		
	}

	if (aobMagnitude < 0) {

		aobX = centreX - spacing - aobDir;
		aobDir = -aobDir;
		aobMagnitude = -aobMagnitude;
		aobChar = 2;

	}

	/*
	 * AOB indicator shows one char per 10 degrees of bank
	 *
	 */

	aobLength = MIN(4, ((UInt16) ((aobMagnitude + 5.0)/ 10.0)));
	if (aobLength == 0 && aobMagnitude > 5.0) aobLength = 1;

	/*
	 * map from 1-4 onto the traffic light colours (0=green,2=red)
	 *
	 */

	if (aobLength < 4) 
		
		colour = AppColourPrefs.green;

	else

		colour = AppColourPrefs.red;

	PFSetTextColour(colour);
			
	PFSetDrawMode(overlayMode);
	while (aobLength--) {

		PFPaintChars(&aobChar, 1, aobX, aobY);
		aobX += aobDir;

	}

	PFSetTextColour(colour);

	if (fdLength) {

		/*
		 * Draw FlightDirector over top
		 *
		 * Break the FD into 'blocks' of fdDir (which represents 10 degrees of
		 * required bank)
		 *
		 */

		PFSetDrawMode(blockMode);
		PFScreenRectangleInset(&fdRect,-1);
		PFSetForeColour(AppColourPrefs.white);
		PFPaintRectangle(&fdRect,0);

		PFScreenRectangleInset(&fdRect,1);
		PFSetForeColour(AppColourPrefs.black);
		PFPaintRectangle(&fdRect,0);

		j = fdLength / fdDir;
		if (j < 0) j = -j;
		while (j--) {

			fdX += fdDir;
			PFEraseLine(fdX, fdY, fdX, fdY+fdH);

		} 

	}
		
	PFDrawStatePop();

	LOGEXIT;

}
