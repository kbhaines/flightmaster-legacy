/*
 * MapAirspace.c
 *
 */

#include "Platform.h"
#include "GlobalTypes.h"
#include "MapAirspace.h"
#include "Utils.h"
#include "MathLib.h"
#include "Modules.h"

#include "FMPreferences.h"

#define ModuleID MasModuleID

/*******************************************************************************
 *
 * global variables
 *
 */

extern const AppColourPrefsType AppColourPrefs;
extern const FMPreferencesType Preferences;

extern const Boolean GPSState;


/*******************************************************************************
 *
 * module variables
 *
 */

static Int32 latRef, lonRef;
static Int32 latScale, lonScale;
static Coord clipLimit;

#define ROUND(x) ((x) > 0 ? (COSINEZERO/2):-(COSINEZERO/2))

/*******************************************************************************
 *
 * private functions
 *
 */

static Boolean StoreClipLine(LineType *line, Coord x1, Coord y1, Coord x2, Coord y2) ADT_SECTION;

/*
 * function : StoreClipLine
 *
 * Returns true if the line was stored and within bounds, false if not (if
 * false, *line is not updated)
 *
 */

static Boolean StoreClipLine(LineType *line, Coord x1, Coord y1, Coord x2, Coord y2) {

	Boolean accepted = ClipLine16(&x1, &y1, &x2, &y2, -clipLimit, -clipLimit, clipLimit, clipLimit);

	if (accepted) {

		line->x1 = x1;
		line->y1 = y1;
		line->x2 = x2;
		line->y2 = y2;

	}

	return accepted;


}
		
static Coord bdryCursorX, bdryCursorY;

#define BoundarySetCursor(x,y) bdryCursorX = (x);bdryCursorY = (y)

#define BoundaryDrawLine(lines,numLines,x,y)  if (StoreClipLine(&lines[(numLines)], bdryCursorX, bdryCursorY, x,y))\
							(numLines)++;\
						BoundarySetCursor(x,y)

#define BoundaryAddLine(lines,numLines,nx1,ny1,nx2,ny2) if (StoreClipLine(&lines[(numLines)],nx1,ny1,nx2,ny2))\
							 (numLines)++;

/*******************************************************************************
 *
 * public functions
 *
 */

/*
 * function : MapAirspaceInit
 *
 */

void MapAirspaceInit(Int32 latReference, Int32 lonReference,
		Int32 latScaling, Int32 lonScaling,
		Coord clip) {

	latRef = latReference;
	lonRef = lonReference;
	latScale = latScaling;
	lonScale = lonScaling;

	clipLimit = clip;

}

/*
 * function : MapAirspaceCreate
 *
 *
 */

Boolean MapAirspaceCreate(const AirspaceType *as, MapAirspaceType *mas, AirspaceIDType id, UInt16 altUnits) {

	const char *segmentCode;
	const void *segRecordPtr;
	Int16 segNum = 0;
	LineType /*@relnull@*/ *lines = NULL;
	Int16 maxLines,j;

	LOGTIMESTART;

	mas->id   = id;
	mas->type = as->type;
	mas->clipCount = 0;

	segmentCode = as->segmentCode;

	/*
	 * altitude labels for airspace.
	 *
	 */

	AsDecodeAltitude(mas->upperAltString, &mas->upperAlt, altUnits, as->upperAltRef,
			as->upperAlt);
	AsDecodeAltitude(mas->lowerAltString, &mas->lowerAlt, altUnits, as->lowerAltRef,
			as->lowerAlt);

	/*
	 * if upper & lower altitudes are AGL, then set effective limits to 0-99999
	 * - otherwise above code will leave both limits at 99999 which will cause
	 *   filtering of the airspace
	 *
	 */

	if (as->lowerAltRef == altAgl) {

		mas->lowerAlt = 0;

	} 

	if (as->upperAltRef == altAgl) {

		mas->upperAlt = 99999;

	}

	if (as->type & asTypeAirway) {

		StrNCopy(mas->airwayIdent, GetStringFromList(as->segmentCode, 1), MAX_AIRWAY_CHARS);

		for (j=0;j<MAX_AIRWAY_CHARS;j++) {

			if (mas->airwayIdent[j] == 10) mas->airwayIdent[j] = 0;

		}

	}

	/*
	 * locate start of segment record, after end of two null-terminated
	 * strings (segmentCode and description)
	 *
	 */
	
	LOGSTR(segmentCode);
	segRecordPtr = segmentCode;
	while (*(char*)segRecordPtr != 0) segRecordPtr ++;
	segRecordPtr++;
	LOGSTR(segRecordPtr);
	while (*(char*)segRecordPtr != 0) segRecordPtr ++;
	segRecordPtr++;

	/*
	 * count the number of line we're likely to need
	 *
	 */

	maxLines = 0;

	for (j=0;segmentCode[j]!=0;j++) 
		maxLines += segmentCode[j] == 'l' ? 1 : 48;

	ModErrThrowIf(!maxLines);
	PFSafeMalloc(lines, sizeof(LineType)*maxLines);

	/*
	 * loop through the segments, adding them to the Boundary 
	 *
	 */

	mas->numLines = 0;

	while (mas->numLines < maxLines && *segmentCode != 0) {

		if ( *segmentCode == 'l' ) {

			LineSegmentType *l = (LineSegmentType*)segRecordPtr;
			Coord x, y;

			LOGLINE;
			y = (Coord) (((l->lat - latRef)) / latScale);
			x = (Coord) ((-l->lon - lonRef) / lonScale);

			if (segNum > 0) {
				
				BoundaryDrawLine(lines,mas->numLines,x,y);

			} else {

				BoundarySetCursor(x,y);

			}

			LOGLINE;
			segRecordPtr += sizeof(LineSegmentType);

		} else if ( *segmentCode == 'a' ) {

			ArcSegmentType *a = (ArcSegmentType*)segRecordPtr;
			Coord cx,cy, r;
			Int32 sn,cs;
			Int16 stepSize = 32768/24;
			Int32 radius = a->radius > 0 ? a->radius : - a->radius;
			const Coord referenceRadius = 80;

			cy = (Coord) ((a->lat - latRef) / latScale);
			cx = (Coord) ((-a->lon - lonRef) / lonScale);
			r  = (Coord) ((radius+(latScale/2))  / latScale);

			/*
			 * adjust the step-size according to radius
			 *
			 */
			
			if (r < referenceRadius) {

				if (r < referenceRadius / 2) {

					if (r < referenceRadius / 4)

						stepSize = 32768/8;

					else

						stepSize = 32768/12;

				} else {

					stepSize = 32768/16;

				}

			}

			if (a->start == a->end) {

				/* 
				 * arc is a circle
				 *
				 */

				Int16 angle = 0;
				Coord lastox, lastoy;
				
				lastox = 0;
				lastoy = r;
				for (angle = stepSize; angle <= 32768/4;angle += stepSize) {

					Coord ox, oy;
					IntSinCos(angle, &sn, &cs);

					ox = (Coord) ((sn*r + (COSINEZERO/2)) / COSINEZERO);
					oy = (Coord) ((cs*r + (COSINEZERO/2)) / COSINEZERO);

					/*
					 * top, right, bottom, left, in pairs
					 *
					 */

					BoundaryAddLine(lines,mas->numLines,cx + lastox, cy + lastoy, cx+ox, cy+oy);
					BoundaryAddLine(lines,mas->numLines,cx-ox, cy+oy, cx - lastox, cy + lastoy);
					
					BoundaryAddLine(lines,mas->numLines,cx+oy, cy+ox, cx + lastoy, cy + lastox);
					BoundaryAddLine(lines,mas->numLines,cx + lastoy, cy - lastox, cx+oy, cy-ox);

					BoundaryAddLine(lines,mas->numLines,cx+ox, cy-oy, cx + lastox, cy - lastoy);
					BoundaryAddLine(lines,mas->numLines,cx - lastox, cy - lastoy, cx-ox, cy-oy);

					BoundaryAddLine(lines,mas->numLines,cx - lastoy, cy + lastox, cx-oy, cy+ox);
					BoundaryAddLine(lines,mas->numLines,cx-oy, cy-ox, cx - lastoy, cy - lastox);

					lastox = ox;
					lastoy = oy;
				}

			} else {

				Coord ox, oy;
				Int32 angle = (Int32)a->start;

				if (segNum == 0) {

					/*
					 * first segment, position cursor to start
					 * of arc
					 *
					 */

					IntSinCos(a->start, &sn, &cs);

					ox = (Coord) ((sn*r + ROUND(sn)) / COSINEZERO);
					oy = (Coord) ((cs*r + ROUND(cs)) / COSINEZERO);
					BoundarySetCursor(cx+ox, cy+oy);

				}

				if (a->radius > 0 ) {

					/*
					 * clockwise arc
					 *
					 */

					if (a->end < a->start) {

						/*
						 * must draw arc in two
						 * parts - draw up to
						 * 180 degrees first
						 *
						 */

						for ( ; angle < 32767; angle += stepSize) {

							IntSinCos((Int16)angle, &sn, &cs);

							ox = (Coord) ((sn*r + ROUND(sn)) / COSINEZERO);
							oy = (Coord) ((cs*r + ROUND(cs)) / COSINEZERO);
							BoundaryDrawLine(lines,mas->numLines,cx+ox, cy+oy);

						}

						angle -= 65536;

					}

					for (; angle < a->end; angle += stepSize) {

						IntSinCos((Int16)angle, &sn, &cs);

						ox = (Coord) ((sn*r + ROUND(sn)) / COSINEZERO);
						oy = (Coord) ((cs*r + ROUND(cs)) / COSINEZERO);
						BoundaryDrawLine(lines,mas->numLines,cx+ox, cy+oy);

					}

					angle += stepSize;

				} else {

					/*
					 * anti-clockwise arc
					 *
					 */

					if (a->end > a->start) {

						/*
						 * must draw arc in two
						 * parts - draw up to
						 * 180 degrees first
						 *
						 */

						for ( ; angle > -32768; angle -= stepSize) {

							IntSinCos((Int16)angle, &sn, &cs);

							ox = (Coord) ((sn*r + ROUND(sn)) / COSINEZERO);
							oy = (Coord) ((cs*r + ROUND(cs)) / COSINEZERO);
							BoundaryDrawLine(lines,mas->numLines,cx+ox, cy+oy);

						}

						angle = angle + 65536;

					}

					for (; angle > a->end; angle -= stepSize) {

						IntSinCos(angle, &sn, &cs);

						ox = (Coord) ((sn*r + ROUND(sn)) / COSINEZERO);
						oy = (Coord) ((cs*r + ROUND(cs)) / COSINEZERO);
						BoundaryDrawLine(lines,mas->numLines,cx+ox, cy+oy);

					}

					angle -= stepSize;

				}

				IntSinCos(a->end, &sn, &cs);

				ox = (Coord) ((sn*r + ROUND(sn)) / COSINEZERO);
				oy = (Coord) ((cs*r + ROUND(cs)) / COSINEZERO);
				BoundaryDrawLine(lines,mas->numLines,cx+ox, cy+oy);

			}

			segRecordPtr += sizeof(ArcSegmentType);

		} else {

			ModErrThrow();

		}

			LOGLINE;
		segmentCode++;
		segNum++;

	}

	LOGINT16(mas->numLines);

	/*
	 * if airspace has some line segments, then calculate its bounding
	 * box and label hint position
	 *
	 */

	LOGTIMESTOP;

	if (mas->numLines) {

		Coord minx = 32000, miny = 32000;
		Coord maxx = -32000, maxy = -32000;
		Int32 range, minRange = 2147483647;
		Int16 closestPoint = 0;
		Boolean locked = false;

		Coord xmid, ymid;
		Coord xvec, yvec;
		Int32 sf;
		Int16 j;

		mas->lines = PFMalloc(mas->numLines*sizeof(LineType));

		PFMemMove(mas->lines, lines, mas->numLines*sizeof(LineType));

		for (j=0;j<mas->numLines;j++) {

			Coord xmid, ymid;

			minx = MIN(minx, lines[j].x1);
			miny = MIN(miny, lines[j].y1);
			minx = MIN(minx, lines[j].x2);
			miny = MIN(miny, lines[j].y2);

			maxx = MAX(maxx, lines[j].x1);
			maxy = MAX(maxy, lines[j].y1);
			maxx = MAX(maxx, lines[j].x2);
			maxy = MAX(maxy, lines[j].y2);

			xmid = lines[j].x1 + (lines[j].x2 - lines[j].x1)/2;
			ymid = lines[j].y1 + (lines[j].y2 - lines[j].y1)/2;

			if (!locked) {

				range = (Int32)xmid*(Int32)xmid + (Int32)ymid*(Int32)ymid;
				if (range < minRange) {

					minRange = range;
					closestPoint = j;

				}

			}

		}

#ifdef XXXX
		if (maxx - minx < 30 && maxy-miny < 30) {

			PFMemFree(mas->lines);
			PFMemFree(lines);
			return false;

		}
#endif

		PFScreenRectangleSet(&mas->bounds, minx, miny, maxx, maxy);

		/*
		 * the hint is half way along the middle line of the boundary.
		 * we'll use j as the pointer to it.
		 *
		 */

		j = closestPoint;
		
		/*
		 * initialise vector, then scale it to length 12 (or 24 for
		 * high density) pixels
		 *
		 */
		
		xvec = (lines[j].x2 - lines[j].x1)/2;
		yvec = (lines[j].y2 - lines[j].y1)/2;
		xmid = xvec + lines[j].x1;
		ymid = yvec + lines[j].y1;
		
		/*
		 * label airspace inside the boundary. label airways
		 * across the segment.
		 *
		 */

		if (as->type & asTypeAirspace) {

			sf = (24 * 32768) / (Int32)sqrt(MAX(1,xvec*xvec + yvec*yvec));

			xvec = (Coord) (((Int32)xvec * sf) / 32768);
			yvec = (Coord) (((Int32)yvec * sf) / 32768);
			
			/*
			 * note the rotation by 90 degrees:
			 *
			 * 	xn = xcos + ysin, yn = ycos - xsin
			 * 
			 * Sin(90) = 1 and Cos(90) = 0 therefore xn = y, yn = -x
			 *
			 */

			mas->labelHint.x = xmid + yvec;
			mas->labelHint.y = ymid - xvec;

		} else {

			mas->labelHint.x = xmid;
			mas->labelHint.y = ymid;

		}

		PFMemFree(lines);
		return true;

	}

	/*
	 * airspace has no segments on screen
	 *
	 */

	PFMemFree(lines);

	return false;

}

/*
 * function : MapAirspaceDraw
 *
 */

Boolean MapAirspaceDraw(MapAirspaceType *this, Boolean border, Boolean thick, Boolean highlight, Boolean label, 
		Boolean (*tf)(Coord, Coord, Coord*, Coord*)) {

	LineType *lines = NULL;
	Int16  numLines;
	Int16 i;
	Coord minx = 640, miny = 640, maxx = -640, maxy = -640;
	Int16 colour, outline, outlineThickness;

	Int32 averagex = 0, averagey = 0;

	LOGENTRY;

	this->visible = false;

	/*
	 * filter by clipCount
	 *
	 */

	if (this->clipCount) {

		this->clipCount --;
		return true;

	}

	/*
	 * translate & rotate the airspace into screen coordinates
	 *
	 */

	numLines = this->numLines;
	PFSafeMalloc(lines, numLines*sizeof(LineType));

	for (i = 0; i < numLines; i++) {

		/*
		 * optimisation: can copy the last line's transformed
		 * points if it ends where this line begins
		 *
		 */

		if (i>0 && this->lines[i-1].x2 == this->lines[i].x1
			&& this->lines[i-1].y2 == this->lines[i].y1) {

			lines[i].x1 = lines[i-1].x2;
			lines[i].y1 = lines[i-1].y2;

		} else {

			tf(this->lines[i].x1, this->lines[i].y1,
					&lines[i].x1, &lines[i].y1);

			minx = MIN(minx, lines[i].x1);
			miny = MIN(miny, lines[i].y1);
			maxx = MAX(maxx, lines[i].x1);
			maxy = MAX(maxy, lines[i].y1);

		}

		tf(this->lines[i].x2, this->lines[i].y2,
				&lines[i].x2, &lines[i].y2);

		minx = MIN(minx, lines[i].x2);
		miny = MIN(miny, lines[i].y2);
		maxx = MAX(maxx, lines[i].x2);
		maxy = MAX(maxy, lines[i].y2);

		averagex += lines[i].x1;
		averagey += lines[i].y1;

	}

	/*
	 * clip airspace from screen
	 *
	 * Set clipCount according to how far off the screen the airspace is.
	 *
	 * Return false if the airspace is off the bottom of the screen
	 *
	 */

	if (maxx < 0) {
		
		Coord distance  = -maxx;

		this->clipCount = distance / 8;

	}

	if (minx > pfScreen.width) {
		
		Coord distance  = minx - pfScreen.width;

		this->clipCount = distance / 8;

	}

	if (maxy < 0) {
		
		Coord distance = -maxy;
		
		this->clipCount += distance / 8;
	}

	if (miny > pfScreen.height) {
		
		Coord distance = miny - pfScreen.height;
		
		this->clipCount += distance / 8;

	}

	if (this->clipCount) {

		this->clipCount += 3;

		PFMemFree(lines);
		LOGEXIT;

		return !(miny > pfScreen.height);

	}

	this->visible = true;

	/*
	 * choose colour
	 *
	 */

	LOGLINE;

	if (highlight) {

		colour = outline = AppColourPrefs.black;

	} else {

		if (this->type & asTypeSUAS)  {
			
			colour = AppColourPrefs.suas;
			outline = AppColourPrefs.airspaceOutline[7];
			
		} else if (this->type & (asTypeClassA | asTypeClassBG) ) {
			
			Int16 i;
			
			switch (this->type & asTypeMask) {

			case asTypeClassA:
				i = 0;
				break;

			case asTypeClassB:
			case asTypeClassC:
				i = 1;
				break;

			case asTypeClassD:
			case asTypeClassE:
				i = 3;
				break;

			default:
				i = 5;
				break;
			}
				
			colour = AppColourPrefs.airspace[i];
			outline = AppColourPrefs.airspaceOutline[i];

		} else if (this->type & asTypeAirway) {

			colour = AppColourPrefs.airway;
			outline = AppColourPrefs.airway;

		} else {

			colour =  AppColourPrefs.classOther;
			outline =  AppColourPrefs.classOther;

		}

	}

	outlineThickness = thick ? 2:1;

	LOGLINE;
	if (border) {

		PFDrawThickLines(lines, numLines, colour, outline, outlineThickness);

	} else {

		PFDrawThickLines(lines, numLines, colour, colour, 0);

	}

	if (label) {

		Coord width, height;
		Coord x = averagex / numLines;
		Coord y = averagey / numLines;

		FntSetFont(stdFont);

		tf(this->labelHint.x, this->labelHint.y, &x, &y);

		PFSetDrawMode(blockMode);
		PFSetTextColour(AppColourPrefs.black);
		PFSetForeColour(AppColourPrefs.black);

		/*
		 * airspace labels comprise upper/lower altitude, 
		 * whereas airways have only their name
		 *
		 */

		if (this->type & asTypeAirspace) {

			y -= FntLineHeight();

			width = MAX( FntCharsWidth(this->lowerAltString, StrLen(this->lowerAltString)),
					FntCharsWidth(this->upperAltString, StrLen(this->upperAltString)));

			height = FntLineHeight()*2;

			PFOutlineBitmapInit(width, height);
			DrawAlignedChars(this->upperAltString, ALIGNCENTRE,width/2,1);
			DrawAlignedChars(this->lowerAltString, ALIGNCENTRE,width/2,FntLineHeight()+1);
			PFDrawLine(1,FntLineHeight(),width, FntLineHeight());
			
			PFOutlineBitmapDraw(x - width/2, y);

		} else {

			height = FntLineHeight();
			width = FntCharsWidth(this->airwayIdent, StrLen(this->airwayIdent));
			y -= height/2;

			PFDrawOutlineChars(this->airwayIdent, ALIGNCENTRE,x,y);

		}

	}

	PFMemFree(lines);
	LOGEXIT;

	return true;

}
