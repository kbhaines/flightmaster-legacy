#ifndef UTILS_H
#define UTILS_H

#include "Platform.h"
#include <VFSMgr.h>
#include "Constants.h"

#define ALIGNLEFT 0
#define ALIGNRIGHT 1
#define ALIGNCENTRE 2

/*
 * function : StrToDouble
 *
 * Convert a string to a double - simple really :-)
 */
extern double StrToDouble(const char *str) UTILS_SECTION;

/*
 * function : DoubleToStr
 *
 * Converts the specified double into a string with the specified
 * number of decimal places
 */
extern char *DoubleToStr(double d,UInt16 dp) UTILS_SECTION;
extern char *FloatToStr(float d,UInt16 dp) UTILS_SECTION;

/*
 * function : FieldIsNumeric
 *
 * Returns true if the specified field contains only [0-9.-]
 */
extern Boolean StringIsNumeric(char *string) UTILS_SECTION;

/*
 * function : DrawAlignedChars
 *
 * Use ALIGNLEFT, ALIGNRIGHT or ALIGNCENTRE for align parameter
 *
 * Returns the X position at which the characters were actually drawn at for 
 * RIGHT, returns rightmost X position for LEFT/CENTRE aligned.
 */
extern Coord DrawAlignedChars(const char *text, UInt8 align, Coord x,Coord y) UTILS_SECTION;

/*
 * function : DrawLargeNumberSmallText
 *
 * Draws two strings: a number in LED font, and the text label
 * in std font.
 *
 * aligment should be ALIGNLEFT,ALIGNRIGHT or ALIGNCENTRE
 * 
 * Returns x position of smaller text
 *
 */
extern Coord DrawLargeNumberSmallText(
	const char *num, const char *txt, Coord x, 
	Coord y, UInt8 alignment,FontID fnt) UTILS_SECTION;

/*
 * function : GetStringFromList
 *
 * Gets a specific string from a list of strings which are
 * arranged back-to-back. 0 is the first string
 *
 */

extern /*@shared@*/ const char *GetStringFromList(const char *str, UInt16 stringNo) UTILS_SECTION;

/*
 * function : GetSinCos
 *
 * Returns integer representation of the sine and cosine of the angle (which is in degrees, 0-360).
 * 
 * Divide the returned values by COSINEZERO to yield a number between 0 and 1.
 *
 */

#define COSINEZERO ((Int32)32768*4)

extern void GetSinCos(Int16 angle, /*@out@*/ Int32 *sn, /*@out@*/ Int32 *cs) UTILS_SECTION;

/*
 * function : IntSinCos
 *
 * Returns sine and cosine of the angle (which is Integer -32767 to 32767). The returned
 * values are 0-32768, where 32768 = 1
 *
 */

extern void IntSinCos(Int16 angle, Int32 *sn, Int32 *cs) UTILS_SECTION;


/*
 * function : ClipLine16/32
 *
 * Clips the specified line and returns true if the line
 * is within the clipLimits
 *
 */

extern Boolean ClipLine16(Coord *x1, Coord *y1, Coord *x2, Coord *y2, Coord minx, Coord miny, Coord maxx, Coord maxy) UTILS_SECTION;
extern Boolean ClipLine32(Int32 *x1, Int32 *y1, Int32 *x2, Int32 *y2, Int32 minx, Int32 miny, Int32 maxx, Int32 maxy) UTILS_SECTION;

/*
 * function : IntSqrt
 *
 */

extern Int32 IntSqrt(Int32 r) UTILS_SECTION;

/*
 * function : DrawCircle
 *
 */

extern LineType *DrawCircle(Coord x, Coord y, Coord r, Int16 *numLines) UTILS_SECTION;

/*
 * function : DrawLines2d
 *
 */

extern void DrawLines2d(LineType *lines, Int16 numLines, IndexedColorType colour, Boolean dashed) UTILS_SECTION;

/*
 * function : MyBinarySearch
 *
 * OK, it's not really mine ;-)
 *
 * Function prototype as per SysBinarySearch in PalmOS
 *
 */

extern Boolean MyBinarySearch (void const * baseP, 
		const Int16 numOfElements, const Int16 width, 
		SearchFuncPtr searchF, void const * searchData, 
		const Int32 other, /*@out@*/ Int32 * position, 
		const Boolean findFirstNoLongerUsed ) UTILS_SECTION;

/*
 * EndianUtils
 *
 */

#define EndianSwap16(n) (((((unsigned int) n) << 8) & 0xFF00) | ((((unsigned int) n) >> 8) & 0x00FF))

#define EndianSwap32(n) (((((unsigned long) n) << 24) & 0xFF000000) | ((((unsigned long) n) <<  8) & 0x00FF0000) | ((((unsigned long) n) >>  8) & 0x0000FF00) | ((((unsigned long) n) >> 24) & 0x000000FF))

/*
 * function : TaskTimer
 *
 * Create, query or release a timer
 *
 * When creating a timer, pass NEWTASKTIMER for the timerRef value - the return value
 * will be the timer reference to be used to reference the timer.
 *
 * To query the timer, pass the timerRef value and done = false.
 *
 * To destroy the timer, pass the timerRef value and done = true.
 *
 * When querying or destroying the timer, the return value is the number
 * of ticks elapsed since the timer was created.
 *
 * Timers *must* be destroyed in the reverse order of their creation, and a maximum
 * of 16 timers may be running at once.
 *
 */

#define NEWTASKTIMER -1
extern Int32 TaskTimer(Int16 timerRef, Boolean done) UTILS_SECTION;

/*
 * function : DrawFlag
 *
 * Draws a flag at the specified location
 *
 */

void DrawFlag(const PointType *pos, const char *str, IndexedColorType colour) UTILS_SECTION;

//#define IsValidCode(code, id) (code == GenerateRegistrationCode(REGCODESEED, REGCODEPRIME, id))
#define IsValidCode(code, id) (1)
Boolean HandleDemoChecks(UInt32 code, const char *uid, UInt32 *demoDateLimit, UInt32 seed, 
		UInt32 prime, UInt32 yearEpoch) UTILS_SECTION;

#endif
