/*
 * Utils.c
 *
 * Copyright (c) 2003 Blackhawk Systems Ltd
 * Portions Copyright (c) 2000-2001, Neil Rhodes and Julie McKeehan
 * neil@pobox.com All rights reserved.  From the book "Palm Programming, the
 * Developer's Guide, 2nd edition"by O'Reilly.
 */

#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif
#include "Platform.h"
#include <PalmOSGlue.h>
#include <PalmCompatibility.h>
#include "Utils.h"
#include "ResourceDefines.h"
#include <FloatMgr.h>
#include "MathLib.h"

#include "GlobalTypes.h"

#include "RegistrationCode.c"
#include "Modules.h"

#define ModuleID UtilsModuleID


/* 
 * function : StrToDouble
 */
double StrToDouble(const char *str) {
	FlpCompDouble result;
	double d;

	FlpBufferAToF(&result.fd,str);

	d = result.d;
	return d;
}

/*
 * function : DoubleToStr
 */

char *DoubleToStr(double d,UInt16 dp) {
	static char result[16];
	static Int32 dpTable[]={1,10,100,1000,10000,100000};
	Int32 i;
	UInt16 j,k;
	Boolean neg = false;

	if (d < 0.0) {

		d = -d;
		neg = true;
		result[0]='-';

	}

	i = (Int32)round((double)dpTable[dp]*d);

	if (dp>0) 
		k=StrPrintF(&result[neg?1:0],"%ld.%*ld",i/dpTable[dp],dp,i  % dpTable[dp]);
	else
		k=StrPrintF(&result[neg?1:0],"%ld",(Int32)round(d));

	for (j=0;j<k;j++)
		if (result[j]==' ')
			result[j]='0';

	return result;
}

/*
 * function : FloatToStr
 */
char *FloatToStr(float d,UInt16 dp) {

	static char result[16];
	static Int32 dpTable[]={1,10,100,1000,10000,100000};
	Int32 i = (Int32)round((double)dpTable[dp]*d);
	UInt16 j,k;

	if (dp>0) 
		k=StrPrintF(result,"%ld.%*ld",i/dpTable[dp],dp,
				((i>0) ? i : -i) % dpTable[dp]);
	else
		k=StrPrintF(result,"%ld",(Int32)round(d));

	for (j=0;j<k;j++)
		if (result[j]==' ')
			result[j]='0';

	return result;

}

/*
 * function : StringIsNumeric
 */
Boolean StringIsNumeric(char *str) {
	UInt16 j;
	UInt16 len = StrLen(str);

	for (j=0;j<len;j++) {
		if ((str[j]<'0' || str[j]>'9') && str[j] != '.' && str[j]!='-')
			return false;
	}
	return true;
}

/*
 * function DrawAlignedChars
 *
 */
Coord DrawAlignedChars(const char *text, UInt8 align, Coord x,Coord y) {
	Coord xpos = x;
	Coord xwidth;
	UInt8 len;
	
	LOGTIMESTART;

	if (text == NULL)
		return 0;

	len = StrLen(text);
	xwidth = FntCharsWidth(text,len);
	switch (align) {
		case ALIGNRIGHT: 
			xpos = x-xwidth;
			break;
		case ALIGNCENTRE: 
			xpos = x-xwidth/2;
			break;
	}

	PFPaintChars(text, len, xpos, y);
	LOGTIMESTOP;
	
	return xwidth;
//	if (align == ALIGNRIGHT)
//		return xpos;
//	else
//		return xpos+xwidth;


}

/*
 * function : DrawLargeNumberSmallText
 * 
 */

Coord DrawLargeNumberSmallText(const char *num, const char *txt, Coord x, 
		Coord y, UInt8 alignment,FontID fnt) {
	Coord xwidth;

#define DRAWFUNC PFDrawOutlineChars

	switch (alignment) {
	
	case ALIGNRIGHT:
		/* draw label first, then number */
		FntSetFont(stdFont);
		xwidth = DRAWFUNC(txt,ALIGNRIGHT,x,y);
		FntSetFont(fnt);
		xwidth += DRAWFUNC(num,ALIGNRIGHT, x-xwidth,y);
		break;
		
	case ALIGNLEFT:
		FntSetFont(fnt);
		xwidth = DRAWFUNC(num,ALIGNLEFT,x,y);
		FntSetFont(stdFont);
		xwidth += DRAWFUNC(txt,ALIGNLEFT,x+xwidth,y);
		break;
		
	case ALIGNCENTRE:
		FntSetFont(fnt);
		xwidth = DRAWFUNC(num,ALIGNCENTRE,x,y);
		FntSetFont(stdFont);
		xwidth += DRAWFUNC(txt, ALIGNLEFT, x+xwidth/2,y);
		break;
		
	}
	
	return xwidth;
	
}


/*
 * function : GetStringFromList
 *
 */

const char *GetStringFromList(const char *str, UInt16 stringNo) {

	UInt16 j;

	for (j=0;j<stringNo;j++)
		str += StrLen(str)+1;

	return str;
}

/*
 * function : GetSinCos
 *
 */

/*
 * these constants were generated by utils/sins.pl
 *
 */

#define SINTABSIZE (256*8)
static const Int32 sine[SINTABSIZE/8+1] = { 0, 402, 804, 1206, 1608, 2011, 2413, 2815, 3217, 3619, 4021, 4423, 4824, 5226, 5628, 6030, 6431, 6833, 7235, 7636, 8037, 8439, 8840, 9241, 9642, 10043, 10444, 10845, 11246, 11646, 12047, 12447, 12847, 13247, 13647, 14047, 14447, 14847, 15246, 15645, 16045, 16444, 16843, 17241, 17640, 18038, 18436, 18834, 19232, 19630, 20027, 20425, 20822, 21219, 21615, 22012, 22408, 22804, 23200, 23596, 23991, 24387, 24782, 25176, 25571, 25965, 26359, 26753, 27147, 27540, 27933, 28326, 28718, 29110, 29502, 29894, 30285, 30676, 31067, 31458, 31848, 32238, 32627, 33017, 33406, 33794, 34183, 34571, 34959, 35346, 35733, 36120, 36506, 36892, 37278, 37663, 38048, 38433, 38817, 39201, 39585, 39968, 40350, 40733, 41115, 41497, 41878, 42259, 42639, 43019, 43399, 43778, 44157, 44535, 44913, 45291, 45668, 46045, 46421, 46797, 47172, 47547, 47922, 48296, 48669, 49042, 49415, 49787, 50159, 50530, 50901, 51271, 51641, 52011, 52380, 52748, 53116, 53483, 53850, 54216, 54582, 54948, 55312, 55677, 56041, 56404, 56766, 57129, 57490, 57851, 58212, 58572, 58931, 59290, 59649, 60007, 60364, 60720, 61076, 61432, 61787, 62141, 62495, 62848, 63201, 63553, 63904, 64255, 64605, 64955, 65304, 65652, 66000, 66347, 66693, 67039, 67384, 67729, 68073, 68416, 68759, 69101, 69442, 69783, 70123, 70463, 70801, 71139, 71477, 71814, 72150, 72485, 72820, 73154, 73487, 73820, 74152, 74483, 74813, 75143, 75472, 75801, 76128, 76455, 76782, 77107, 77432, 77756, 78079, 78402, 78724, 79045, 79366, 79685, 80004, 80322, 80640, 80956, 81272, 81587, 81902, 82215, 82528, 82840, 83151, 83462, 83771, 84080, 84388, 84696, 85002, 85308, 85613, 85917, 86220, 86523, 86824, 87125, 87425, 87724, 88023, 88320, 88617, 88913, 89208, 89502, 89795, 90088, 90379, 90670, 90960, 91249, 91538, 91825, 92111, 92397, 92682 };
static const Int32 cosine[SINTABSIZE/8+1] = { 131072, 131071, 131069, 131066, 131062, 131056, 131049, 131041, 131032, 131022, 131010, 130997, 130983, 130967, 130951, 130933, 130914, 130893, 130872, 130849, 130825, 130800, 130773, 130745, 130716, 130686, 130655, 130622, 130588, 130553, 130517, 130479, 130440, 130400, 130359, 130317, 130273, 130228, 130182, 130134, 130086, 130036, 129985, 129933, 129879, 129824, 129768, 129711, 129653, 129593, 129532, 129470, 129407, 129343, 129277, 129210, 129142, 129072, 129002, 128930, 128857, 128783, 128707, 128631, 128553, 128474, 128394, 128312, 128230, 128146, 128061, 127974, 127887, 127798, 127708, 127617, 127525, 127431, 127336, 127241, 127143, 127045, 126946, 126845, 126743, 126640, 126536, 126430, 126324, 126216, 126107, 125996, 125885, 125772, 125659, 125544, 125428, 125310, 125192, 125072, 124951, 124829, 124706, 124582, 124456, 124329, 124201, 124072, 123942, 123811, 123678, 123544, 123410, 123274, 123136, 122998, 122858, 122718, 122576, 122433, 122289, 122143, 121997, 121849, 121701, 121551, 121400, 121248, 121094, 120940, 120784, 120627, 120470, 120311, 120150, 119989, 119827, 119663, 119499, 119333, 119166, 118998, 118829, 118659, 118487, 118315, 118141, 117966, 117791, 117614, 117436, 117256, 117076, 116895, 116712, 116529, 116344, 116158, 115972, 115784, 115595, 115405, 115213, 115021, 114828, 114633, 114438, 114241, 114044, 113845, 113645, 113444, 113242, 113039, 112835, 112630, 112424, 112216, 112008, 111799, 111588, 111377, 111164, 110951, 110736, 110520, 110304, 110086, 109867, 109647, 109427, 109205, 108982, 108758, 108533, 108307, 108080, 107852, 107623, 107393, 107162, 106930, 106697, 106463, 106228, 105992, 105755, 105517, 105278, 105037, 104796, 104554, 104311, 104067, 103822, 103577, 103330, 103082, 102833, 102583, 102332, 102081, 101828, 101574, 101320, 101064, 100807, 100550, 100292, 100032, 99772, 99511, 99248, 98985, 98721, 98456, 98190, 97923, 97656, 97387, 97117, 96847, 96576, 96303, 96030, 95756, 95481, 95205, 94928, 94650, 94372, 94092, 93812, 93530, 93248, 92965, 92681 };

void GetSinCos(Int16 angle, Int32 *sn, Int32 *cs) {


	Int16 i = (Int16) ((Int32)angle*SINTABSIZE/360);

	/*
	 * in this function 0 to 512 represents 0 to 360 degrees, ergo 
	 * 90deg = 128, 180deg = 256 etc..
	 *
	 */

	if (i >= SINTABSIZE/2) i -= SINTABSIZE/2;

	if (i < SINTABSIZE/8) {

		*sn = sine[i];
		*cs = cosine[i];
	
	} else if (i < SINTABSIZE/4) {

		*sn = cosine[SINTABSIZE/4-1 - i];
		*cs = sine[SINTABSIZE/4-1 - i];

	} else if (i < 3*SINTABSIZE/8) {

		*sn = cosine[i-SINTABSIZE/4];
		*cs = -sine[i-SINTABSIZE/4];

	} else {

		*sn = sine[SINTABSIZE/2-1-i];
		*cs = -cosine[SINTABSIZE/2-1-i];

	}

	if (angle >= 180) {

		*sn = -*sn;
		*cs = -*cs;

	}

}

/*
 * function : IntSinCos
 *
 */

void IntSinCos(Int16 angle, Int32 *sn, Int32 *cs) {

	/*
	 * index is angle shifted left 6 bits to leave
	 * 10 bits (0-1023)
	 *
	 */

	Int16 i;

	if (angle < 0)
		
		angle = (angle - 16) /32;
	
	else 
		
		angle = (angle + 16) / 32;

	i = angle;

	//double a = ((double)angle / 32768)*PI;

	//*sn = COSINEZERO*sin(a);
	//*cs = COSINEZERO*cos(a);
	//return;

	/*
	 * in this function 0 to SINTABSIZE represents 0 to 360 degrees, ergo 
	 * 90deg = SINTABSIZE/4, 180deg = SINTABSIZE/2 etc..
	 *
	 */

	if (i<0) i += SINTABSIZE;

	if (i > SINTABSIZE/2) i -= SINTABSIZE/2;

	if (i < SINTABSIZE/8) {

		*sn = sine[i];
		*cs = cosine[i];
	
	} else if (i < SINTABSIZE/4) {

		*sn = cosine[SINTABSIZE/4 - i];
		*cs = sine[SINTABSIZE/4 - i];

	} else if (i < 3*SINTABSIZE/8) {

		*sn = cosine[i-SINTABSIZE/4];
		*cs = -sine[i-SINTABSIZE/4];

	} else {

		*sn = sine[SINTABSIZE/2-i];
		*cs = -cosine[SINTABSIZE/2-i];

	}

	if (angle < 0) {

		*sn = -*sn;
		*cs = -*cs;

	}

}



/*
 * function : ClippedOutCode16/32
 *
 * Helper function for DrawClippedLine, part of the Cohen-Sutherland line
 * clipping algorithm.
 *
 */


#define TOP 0x08
#define BOTTOM 0x04
#define LEFT 0x01
#define RIGHT 0x02
#define INSIDE 0x00

#define ACCEPT(c1,c2) (((c1)|(c2))==0)
#define REJECT(c1,c2) ((c1)&(c2))

static UInt8 ClippedOutCode16(Coord x, Coord y, Coord minx, Coord miny, Coord maxx, Coord maxy) {

	UInt8 out;

	if (y<miny)  // miny
		out = BOTTOM ;
	else if (y>maxy)  // maxy
		out = TOP;
	else 
		out = INSIDE;
	
	if (x>maxx) //maxx
		out |= RIGHT;
	else if (x<minx) //minx
		out|=LEFT;

	return out;
}
	
static UInt8 ClippedOutCode32(Int32 x, Int32 y, Int32 minx, Int32 miny, Int32 maxx, Int32 maxy) {

	UInt8 out;

	if (y<miny)  // miny
		out = BOTTOM ;
	else if (y>maxy)  // maxy
		out = TOP;
	else 
		out = INSIDE;
	
	if (x>maxx) //maxx
		out |= RIGHT;
	else if (x<minx) //minx
		out|=LEFT;

	return out;
}
	
/*
 * function : ClipLine16/32
 *
 * Implementation of the Cohen-Sutherland line clipping algorithm. 
 *
 */

Boolean ClipLine16(Coord *x1, Coord *y1, Coord *x2, Coord *y2, Coord minx, Coord miny, Coord maxx, Coord maxy) {

	UInt8 outcode1 = ClippedOutCode16(*x1,*y1,minx, miny, maxx,maxy);
	UInt8 outcode2 = ClippedOutCode16(*x2,*y2,minx, miny, maxx,maxy);
	Int32 tmp;
	UInt8 tmpo;

	if (!REJECT(outcode1,outcode2)) {

		Int32 xc1 = *x1;
		Int32 yc1 = *y1;
		Int32 xc2 = *x2;
		Int32 yc2 = *y2;

		while (!ACCEPT(outcode1,outcode2) && !REJECT(outcode1,outcode2)) {

			if (outcode1 == INSIDE) {
				tmp = xc1; xc1 = xc2; xc2 = tmp;
				tmp = yc1; yc1 = yc2; yc2 = tmp;
				tmpo = outcode1; outcode1=outcode2; outcode2 = tmpo;
			}

			if (outcode1 & TOP) {
				xc1 += (xc2-xc1)*(maxy-yc1)/(yc2-yc1);
				yc1 = maxy;
			} else if (outcode1 & BOTTOM) {
				xc1 += (xc2-xc1)*(miny-yc1)/(yc2-yc1);
				yc1 = miny;
			} else if (outcode1 & RIGHT) {
				yc1 += (yc2-yc1)*(maxx-xc1)/(xc2-xc1);
				xc1 = maxx;
			} else if (outcode1 & LEFT) {
				yc1 +=(yc2-yc1)*(minx-xc1)/(xc2-xc1);
				xc1 = minx;
			}
			outcode1 = ClippedOutCode16(xc1,yc1,minx, miny, maxx, maxy);
		}
		*x1 = xc1;
		*y1 = yc1;
		*x2 = xc2;
		*y2 = yc2;
	}

	return ACCEPT(outcode1,outcode2);


}


Boolean ClipLine32(Int32 *x1, Int32 *y1, Int32 *x2, Int32 *y2, Int32 minx, Int32 miny, Int32 maxx, Int32 maxy) {

	UInt8 outcode1 = ClippedOutCode32(*x1,*y1,minx, miny, maxx,maxy);
	UInt8 outcode2 = ClippedOutCode32(*x2,*y2,minx, miny, maxx,maxy);
	Int32 tmp;
	UInt8 tmpo;

	if (!REJECT(outcode1,outcode2)) {

		Int64 xc1 = *x1;
		Int64 yc1 = *y1;
		Int64 xc2 = *x2;
		Int64 yc2 = *y2;

		while (!ACCEPT(outcode1,outcode2) && !REJECT(outcode1,outcode2)) {

			if (outcode1 == INSIDE) {
				tmp = xc1; xc1 = xc2; xc2 = tmp;
				tmp = yc1; yc1 = yc2; yc2 = tmp;
				tmpo = outcode1; outcode1=outcode2; outcode2 = tmpo;
			}

			if (outcode1 & TOP) {
				xc1 += (xc2-xc1)*(maxy-yc1)/(yc2-yc1);
				yc1 = maxy;
			} else if (outcode1 & BOTTOM) {
				xc1 += (xc2-xc1)*(miny-yc1)/(yc2-yc1);
				yc1 = miny;
			} else if (outcode1 & RIGHT) {
				yc1 += (yc2-yc1)*(maxx-xc1)/(xc2-xc1);
				xc1 = maxx;
			} else if (outcode1 & LEFT) {
				yc1 +=(yc2-yc1)*(minx-xc1)/(xc2-xc1);
				xc1 = minx;
			}
			outcode1 = ClippedOutCode32(xc1,yc1,minx,miny,maxx,maxy);
		}
		*x1 = xc1;
		*y1 = yc1;
		*x2 = xc2;
		*y2 = yc2;
	}

	return ACCEPT(outcode1,outcode2);

}

/*
 * function : IntSqrt
 *
 * Source: http://astronomy.swin.edu.au/~pbourke/analysis/sqrt/
 *
 */

Int32 IntSqrt(Int32 r) {

   Int32 t,b,c=0;

   for (b=0x10000000;b!=0;b>>=2) {
      t = c + b;
      c >>= 1;
      if (t <= r) {
         r -= t;
         c += b;
      }
   }

   return(c);

}

/*
 * function : DrawCircle
 *
 */

LineType *DrawCircle(Coord cx, Coord cy, Coord r, Int16 *numLines) {

#define STORELINE(xl1,yl1,xl2,yl2) lines[i].x1 = xl1; lines[i].y1=yl1; lines[i].x2=xl2; lines[i].y2=yl2; i++

	Int16 angle = 0;
	Coord lastox, lastoy;
	Int16 stepSize;
	Int16 i = 0;
	static LineType lines[48];
	const Int16 referenceRadius = 40;
	
	lastox = 0;
	lastoy = -r;


	if (r < referenceRadius / 2)

		if (r < referenceRadius / 4)

			stepSize = 32768/8;

		else

			stepSize = 32768/12;

	else

		stepSize = 32768/16;

	for (angle = stepSize; angle <= 32768/4;angle += stepSize) {

		Coord ox, oy;
		Int32 sn, cs;
		IntSinCos(angle, &sn, &cs);

		ox = (Coord) ((sn*r + (COSINEZERO/2)) / COSINEZERO);
		oy = -(Coord) ((cs*r + (COSINEZERO/2)) / COSINEZERO);

		/*
		 * top, right, bottom, left, in pairs
		 *
		 */

		STORELINE(cx + lastox, cy + lastoy, cx+ox, cy+oy);
		STORELINE(cx-ox, cy+oy, cx - lastox, cy + lastoy);
		
		STORELINE(cx+oy, cy+ox, cx + lastoy, cy + lastox);
		STORELINE(cx + lastoy, cy - lastox, cx+oy, cy-ox);

		STORELINE(cx+ox, cy-oy, cx + lastox, cy - lastoy);
		STORELINE(cx - lastox, cy - lastoy, cx-ox, cy-oy);

		STORELINE(cx - lastoy, cy + lastox, cx-oy, cy+ox);
		STORELINE(cx-oy, cy-ox, cx - lastoy, cy - lastox);

		lastox = ox;
		lastoy = oy;
	}

	*numLines = i;
	return lines;

}

/*
 * function : DrawLine2d
 *
 * Directly accesses screen memory to draw lines really
 * fast
 *
 */

void DrawLines2d(LineType *lines, Int16 numLines, IndexedColorType colour, Boolean dashed) {

   Int16 i,j;
   Coord sx, sy;  /* step positive or negative (1 or -1) */
   Coord dx, dy;  /* delta (difference in X and Y between points) */
   Coord dx2, dy2;
   Coord e;
   Coord temp;
   Int32 x0, y0, x1, y1;
   Coord width, height;
 
   BitmapType *bm = WinGetBitmap(WinGetDrawWindow());
   UInt8 *screen = BmpGetBits(bm);
   UInt16 rowHeight;
   

   LOGENTRY;

   LOGINT16(x0); LOGINT16(y0);
   LOGINT16(x1); LOGINT16(y1);

   BmpGetDimensions(bm, &width, &height, &rowHeight);

   width--;
   height--;

   for (j=0;j<numLines;j++) {

	   x0 = lines[j].x1; y0 = lines[j].y1;
	   x1 = lines[j].x2; y1 = lines[j].y2;

	   if (!ClipLine32(&x0,&y0,&x1,&y1, 0,0, width, height)) {
		   
		   continue;

	   }

	   LOGINT16(x0); LOGINT16(y0);
	   LOGINT16(x1); LOGINT16(y1);

	   dx = x1 - x0;
	   sx = (dx > 0) ? 1 : -1;
	   if (dx < 0)
	       dx = -dx;
	 
	   dy = y1 - y0;
	   sy = (dy > 0) ? 1 : -1;
	   if (dy < 0)
	       dy = -dy;
	 
	   if (dx == 0 && dy == 0) continue;

	   dx2 = dx * 2 ; /* dx2 = 2 * dx */
	   dy2 = dy * 2; /* dy2 = 2 * dy */
	 
	   if (dy <= dx) { /* steep */
	       e = dy2 - dx;
	 
		   if (dashed) {

			   for (i = 0; i <= dx; ++i) {
				   if (i & 3) screen[x0+y0*rowHeight]= colour;
			 
				   while (e >= 0) {
					   y0 += sy;
					   e -= dx2;
				   }
			 
				   x0 += sx;
				   e += dy2;
			   }

			} else {

			   for (i = 0; i <= dx; ++i) {
				   screen[x0+y0*rowHeight]= colour;
			 
				   while (e >= 0) {
					   y0 += sy;
					   e -= dx2;
				   }
			 
				   x0 += sx;
				   e += dy2;
			   }

		  }	

	   } else {
	       /* swap x0 <-> y0 */
	       temp = x0;
	       x0 = y0;
	       y0 = temp;
	 
	       /* swap dx <-> dy */
	       temp = dx;
	       dx = dy;
	       dy = temp;
	 
	       /* swap dx2 <-> dy2 */
	       temp = dx2;
	       dx2 = dy2;
	       dy2 = temp;
	 
	       /* swap sx <-> sy */
	       temp = sx;
	       sx = sy;
	       sy = temp;
	 
	       e = dy2 - dx;
	 
		   if (dashed) {

			   for (i = 0; i <= dx; ++i) {
				   if (i & 3) screen[y0+x0*rowHeight] = colour;
			 
				   while (e >= 0) {
					   y0 += sy;
					   e -= dx2;
				   }
			 
				   x0 += sx;
				   e += dy2;
			   }

		   } else {

			   for (i = 0; i <= dx; ++i) {
				   screen[y0+x0*rowHeight] = colour;
			 
				   while (e >= 0) {
					   y0 += sy;
					   e -= dx2;
				   }
			 
				   x0 += sx;
				   e += dy2;
			   }

		  }

	   }

   }

   LOGEXIT;

}

/*
 * function : MyBinarySearch
 *
 */

Boolean MyBinarySearch (void const * baseP, 
		const Int16 numOfElements, const Int16 width, 
		SearchFuncPtr searchF, void const * searchData, 
		const Int32 other, Int32 * position, 
		const Boolean findFirstNoLongerUsed ) {

// note: findFirst (last param) is now ignored, algorithm always returns
// leftmost match
// note(2): please don't write to me and tell me this algorithm can be
// 'improved' by paying attention to this.  Do the analysis!

	Int32 lPos = 0;
	Int32 rPos = numOfElements - 1;
	Int32 mPos;

	// keep going until the pointers cross, that way
	// you get the index of smallest (leftmost) elt >= item
	// NOTE: (may return Length(array))
	while (lPos <= rPos)
	{
		mPos = (lPos + rPos) / 2;

		// mid value < search value
		if (searchF(searchData, (UInt8 *)baseP+mPos*width, other) > 0)
			lPos = mPos + 1;
		else
			rPos = mPos - 1;
	}
	*position = lPos;

	// if we've gone off the right end, don't do the final compare
	if (lPos >= numOfElements)
		return false;

	// otherwise, may not have checked current element for equality
	// so do it now
	return (searchF(searchData, (UInt8 *)baseP+lPos*width, other) == 0);
}

/*
 * function : TaskTimer
 *
 */

#define MAX_TIMERS 16

Int32 TaskTimer(Int16 timerRef, Boolean done) {

	static Int32 timers[MAX_TIMERS];
	static Int16 timerNo = 0;
	Int32 elapsedTime;

	if (timerRef == NEWTASKTIMER) {

		if (timerNo == MAX_TIMERS-1) ErrThrow(0x12345);

		timers[timerNo] = PFGetTicks();
		return timerNo ++;

	}

	elapsedTime = PFGetTicks() - timers[timerRef];

	if (done) timerNo--;

	if (timerNo <0) ErrThrow(0x12345);

	return elapsedTime;

}


/*
 * function : DrawFlag
 *
 */


void DrawFlag(const PointType *pos, const char *str, IndexedColorType colour)  {

	PFScreenRectType r;
	UInt16 length = StrLen(str);

	LOGENTRY;
	
	PFSetForeColour(colour);
	FntSetFont(stdFont);
	PFScreenRectangleSet(&r, pos->x, pos->y, 
			pos->x + FntCharsWidth(str,length)+4, pos->y + FntLineHeight()+4);

	PFPaintRectangle(&r,2);

	PFSetForeColour(GUIGetSystemColour(GUIObjectForeground));
	PFDrawChars(str,length,pos->x+2, pos->y+2);

	LOGEXIT;
}

/*
 * HandleDemoChecks
 *
 * Perform demo code checks, set ValidRegistration and display
 * appropriate dialog. Returns true if valid demo code.
 *
 */

Boolean HandleDemoChecks(UInt32 code, const char *uid, UInt32 *demoDateLimit, UInt32 seed, 
		UInt32 prime, UInt32 yearEpoch) {

	PFTimeStampType date; 
	UInt32   isoDate;	// in 20060606 format
	char limitStr[16];

	*demoDateLimit = GetDemoDate(code, uid, seed, prime, yearEpoch);

	PFSecondsToTimeStamp(PFGetSeconds(), &date);
	
	isoDate = ((UInt32)date.year) * 10000 + (UInt32)date.month * 100 + (UInt32)date.day;

	LOGINT32(isoDate);
	LOGINT32(*demoDateLimit);

	StrPrintF(limitStr,"%ld-%ld-%ld", (*demoDateLimit/10000), (*demoDateLimit / 100) % 100, (*demoDateLimit % 100));

	if (isoDate >= *demoDateLimit) {

		GUICustomAlertShow(DemoExpiredAlert, limitStr, NULL, NULL);
		return false;

	} else {

		GUICustomAlertShow(DemoLeftAlert, limitStr, NULL, NULL);
		return true;

	}

}

