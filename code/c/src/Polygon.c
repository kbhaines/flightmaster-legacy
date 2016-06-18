/*
 * Polygon.c
 *
 */

#include "Platform.h"
#include "GlobalTypes.h"
#include "Polygon.h"

#define swap(type, a, b)	\
{			   \
	type _t_;   \
	_t_ = a;	\
	a = b;	  \
	b = _t_;	\
}

#define HEIGHT 480


typedef struct
{
	FP14 x;
} edgeRec;

static Coord adjust(FP14 val);

Coord adjust(FP14 val)
{
	if (FPFract(val) == FPHalf)
	{
		val++;
	}
	return (Coord) FP2Int(val + FPHalf);
}

static void span(Coord y, edgeRec *pt1, edgeRec *pt2)
{
	Coord ix1, ix2;

	/*
	if (pt2->x < pt1->x)
	{
		swap(edgeRec *, pt1, pt2);
	}
	*/

	ix1 = adjust(pt1->x);
	ix2 = adjust(pt2->x);

	/*
	idx = ix2 - ix1;

	if (idx == 0)
	{
		return;
	}
	*/

	PFDrawLine(ix1,y,ix2,y);
}

static void edge(edgeRec *table, PolyPointType *pt1, PolyPointType *pt2)
{
	FP14 x, dx;
	Coord idy, iy1, iy2;

	if (pt2->y < pt1->y)
	{
		swap(PolyPointType *, pt1, pt2)
	}

	iy1 = adjust(pt1->y);
	iy2 = adjust(pt2->y);
	idy = iy2 - iy1;

	if (idy == 0)
	{
		return;
	}

	idy = MAX(2, idy - 1);

	x = pt1->x;
	dx = FPDiv((pt2->x - pt1->x), Int2FP(idy)); 

	do
	{
		if (iy1>=0) table[iy1].x = x;
		x += dx;
		iy1++;
	} while(iy1 < iy2 && iy1 < HEIGHT);
}

static edgeRec table1[HEIGHT];
static edgeRec table2[HEIGHT];

void DrawPolygon(PolyPointType *pnts, Int16 n) {

	Coord iy1, iy2;
	UInt16 imaxy, iminy;
	Coord pnt1, pnt2;
	Int16 i;
	FP14 maxy, miny;

	if (n < 3)
	{
		return;
	}

	maxy = miny = pnts[0].y;
	imaxy = iminy = 0;

	for (i = 1; i < n; i++)
	{
		if (pnts[i].y > maxy)
		{
			maxy = pnts[i].y;
			imaxy = i;
		}

		if (pnts[i].y < miny)
		{
			miny = pnts[i].y;
			iminy = i;
		}
	}

	iy1 = adjust(miny);
	iy2 = adjust(maxy);

	if (iy1 == iy2)
	{
		return;
	}

	pnt1 = iminy;
	pnt2 = iminy + 1;
	if (pnt2 >= n)
	{
		pnt2 = 0;
	}

	do
	{
		edge(table1, &pnts[pnt1], &pnts[pnt2]);

		pnt1 = pnt2;
		pnt2 = pnt2 + 1;
		if (pnt2 >= n)
		{
			pnt2 = 0;
		}
	} while (pnt1 != imaxy);

	pnt1 = imaxy;
	pnt2 = imaxy + 1;
	if (pnt2 >= n)
	{
		pnt2 = 0;
	}

	do
	{
		edge(table2, &pnts[pnt1], &pnts[pnt2]);

		pnt1 = pnt2;
		pnt2 = pnt2 + 1;
		if (pnt2 >= n)
		{
			pnt2 = 0;
		}
	} while (pnt1 != iminy);

	if (iy1 < 0) {
		iy1 = 0;
	}
	do
	{
		span(iy1, &table1[iy1], &table2[iy1]);
		iy1++;
	} while (iy1 < iy2);
}
