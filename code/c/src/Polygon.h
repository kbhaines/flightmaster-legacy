/*
 * Polygon.h
 *
 */

#ifndef POLYGON_H_INCLUDED
#define POLYGON_H_INCLUDED

#include "Platform.h"
#include "Constants.h"
#include "Fixed.h"

typedef struct {
	FP14 x, y;
} PolyPointType;

extern void DrawPolygon(PolyPointType *poly, Int16 n);

#endif
