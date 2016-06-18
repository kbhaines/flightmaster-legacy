/*
 * AIGauge.h
 *
 */
#ifndef AIGAUGE_H_INCLUDED
#define AIGAUGE_H_INCLUDED

#include "Platform.h"
#include "Constants.h"

/*
 * function : AIGaugeInit
 *
 * Initialises the attitude indicator gauge
 *
 */
extern Err AIGaugeInit(Coord *radius) AI_SECTION;

/*
 * function : AIGaugeDraw
 *
 * Draw the attitude indicator. 
 *
 * roll and pitch are in radians, left/up is +ve
 *
 */

extern void AIGaugeDraw(Coord x, Coord y, double roll, double pitch) AI_SECTION;

/*
 * function : AIGaugeDeInit
 *
 */
extern void AIGaugeDeInit(void) AI_SECTION;

#endif
