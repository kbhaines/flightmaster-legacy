/*
 * DiversionMgr.h
 *
 * Manages diversion setup and clear-down, and the diversion keypresses
 *
 * Diversion setup is accomplished by directly modifying the contents of the
 * waypoint database. This involves inserting either one or two waypoints
 * (depending on divert type). This module needs to know the current GPS
 * position in order to work.
 *
 * (c)2003 Blackhawk Systems Ltd.
 *
 */

#ifndef DIVERSIONMGR_H_INCLUDED 
#define DIVERSIONMGR_H_INCLUDED

#include "Platform.h"
#include "Constants.h"
#include "GlobalTypes.h"


/*
 * function : DvEmergencyDiversion
 *
 */

extern Boolean DvEmergencyDiversion(void) DIVERSIONMGR_SECTION;

#endif
