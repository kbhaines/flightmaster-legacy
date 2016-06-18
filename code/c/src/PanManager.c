/********************************************************************************
 *
 * Pan Control State Machine
 *
 */

#include "PanManager.h"

/*******************************************************************************
 *
 * Module variables
 *
 */

static struct {

	enum { panOff, panInit, panFree, panPlan } state;

	Int32 lat;
	Int32 lon;

	Int32 lastLat, lastLon;

	Int16 waypointNum;

	Boolean v1;

} pan = { panOff, 0, 0, 0, 0, 99, false };


static const char *panModes[] = {

	NULL, "Scroll" ,"Scroll*", "Scroll/FP"

};


/*******************************************************************************
 * 
 * Internal/Helper Functions
 *
 */

static void PanSetToPlanWaypoint (FlightPlanType fp, Int16 wp) MAP_SECTION;
static void PanHandleCursorKey(WChar key, MapType map, ScreenInfoType lscreen) MAP_SECTION;


/*
 *  function: PanHandleKey
 *
 */

static void PanHandleCursorKey(WChar key, MapType map, ScreenInfoType lscreen) {

	Coord x,y;
	Coord pd = lscreen.width / 4;	// pd is pan distance
	Coord pdd = (3 * pd / 4);		// pdd is for diagonals

	switch (key) {

	case vchrPageUp: 
		x=0;y=-pd;
		break;

	case vchrUpLeft:
		x=-pdd;y=-pdd;
		break;

	case vchrRockerLeft:
		x=-pd;y=0;
		break;

	case vchrDownLeft:
		x=-pdd;y=pdd;
		break;

	case vchrPageDown:
		x=0;y=pd;
		break;

	case vchrDownRight:
		x=pdd;y=pdd;
		break;

	case vchrRockerRight:
		x=pd;y=0;
		break;

	case vchrUpRight:
		x=pdd;y=-pdd;
		break;

	default:
//		ModErrThrowIf(1);
		break;

	}

	MapGetLatLon(map, lscreen.xcentre + x, lscreen.ycentre + y, &pan.lat, &pan.lon);

}

/*
 *  function: PanSetToPlanWaypoint 
 *
 */

static void PanSetToPlanWaypoint(FlightPlanType fp, Int16 wpNum) {

	const FlightPlanLegWaypointType *wp;

	pan.waypointNum = wpNum;

	if (pan.waypointNum > FpGetNumLegs(fp)) {

		pan.waypointNum = 0;

	}  else if (pan.waypointNum < 0) {
		
		pan.waypointNum = FpGetNumLegs(fp);

	} 

	wp = FpGetWaypoint(fp, pan.waypointNum);
	pan.lat = wp->lat;
	pan.lon = wp->lon;

}

/*******************************************************************************
 *
 * Public functions
 *
 */


/*
 *  function: PanHandleKeyEvent
 *
 */

Boolean PanHandleKeyEvent(WChar key, const FlightPlanType fp, const GPSType gps, 
		Int16 planModeWaypointNum,	const MapType map, Boolean inPlanMode, ScreenInfoType lscreen) {

	Boolean handled = false;

	switch (pan.state) {

	case panOff:
		if (key == vchrRockerCenter && !inPlanMode) {
		   
			pan.state = panInit;
			pan.lat = gps.posn.lat32;
			pan.lon = gps.posn.lon32;

			if (pan.lastLat == 0 && pan.lastLon ==0) {

				pan.lastLat = pan.lat;
				pan.lastLon = pan.lon;

			}

			handled = true;

		} else if (key == vchrRockerCenter && inPlanMode) {

			if (!FpIsBlank(fp)) {

				pan.state = panInit;
				PanSetToPlanWaypoint(fp, planModeWaypointNum);

			}  else {

				if (!pan.lat || !pan.lon) {
					
					MapGetLatLon(map, lscreen.xcentre, lscreen.ycentre, &pan.lat, &pan.lon);
					
				}
				
				pan.state = panFree;

			}

			handled = true;

		}
		break;

	case panInit:
		if (key == vchrRockerCenter) {
		   
			if (!FpIsBlank(fp)) {

				PanSetToPlanWaypoint(fp, FpGetCurrentLeg(fp)+1);

				pan.v1 = false;
				pan.state = panPlan;

			} else {

				pan.lat = pan.lastLat;
				pan.lon = pan.lastLon;

				pan.state = panFree;
			}

			handled = true;

		} else if ( PFIsCursorKey(key) ) {

			PanHandleCursorKey(key, map, lscreen);

			pan.state = panFree;
			handled = true;

		}
		break;

	case panPlan:
		switch (key) {

		case vchrRockerCenter:
			if (!pan.v1 && pan.lastLat && pan.lastLon) {

				pan.lat = pan.lastLat;
				pan.lon = pan.lastLon;

			}

			pan.state = panFree;
			handled = true;
			break;

		case vchrRockerLeft:
		case vchrRockerRight:
			if (FpIsBlank(fp)) {

				pan.state = panFree;

			} else {

				Int16 newWp = pan.waypointNum;

				if (key == vchrRockerLeft) 
					newWp--;
				else
					newWp++;

				PanSetToPlanWaypoint(fp, newWp);
				pan.v1 = true;

			} 

			handled = true;
			break;

		}

		break;

	case panFree:
		if (key == vchrRockerCenter) {

			pan.state = panOff;

			pan.lastLat = pan.lat;
			pan.lastLon = pan.lon;

			handled = true;

		} else if (PFIsCursorKey(key)) {

			PanHandleCursorKey(key, map, lscreen);
			handled = true;

		}
		break;

	}

	return handled;
					
}

/*
 *  function: PanGetPosition
 *
 */

void PanGetPosition(Int32 *lat, Int32 *lon) {

	*lat = pan.lat;
	*lon = pan.lon;

}

/*
 * function: PanGetModeString
 *
 */

const char *PanGetModeString(void) {

	return panModes[pan.state];

}
/*
 *  function: PanForceActivate
 *
 */

void PanForceActivate(const Waypoint *wp) {
	
	pan.state = panFree;


	pan.lat = RAD_TO_INT32(wp->latitude);
	pan.lon = RAD_TO_INT32(wp->longitude);

}

/*
 *  function: PanForceDeactivate
 *
 */

void PanForceDeactivate(void) {

	if (pan.state > panOff) {

		pan.state = panOff;

	}

}
