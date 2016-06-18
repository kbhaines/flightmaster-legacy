#ifndef EDITWAYPOINTFORM_H
#define EDITWAYPOINTFORM_H

#include "Constants.h"
#include "GlobalTypes.h"

extern Boolean EditWaypointFormHandleEvent(EventPtr eventP) EDITWAYPOINT_SECTION;

/*
 * function : EditWaypointFormSetup
 *
 * Must be called before the Edit Waypoint form is opened
 *
 * inWp is pointer to the existing waypoint, if it is NULL then the waypoint
 * must be a new one: the fields in the form will be blank, or filled with the
 * current GPS data (if the GPS is available). This function copies the
 * waypoint so you can free the memory after the function is called.
 *
 * cb is a pointer to a callback function that will accept the editted
 * waypoint, this function will be called when the user exits the dialog. The
 * function is called twice - once with validate=true, to allow the function to
 * perform extra validation on the waypoint, and once with validate=false as
 * the form is closing. When validate=true the function should return true if
 * the waypoint is OK to use, or false if not. The callback should show an
 * error dialog before returning false.
 *
 * If the user presses Delete then wp will be null and deleted will be true
 * (the callback is only called once in this case). If cancel is pressed wp
 * will be null and deleted will be false (again, callback is only invoked
 * once).  If OK is pressed then wp will point to the new waypoint data. The
 * callback can validate the waypoint and return FALSE to prevent the dialog
 * closing.
 *
 * wp is deallocated as the form closes, so you must copy the waypoint
 * somewhere else during the callback.
 * 
 */

extern void EditWaypointFormSetup(const Waypoint *inWp, 
		Boolean(*cb)(Boolean validate, const Waypoint *wp, UInt16 size, Boolean deleted)) EDITWAYPOINT_SECTION;
#endif
