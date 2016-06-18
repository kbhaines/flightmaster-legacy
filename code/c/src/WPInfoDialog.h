/*
 * WPInfoDialog.h
 *
 * Waypoint Info Dialog displays the information about a waypoint, and can format the
 * information to show just the frequencies or just the runway information at the
 * users request.
 *
 * This dialog should be opened via a call to FrmPopupForm, not FrmGotoForm.
 *
 */

#ifndef WPINFO_H_INCLUDED
#define WPINFO_H_INCLUDED

#include "Platform.h"
#include "Constants.h"
#include "GlobalTypes.h"
#include "WDManager.h"

/*
 * function : WPInfoDialogHandleEvent
 *
 */

extern Boolean WPInfoDialogHandleEvent(EventPtr event) MODALDIALOGS_SECTION;

/*
 * function : WPInfoSetWaypointInfo
 *
 * This function must be called *immediately before* displaying the WPInfo
 * dialog.
 *
 */

extern void WPInfoSetWaypointInfo(WaypointIDType wp) MODALDIALOGS_SECTION;

#endif
