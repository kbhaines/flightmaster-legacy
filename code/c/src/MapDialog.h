/*
 * MapDialog.h
 *
 */

#ifndef MAP_DIALOG_H_INCLUDED
#define MAP_DIALOG_H_INCLUDED

#include "Constants.h"
#include "WDManager.h"
#include "AsDatabase.h"
#include "TerrainType.h"

/*
 * shortcut to map settings in Preferences...
 *
 */

#define mapPrefs Preferences.mapSetting[Preferences.mapNumber]

typedef struct {

	Int16 scale;

	WaypointClassType icons;
	WaypointClassType labels;

	AirspaceClassType airspace;
	AirspaceClassType airspaceLabels;

	UInt8   lowerFilter;
	UInt8   upperFilter;

	Boolean showTrack;
	Boolean showZones;

	Boolean trackUp;
	Boolean showHeadingArc;

	Boolean trackLog;
	Boolean route;

	TerrainPaletteType terrain;
	float terrainRefAlt;			// reference altitude in feet

} MapSettingType;

extern Boolean MapDialogHandleEvent(EventPtr event) MODALDIALOGS_SECTION;

#endif
