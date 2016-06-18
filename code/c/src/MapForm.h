#ifndef MAPFORM_H_INCLUDED
#define MAPFORM_H_INCLUDED

#include "Platform.h"
#include "Constants.h"
#include "GlobalTypes.h"

extern Boolean MapFormHandleEvent(EventPtr event) MAP_SECTION;

extern Boolean MapCommandDialogHandleEvent(EventPtr event) MAP_SECTION;

extern void MapSetSelection(WaypointIDType wp) MAP_SECTION;

extern UInt16 MapUnitTestHead(TestActionType action, UInt16 extra, HostFILE *f) MAP_SECTION;
#endif
