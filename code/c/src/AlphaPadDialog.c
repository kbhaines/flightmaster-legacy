/*
 * AlphaPadDialog.c
 *
 * (c) 2003 Blackhawk Systems
 *
 */
#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif

#include "Platform.h"
#include "GlobalTypes.h"
#include "ResourceDefines.h"

#include "AlphaPadDialog.h"

#include "Utils.h"

#include "Modules.h"

#define ModuleID AlphaPadDialogModuleID

/*******************************************************************************
 * 
 * module variables
 *
 * 
 */

static char *inputString;

/*******************************************************************************
 *
 * private functions
 *
 * are declared here to allow easy relocation to another code section
 * 
 */

/*******************************************************************************
 *
 * public functions
 *
 */

/*
 * function : AlphaPadDialogHandleEvent
 *
 */

Boolean AlphaPadDialogHandleEvent(EventPtr event) {

	Boolean       handled        = false;
	WChar chrToSend;
	
	switch (PFEventGetType(event)) {

	case frmOpenEvent:
		GUIFormResize(true, true);
		GUIFieldSetText(AlphaPadField, inputString, false);
		GUIFocusSet( AlphaPadField);
		GUIFormDraw();
		handled = true;
		break;

	case winDisplayChangedEvent:
		GUIFormResize(true, true);
		GUIFormDraw();
		handled = true;
		break;

	case ctlSelectEvent:

		if (PFEventGetControlSelectID(event) == AlphaPadOK) {

			char *fieldStr = GUIFieldGetText(AlphaPadField);

			if (fieldStr) {

				inputString = PFMalloc(StrLen(fieldStr)+1);
				StrCopy(inputString, fieldStr);

			} else {

				inputString = NULL;

			}

			GUIFormReturn();
			PFSendSimpleEvent(evtAlphaInput);
			return true;

		}

		if (PFEventGetControlSelectID(event) >= AlphaPadA &&
				PFEventGetControlSelectID(event) < AlphaPadA+26) {

			chrToSend = 'A' + PFEventGetControlSelectID(event) - AlphaPadA;

		} else if (PFEventGetControlSelectID(event) >= AlphaPad0 &&
				PFEventGetControlSelectID(event) < AlphaPad0+10) {

			chrToSend = '0' + PFEventGetControlSelectID(event) - AlphaPad0;

		} else {
			
			switch (PFEventGetControlSelectID(event) ) {

			case AlphaPadDash:
				chrToSend = '-';
				break;

			case AlphaPadSpace:
				chrToSend = ' ';
				break;

			case AlphaPadBS:
				chrToSend = 8;
				break;

			case AlphaPadClear:
				GUIFieldSetText(AlphaPadField, "", true);
				chrToSend = 8;
				break;

			default:
				ErrThrow(998);
				break;

			}

		}

		PFEventSendKey(chrToSend);

		handled = true;
		break;

	default:
		break;
	}
	return handled;

}

/*
 * function : AlphaPadDialogInit
 *
 */

void AlphaPadDialogInit(char *initialValue) {

	inputString = initialValue;

}

char *AlphaPadGetInput(void) {

	return inputString;

}
