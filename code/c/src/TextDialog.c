/*
 * TextDialog.c
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

#include "TextDialog.h"

#include "Utils.h"
#include "MathLib.h"
#include "Modules.h"
#include "AsDatabase.h"

#include "WPInfoDialog.h"

#define ModuleID TextDialogModuleID

/*******************************************************************************
 * 
 * global variables
 *
 * 
 */

extern const WDMHandle WPDataset;

/*******************************************************************************
 * 
 * module variables
 *
 * 
 */


/*
 * pointer to the notes, this must be initialised by a call to
 * TextDialogSet *before* the info dialog is displayed.
 *
 * The memory will be deallocated before the dialog closes.
 *
 */

static char *notes;

/*******************************************************************************
 *
 * private functions
 *
 * are declared here to allow easy relocation to another code section
 * 
 */

static void FormInit(void) MODALDIALOGS_SECTION;
static void FormDeinit(void) MODALDIALOGS_SECTION;
static Boolean ResizeForm(void) MODALDIALOGS_SECTION;

/*
 * function : FormInit
 *
 */

static void FormInit(void) {

	LOGENTRY;

	GUIFieldSetText(TextDialogField, notes, false);

	LOGEXIT;
	return;


#ifdef XXXX
	LOGEXIT;
#endif

}

/*
 * function : FormDeinit
 *
 */

static void FormDeinit(void) {

	PFMemFree(notes);

}

/*
 * function : ResizeForm
 *
 */

static Boolean ResizeForm() {

	Int16 ydelta, xdelta;

	if (!GUIFormResize(true, false)) {
	
		return false;

	}

	GUIFormResizeGetDeltas(&xdelta, &ydelta);
	
	GUIObjectResize(TextDialogField, xdelta, ydelta);
	
	return true;

}

/*******************************************************************************
 *
 * public functions
 *
 */

/*
 * function : TextDialogHandleEvent
 *
 */

Boolean TextDialogHandleEvent (EventPtr event)
{
	Boolean handled = false;

	LOGENTRY;

	switch (PFEventGetType(event)) 
	{
	case frmOpenEvent:
		ResizeForm();
		FormInit();
		GUIFormDraw();
		handled = true;
		break;

	case winDisplayChangedEvent:
		if (ResizeForm()) GUIFormDraw();
		handled = true;
		break;

	case penDownEvent:
		FormDeinit();
		GUIFormReturn();
		handled = true;
		break;

	case keyDownEvent:

		/*
		 * hard1/hard4 closes the dialog, up & down scroll.
		 *
		 * The other keys are ignored.
		 *
		 */

		switch (PFEventGetKeyChr(event)) {

#ifdef XXXX
		case 'i':
			wpID = WDMSearchForWaypointByLocation(WPDataset, "EGLL",0,0,0);
			if (wpID) { 
				WPInfoSetWaypointInfo(wpID);
				FormDeinit();
				GUIFormReturn();
				GUIFormPopup(WPInfoDialog);

			}
			break;
#endif

		}
		handled = true;
		break;
			
	case frmCloseEvent:
		FormDeinit();
		break;

	default:
		break;
	}
			
	LOGEXIT;

	return handled;
}

/*
 * function : TextDialogSet
 *
 */

void TextDialogSet(const char *in) {

	notes = PFMalloc(StrLen(in)+1);

	StrCopy(notes, in);

}
