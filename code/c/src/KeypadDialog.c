/*
 * KeypadDialog.c
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

#include "KeypadDialog.h"

#include "Utils.h"


/*******************************************************************************
 * 
 * module variables
 *
 * 
 */

#define MAXSTRLEN 8

static char inputString[MAXSTRLEN+1];
static const char *windowTitle;
static const char **buttons;
static Boolean (*keypadCallback)(UInt16,float);

/*******************************************************************************
 *
 * private functions
 *
 * are declared here to allow easy relocation to another code section
 * 
 */

static void FormInit(void) MODALDIALOGS_SECTION;

/*
 * FormInit
 *
 */

static void FormInit(void) {

	Int16 j;

	GUIFieldSetText(KPInputField, inputString, false);
	GUIFormSetTitle((char*)windowTitle);
	GUIFocusSet( KPInputField);

	GUIFieldSetSelection(KPInputField, 0, StrLen(inputString));

	/*
	 * set labels for the buttons (or hide them)
	 *
	 */
	
	for (j=0;j<4;j++) {

		if ( buttons[j] ) 

			GUIObjectSetLabel(KPUser1+j, buttons[j]);

		else

			GUIObjectHide(KPUser1+j);

	}

}

/*******************************************************************************
 *
 * public functions
 *
 */

/*
 * function : KeypadDialogHandleEvent
 *
 */
Boolean KeypadDialogHandleEvent(EventPtr event) {

	Boolean       handled        = false;
	WChar chrToSend;
	
	FntSetFont(largeBoldFont);

	switch (PFEventGetType(event)) {

	case frmOpenEvent:
		GUIFormResize(true, true);
		FormInit();
		GUIFormDraw();		
		handled = true;
		break;
		
	case winDisplayChangedEvent:
		GUIFormResize(true, true);
		GUIFormDraw();
		handled = true;
		break;

	case ctlSelectEvent:

		switch (PFEventGetControlSelectID(event)) {

		case KPUser1:
		case KPUser2:
		case KPUser3:
		case KPUser4:

			if (buttons[PFEventGetControlSelectID(event)-KPUser1] && 
					keypadCallback(PFEventGetControlSelectID(event), 
					 (float)StrToDouble(GUIFieldGetText(KPInputField))))  {

				GUIFormReturn();
				PFSendSimpleEvent(evtKeypadInput);

			}
			handled = true;
			break;
							 
		case KPDeleteButton:
			chrToSend = 8;
			break;

		case KPDecimalButton:
			chrToSend = '.';
			break;

		}

		if (PFEventGetControlSelectID(event) >= KPButton0 &&
				PFEventGetControlSelectID(event) <= KPButton9) {

			chrToSend = '0' + PFEventGetControlSelectID(event) - KPButton0;

		}

		if (!handled) PFEventSendKey(chrToSend);

		handled = true;

		break;

	default:
		break;

	}
	return handled;
}

/*
 * function : KeypadDialogInit
 *
 */

void KeypadDialogInit(Boolean (*callback)(UInt16,float), float initialValue,
		UInt16 precision, const char *title, const char **button) {

	keypadCallback = callback;
	windowTitle = title;
	buttons = button;
	StrCopy(inputString, FloatToStr(initialValue, precision));

}
