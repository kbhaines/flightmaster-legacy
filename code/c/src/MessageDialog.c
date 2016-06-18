/*
 * MessageDialog.c
 *
 * (c) 2005 Blackhawk Systems
 *
 */
#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif

#include "Platform.h"
#include "GlobalTypes.h"
#include "Modules.h"
#include "ResourceDefines.h"
#include "MessageDialog.h"
#include "Utils.h"


#define ModuleID MessageDialogModuleID

/*******************************************************************************
 * 
 * module variables
 *
 * 
 */


static MessageDialogDataType *messageData;


/*******************************************************************************
 *
 * private functions
 *
 */

static void SetupDialog(void) MODALDIALOGS_SECTION;

/*
 * SetupDialog
 * 
 * Configures the dialog with the data stored in messageData.
 *
 */

static void SetupDialog(void) {
	
    Int16 j;

    GUIFieldSetText(MDField, messageData->message, false);

    /*
     * set up the visible buttons
     *
     */

    for (j=0; j<messageData->numItems; j++)
		GUIObjectSetLabel(MDButton1+j, messageData->string[j]);

    /*
     * hide the rest of the buttons
     *
     */
     
    for (j=messageData->numItems; j < MAX_MESSAGE_ITEMS; j++) 
		GUIObjectHide(MDButton1+j);

}


/*******************************************************************************
 *
 * public functions
 *
 */

/*
 * MessageDialogDataNew
 *
 */

MessageDialogDataType *MessageDialogDataNew(const char *message, Int16 numItems,
		const char *str1, UInt16 action1,
		const char *str2, UInt16 action2,
		const char *str3, UInt16 action3,
		const char *str4, UInt16 action4) {

	MessageDialogDataType *md = PFMalloc(sizeof(MessageDialogDataType));

	char *messageCopy = PFMalloc(StrLen(message)+1);

	LOGENTRY;

	ModErrThrowIf(!md);
	ModErrThrowIf(!messageCopy);

	StrCopy(messageCopy, message);
	md->message = messageCopy;

	md->numItems = numItems;
	md->string[0] = str1;
	md->string[1] = str2;
	md->string[2] = str3;
	md->string[3] = str4;

	md->action[0] = action1;
	md->action[1] = action2;
	md->action[2] = action3;
	md->action[3] = action4;

	LOGEXIT;
	return md;

}



/*
 * function : MessageDialogHandleEvent
 *
 */
Boolean MessageDialogHandleEvent(EventPtr event) {

	Boolean handled = false;

	LOGENTRY;

	switch (PFEventGetType(event)) {

	case frmOpenEvent:
		GUIFormResize(true, true);
		SetupDialog();
		GUIFormDraw();	
		handled = true;
		break;

	case winDisplayChangedEvent:
		GUIFormResize(true, true);
		GUIFormDraw();
		handled = true;
		break;

	case frmCloseEvent:
		PFMemFree(messageData);
		break;

	case ctlSelectEvent:
		if (messageData->action[PFEventGetControlSelectID(event) - MDButton1]) {

			GUIFormReturn();
			PFEventSendMenu(messageData->action[PFEventGetControlSelectID(event) - MDButton1]);
			PFMemFree(messageData);
			handled = true;

		} 
		break;

	default:
		break;
	}

	LOGEXIT;

	return handled;

}

/*
 * function : MessageDialogInit
 *
 */

void MessageDialogInit(MessageDialogDataType *md) {

    messageData = md;

}

/*
 * function : MessageDialogFree
 *
 */

void MessageDialogDataFree(MessageDialogDataType *md) {

	PFMemFree(md->message);
	PFMemFree(md);

}

