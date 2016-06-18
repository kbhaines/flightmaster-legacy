/*
 * MessageDialog.h
 *
 * The message dialog shows a message to the user up to three options, each option
 * generated a different PalmOS event
 * 
 */

#ifndef MESSAGE_DIALOG_H_INCLUDED
#define MESSAGE_DIALOG_H_INCLUDED

#include "Platform.h"
#include "Constants.h"

#define MAX_MESSAGE_ITEMS 4

/*
 * Fill one of these structures with details of the message and buttons that
 * should be displayed on the dialog.
 *
 * (See function MessageDialogDataNew)
 *
 */

typedef struct {

	char *message;
	Int16 numItems;

	/*
	 * string is the label to display in the button, action
	 * is the event that will be posted if the button is pressed
	 *
	 */

	const char *string[MAX_MESSAGE_ITEMS];
	UInt16 action[MAX_MESSAGE_ITEMS];

} MessageDialogDataType;

/*
 * function : MessageDialogDataNew
 *
 * Creates a MessageDialogDataType structure and fills it with the specified
 * values.
 *
 * 'message' is copied, so does not need to persist after this function
 * is called.
 *
 */

MessageDialogDataType *MessageDialogDataNew(const char *message, Int16 numItems,
		const char *str1, UInt16 action1,
		const char *str2, UInt16 action2,
		const char *str3, UInt16 action3,
		const char *str4, UInt16 action4) MODALDIALOGS_SECTION;

/*
 * function : MessageDialogHandleEvent
 *
 *
 */

extern Boolean MessageDialogHandleEvent(EventPtr event) MODALDIALOGS_SECTION;

/*
 * function : MessageDialogInit
 *
 * This function *must* be called before the dialog is opened.
 *
 * The memory for 'md' will be freed when the dialog closes.
 *
 */

extern void MessageDialogInit(MessageDialogDataType *md) MODALDIALOGS_SECTION;
		
/*
 * function : MessageDialogDataFree
 *
 */

extern void MessageDialogDataFree(/*@out@*/ /*@only@*/ MessageDialogDataType *md) MODALDIALOGS_SECTION;

#endif
