/*
 * KeypadDialog.h
 *
 * The keypad dialog is a pad of large buttons 0-9, OK and CLR to accept
 * numerical input from the user.
 *
 */

#ifndef KEYPAD_H_INCLUDED
#define KEYPAD_H_INCLUDED

#include "Platform.h"
#include "Constants.h"

/*
 * function : KeypadDialogHandleEvent
 *
 *
 */

extern Boolean KeypadDialogHandleEvent(EventPtr event) MODALDIALOGS_SECTION;

/*
 * function : KeypadDialogInit
 *
 * This function *must* be called before the dialog is opened.
 *
 * callback is a pointer to a callback function which will be passed the button
 * Id (0-3) used to exit the dialog, and the value that the user entered.
 *
 * If the callback returns false, then the data is invalid and the keypad will
 * not close.
 *
 * precision is the number of decimal places to format the initialValue to.
 *
 * title is the title of the keypad window.
 *
 * button is an array[4] of pointers to the strings that are the labels for the
 * buttons (max. 4 strings) - use NULL for unused values
 *
 */

extern void KeypadDialogInit(Boolean (*callback)(UInt16,float), float initialValue,
		UInt16 precision, const char *title, const char **button) MODALDIALOGS_SECTION;
		
#endif
