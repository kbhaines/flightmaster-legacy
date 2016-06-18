/*
 * AlphaPadDialog.h
 *
 */

#ifndef ALPHAPAD_H_INCLUDED
#define ALPHAPAD_H_INCLUDED

#include "Platform.h"
#include "Constants.h"

/*
 * function : AlphaPadDialogHandleEvent
 *
 *
 */

extern Boolean AlphaPadDialogHandleEvent(EventPtr event) MODALDIALOGS_SECTION;

/*
 * function : AlphaPadDialogInit
 *
 * This function must be called before loading the alpha-dialog. 
 *
 * Pass it an initial value for the string to display.
 * 
 */

extern void AlphaPadDialogInit(char *initialValue) MODALDIALOGS_SECTION;

/*
 * Free the returned string when you're done...
 *
 */

extern char *AlphaPadGetInput(void) MODALDIALOGS_SECTION;

#endif
