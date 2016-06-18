/*
 * CommandDialog.h
 *
 */

#ifndef COMMAND_H_INCLUDED
#define COMMAND_H_INCLUDED

#include "Platform.h"
#include "Constants.h"

/*
 * function : CommandDialogHandleEvent
 *
 */

extern Boolean CommandDialogHandleEvent(EventPtr event) MODALDIALOGS_SECTION;

/*
 * function : CommandDialogInit
 *
 * This function *must* be called before the command dialog is invoked.
 *
 */

extern void CommandDialogInit(void) MODALDIALOGS_SECTION;
		

#endif
