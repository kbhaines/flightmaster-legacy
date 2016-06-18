/*
 * TextDialog.h
 *
 * Info Dialog displays the information about airspace
 *
 * This dialog should be opened via a call to FrmPopupForm, not FrmGotoForm.
 *
 */

#ifndef TEXTDIALOG_H_INCLUDED
#define TEXTDIALOG_H_INCLUDED

#include "Platform.h"
#include "Constants.h"
#include "GlobalTypes.h"

/*
 * function : TextDialogHandleEvent
 *
 */

extern Boolean TextDialogHandleEvent(EventPtr event) MODALDIALOGS_SECTION;

/*
 * function : TextDialogSet
 *
 * This function must be called *immediately before* displaying the 
 * dialog.
 *
 */

extern void TextDialogSet (const char *notes) MODALDIALOGS_SECTION;

#endif
