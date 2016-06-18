/******************************************************************************
 *
 * EditWaypointForm.c
 *
 * (c) Blackhawk Systems Ltd 2002
 *
 */

#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif
#include "Platform.h"
#include <FloatMgr.h>
#include "ResourceDefines.h"
#include "EditWaypointForm.h"
#include "Utils.h"
#include "MathLib.h"
#include "GlobalTypes.h"
#include "Modules.h"


/**************************************************************************
 *
 * global variables
 *
 */

/*
 * lets us know if the GPS is running, and the GPS position if so. We use this
 * to set up the waypoints Lat & Lon if the GPS is running with a valid
 * position fix
 *
 */

extern Boolean  GPSState;
extern GPSType GPS;

/**************************************************************************
 *
 * module variables
 *
 */

/*
 * pointer to callback function that we call when exitting dialog
 * 
 */

static Boolean (*callbackFn)(Boolean validate, const Waypoint *wp, UInt16 size, Boolean deleted);

/*
 * pointer to the waypoint to use to set up the form
 *
 */

static Waypoint *inWaypoint;

#define EwErrThrow ErrThrow(EditWaypointFormModuleID | __LINE__)

/**************************************************************************
 *
 * private functions
 *
 * are predeclared here to allow them to be easily moved into other code sections
 *
 */

static void SetDegreesAndMinutes(FormPtr form, UInt16 degrees,UInt16 minutes, 
					double rad) EDITWAYPOINT_SECTION;
static void EditWaypointFormInit(FormPtr form) EDITWAYPOINT_SECTION;
static void EditWaypointFormDeinit(void) EDITWAYPOINT_SECTION;
static Err ValidateAndTranslateInputs(void **record, UInt16 *recSize) EDITWAYPOINT_SECTION;
static Boolean HandleCtlSelectEvent(EventPtr event) EDITWAYPOINT_SECTION;
static Boolean HandleKeyEvent(EventPtr event) EDITWAYPOINT_SECTION;



/*
 * function : SetDegreesAndMinutes
 *
 * Takes a radian value and uses it to set up specified degrees and minutes 
 * fields
 * 
 */
static void SetDegreesAndMinutes(FormPtr form, UInt16 degrees,UInt16 minutes, 
		double rad) {
//	FieldPtr deg = GetObjectPtr(form,degrees);
//	FieldPtr min = GetObjectPtr(form,minutes);

	if (rad<0) {
		rad=-rad;
	}
	rad *= (180/PI);
	SetFieldTextFromStr(deg,DoubleToStr(trunc(rad),0), false);
	rad = (rad-trunc(rad))*60;
	SetFieldTextFromStr(min,DoubleToStr(rad,2), false);
}

/*
 * function : EditWaypointFormInit
 *
 * Initialises the fields on the form with the contents of the input Waypoint
 *
 */

static void EditWaypointFormInit(FormPtr form)
{
	double tempd;

	/*  warning-- don't do any drawing in this routine. */
	/*  Also, don't call FrmSetFocus from here (it must be called *after* */
	/*  FrmDrawForm) */

	/* if this waypoint is only waypoint, user may not delete it */
	GUIObjectHide( DeleteButton);

	/* if this is not a new waypoint, initialise the data on the
	 * form with the information from the record
	 */
	if (inWaypoint) {
		SetFieldTextFromStr(GetObjectPtr(form, IdField), inWaypoint->ident, false);
		/* Latitude information */
		SetDegreesAndMinutes(form,LatDegreesField, LatMinutesField, 
				inWaypoint->latitude);
		FrmSetControlGroupSelection(form, LatPushButtonGroup,
				inWaypoint->latitude>0?NorthPushButton:SouthPushButton);

		/* Longitude information */
		SetDegreesAndMinutes(form,LonDegreesField, LonMinutesField, 
				inWaypoint->longitude);
		FrmSetControlGroupSelection(form, LonPushButtonGroup,
				inWaypoint->longitude>0?WestPushButton:EastPushButton);

		/* magnetic variation */
		tempd = inWaypoint->magVar*180/PI;
		SetFieldTextFromStr((FieldPtr)GetObjectPtr(form,MagVarnField),
				DoubleToStr(tempd,1), false);

		/* notes */
		SetFieldTextFromStr((FieldPtr)GetObjectPtr(form,DescriptionField), GetStringFromList(inWaypoint->ident,2),false);
	} else {
		/* a new waypoint - if the GPS is running and got a valid
		 * fix then read the GPS data into the fields
		 */
		if (GPSState && GPS.sat.fixType>1) {
			char id[10];
			/* lat and long */
			SetDegreesAndMinutes(form,LatDegreesField,
					LatMinutesField, GPS.posn.latitude);
			FrmSetControlGroupSelection(form, LatPushButtonGroup,
				GPS.posn.latitude>0?
					NorthPushButton:SouthPushButton);
			SetDegreesAndMinutes(form,LonDegreesField,
					LonMinutesField, GPS.posn.longitude);
			FrmSetControlGroupSelection(form, LonPushButtonGroup,
				GPS.posn.longitude<0?
					EastPushButton:WestPushButton);

			/* mag variation */
			tempd = (double)GPS.posn.magVarn;
			SetFieldTextFromStr(GetObjectPtr(form,MagVarnField),
				DoubleToStr(tempd,1), false);

			/* Waypoint Id */
			StrPrintF(id,"FM%02d:%02d",(UInt16)GPS.posn.utc.hour,
					(UInt16)GPS.posn.utc.min);
			SetFieldTextFromStr(GetObjectPtr(form, IdField),id,
					false);
			
		} else {
			FrmSetControlGroupSelection(form,
					LatPushButtonGroup,NorthPushButton);
			FrmSetControlGroupSelection(form,
					LonPushButtonGroup,WestPushButton);
		}
	}
}

/*
 * function : EditWaypointFormDeinit
 *
 * Clean up and return to the form which opened us...
 *
 */

static void EditWaypointFormDeinit(void)
{
	if (inWaypoint) MemPtrFree((void*)inWaypoint);
}

/* 
 * function : ValidateAndTranslateInputs
 *
 * Clean up and return to the form which opened us...
 * Checks the fields on the form for accurate data and translates the data in
 * the fields into a waypoint record structure.
 *
 * *record will contain a pointer to a Waypoint structure, *recSize will be set
 * to the size of the structure (which may vary according to the length of the
 * notes field).
 *
 * Returns an error code if the input fields are incorrect, or returns errNone
 * if everything is OK.  The error code is (coincidentally!) a valid alert
 * dialog resource number corresponding to the error.
 *
 * NOTE: IN ANY EVENT IT DOES *NOT* DE-ALLOCATE THE POINTER IN *record - THAT
 * IS THE RESPONSIBILITY OF THE CALLING PROCESS.
 *
 */

static Err ValidateAndTranslateInputs(void **record, UInt16 *recSize) {
	char 	*ident;
	char    *notes;
	char    *field;
	double 	deg,min,magVarn;
	FormPtr	thisForm = FrmGetActiveForm();
	Waypoint *wp;

	/* 
	 * work out the size of the record 
	 * 
	 * Ident and notes are variable length fields, we need to know how big
	 * they are to allocate a record of the correct size
	 *
	 */
	*recSize = sizeof(Waypoint);
	*record  = NULL;

	/* Id field */
	ident = FldGetTextPtr((FieldPtr)GetObjectPtr(thisForm, IdField));
	if (ident == NULL)
		return WpIdErrorAlert;
	if (StrLen(ident)==0)
		return WpIdErrorAlert;

	*recSize += StrLen(ident);

	notes = FldGetTextPtr((FieldPtr)GetObjectPtr(thisForm, DescriptionField));
	if (notes)
		*recSize += StrLen(notes);

	*record = MemPtrNew(*recSize);
	if(!*record) EwErrThrow;
	wp = *record;

	/* Store the ident and notes */
	StrCopy(wp->ident, ident);

	/*
	 * name is not used by this form - zero-length string
	 *
	 */

	wp->name[StrLen(ident)] = 0;

	if (notes)
		StrCopy(&wp->notes[StrLen(ident)], notes);
	else
		wp->notes[StrLen(ident)]=0;


	/* Latitude data */
	field = FldGetTextPtr((FieldPtr)GetObjectPtr(thisForm, 
				LatDegreesField));
	if (field == NULL)
		return WpLatErrorAlert;
	deg = StrToDouble(field);
	if (deg <0 || deg > 90)
		return WpLatErrorAlert;
	field = FldGetTextPtr((FieldPtr)GetObjectPtr(thisForm, 
				LatMinutesField));
	if (field == NULL)
		return WpLatErrorAlert;
	min = StrToDouble(field);
	if (min < 0 || min > 59.99 || (min>0 && deg == 90))
		return WpLatErrorAlert;
	deg = (deg+min/60)*PI/180;
	wp->latitude = FrmGetObjectId(thisForm,FrmGetControlGroupSelection(thisForm,
			LatPushButtonGroup))==NorthPushButton?deg:-deg;

	/* Longitude data */
	field = FldGetTextPtr((FieldPtr)GetObjectPtr(thisForm, LonDegreesField));
	if (field == NULL)
		return WpLonErrorAlert;
	deg = StrToDouble(field);
	if (deg < 0 || deg > 180)
		return WpLonErrorAlert;
	field = FldGetTextPtr((FieldPtr)GetObjectPtr(thisForm, LonMinutesField));
	if (field == NULL)
		return WpLonErrorAlert;
	min = StrToDouble(field);
	if (min<0 || min > 59.99 || (min>0 && deg == 180))
		return WpLonErrorAlert;
	deg = (deg+min/60)*PI/180;
	wp->longitude = FrmGetObjectId(thisForm,FrmGetControlGroupSelection(thisForm,
			LonPushButtonGroup))==EastPushButton?-deg:deg;

	/* Magnetic Variation */
	field = FldGetTextPtr((FieldPtr)GetObjectPtr(thisForm,MagVarnField));
	if (!field || !StringIsNumeric(field))
		return WpMagVarAlert;
	magVarn = StrToDouble(field);
	if (magVarn<-180.0 || magVarn >180.0)
		return WpMagVarAlert;
	wp->magVar = (float)magVarn*PI/180;
	wp->elevation = 0;

	return errNone;
}

/*
 * function : HandleCtlSelectEvent
 *
 * Handle all form's control events 
 *
 * Returns true if this function handled the event.
 */
static Boolean HandleCtlSelectEvent(EventPtr event) {
	Boolean 	handled = false;
	Err		error;
	void 		*wp;
	UInt16 		recSize;

	switch (PFEventGetControlSelectID(event)) {
	case OKButton:
		error = ValidateAndTranslateInputs(&wp, &recSize);
		if (!error) {
			if (callbackFn(true, wp, recSize, false)) {
				EditWaypointFormDeinit();
				GUIFormReturn();
				callbackFn(false, wp, recSize, false);
			}
		} else {
			GUIAlertShow(error); 
		}
		if (wp!=NULL)
			MemPtrFree(wp);
		handled = true;
		break;

	case CancelButton:
		EditWaypointFormDeinit();
		GUIFormReturn();
		callbackFn(false, NULL, 0,false);
		handled = true;
		break;

	case DeleteButton:
		if (GUIAlertShow(ConfirmDeleteDialogue) == 0) {
			EditWaypointFormDeinit();
			GUIFormReturn();
			callbackFn(false, NULL,0, true);
		}
		handled = true;
	}
	return handled;
}

/*
 * function : HandleKeyEvent
 *
 * vchrHard1 and vchrHard4 simulate OK and Cancel button taps, while
 * the other hard keys are ignored.
 *
 */

static Boolean HandleKeyEvent(EventPtr e) {

	Boolean handled = false;

	switch (e->data.keyDown.chr) {

	}
	return handled;
}

/**************************************************************************
 *
 * public functions
 *
 */

/*
 * function : EditWaypointFormHandleEvent
 *
 * Form's event handler
 *
 * Returns true if the function handled the event.
 */
Boolean EditWaypointFormHandleEvent(EventPtr event)
{
	Boolean handled = false;
	FormPtr form = FrmGetActiveForm();

	switch (PFEventGetType(event)) 
	{
	case frmOpenEvent:

		CollapseSetState(form, collapseStateUser);
		EditWaypointFormInit(form);
		GUIFormDraw();		/*  here's where you'd add a call to FrmSetFocus */
		handled = true;
		break;
			
	case winDisplayChangedEvent:

		if ( SimpleResizeForm(form,true) || CollapseCheckForPin10NeedToRedraw()) {

			GUIFormDraw();
		}

		handled = true;
		break;

	case ctlSelectEvent:
		handled = HandleCtlSelectEvent(event);
		break;

	case keyDownEvent:
		handled = HandleKeyEvent(event);
		break;

	case frmCloseEvent:
		if (inWaypoint) MemPtrFree((void*)inWaypoint);
		break;

	default:
		break;
	}	
	return handled;
}

/*
 * function : EditWaypointFormSetup
 *
 */

void EditWaypointFormSetup(const Waypoint *inWp, Boolean(*cb)(Boolean validate, const Waypoint *wp, UInt16 size, Boolean deleted)) {

	callbackFn = cb;
	if (inWp) {
		inWaypoint = MemPtrNew(PFMallocSize((void*)inWp));
		PFMemMove(inWaypoint, inWp, PFMallocSize((void*)inWp));
	} else {
		inWaypoint = NULL;
	}

}

