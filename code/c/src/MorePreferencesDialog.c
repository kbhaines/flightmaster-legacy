/*
 *
 * MorePreferencesDialog.c
 *
 */

#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif

#include "Platform.h"
#include "MorePreferencesDialog.h"
#include "Utils.h"
#include "CpInterface.h"
#include "ResourceDefines.h"
#include "GlobalTypes.h"
#include "MathLib.h"


#include "FMPreferences.h"

/*
 * RangeLabels
 *
 */
#include "MapForm.h"

/*******************************************************************************
 *
 * Global variables
 *
 */

extern FMPreferencesType Preferences;
extern const DisplayUnitsType   DisplayUnits;

/*****************************************************************************
 *
 * module variables
 *
 */

/*******************************************************************************
 *
 * private functions
 *
 * are declared here to allow them to be easily relocated to another code
 * section
 *
 */

static Int16 RwyDimFieldValue(UInt16 fld) MODALDIALOGS_SECTION;
static Boolean CheckDimensions(UInt16 *length, UInt16 *width) MODALDIALOGS_SECTION;
static Boolean ValidateAndSetPrefs(void) MODALDIALOGS_SECTION;
static void SetupDialog(void) MODALDIALOGS_SECTION;

/****************************************************************************/


/*
 * function : RwyDimFieldValue
 *
 * Returns the runway dimension entered into the specified field.
 *
 * Returns -1 if the input is invalid
 *
 */

Int16 RwyDimFieldValue(UInt16 fld) {
	Int32   runwayDim;
	char    *txt;

	txt = GUIFieldGetText(fld);

	if (!txt || !StringIsNumeric(txt)) {
		return -1;
	}
	runwayDim=StrAToI(txt);

	return (Int16)runwayDim;
}

/*
 * function : CheckDimensions
 *
 * validate and return the runway dimensions
 *
 * Returns true if OK, false if not
 *
 */

Boolean CheckDimensions(UInt16 *length, UInt16 *width) {

	Int16 dim;

	dim = RwyDimFieldValue(MpLengthField);
	if (dim>=0)
		*length = (UInt16)dim;
	else
		return false;

	dim = RwyDimFieldValue(MpWidthField);
	if (dim>=0)
		*width = (UInt16)dim;
	else
		return false;

	return true;
}


/*
 * function : ValidateAndSetPrefs
 *
 */
static Boolean ValidateAndSetPrefs(void) {

	FMPreferencesType np;
	double atz;

	/*
	 * check & get the runway dimensions
	 *
	 */

	if (!CheckDimensions(&np.runwayLength, &np.runwayWidth)) {
		GUIAlertShow(InvalidRunwayDimensionAlert);
		return false;
	}


	/*
	 * runway surface
	 * 
	 */

	switch (GUIObjectGroupGetValue(MpSurfacePushbuttonGroup)) {
	
	case MpAsphaltPushbutton:
		np.runwaySurface = surfHard;
		break;
	case MpGrassPushbutton:
		np.runwaySurface = surfGrass;
		break;
	case MpBothPushbutton:
		np.runwaySurface = surfEither;
		break;

	}

	/*
	 * convert and then check the ATZ radius
	 *
	 */

	atz = StrToDouble(GUIFieldGetText(MpATZRadiusField));
	if (DisplayUnits.distance == MI_UNITS)
		atz /= MI_PER_NM;
	else if (DisplayUnits.distance == KM_UNITS)
		atz /= KM_PER_NM;

	if (atz<1 || atz>6)  {
		GUIAlertShow(InvalidATZRadiusAlert);
		return false;
	}
	np.atzRadius = (UInt16)(atz*8);
		
	Preferences.runwayLength=np.runwayLength;
	Preferences.runwayWidth =np.runwayWidth;
	Preferences.runwaySurface = np.runwaySurface;
	Preferences.atzRadius = np.atzRadius;

	return true;
}

/*
 * function : SetupDialog
 *
 * Fill the fields on the Preferences Dialog with the values from
 * the Preferences structure.
 *
 */
static void SetupDialog(void) {
	char         tmp[6];
	UInt16       surfaceButton;
	double       radius;

	/* 
	 * set up the runway surface selector
	 *
	 */

	switch (Preferences.runwaySurface) {
	case surfEither:
		surfaceButton = MpBothPushbutton;
		break;
	case surfGrass:
		surfaceButton = MpGrassPushbutton;
		break;
	default:
	case surfHard:
		surfaceButton = MpAsphaltPushbutton;
		break;
	}
	
	GUIObjectGroupSetValue(MpSurfacePushbuttonGroup, surfaceButton);

	/*
	 * set the runway lengths
	 *
	 */

	StrPrintF(tmp, "%d",Preferences.runwayLength);
	GUIFieldSetText(MpLengthField, tmp, false);
	StrPrintF(tmp, "%d",Preferences.runwayWidth);
	GUIFieldSetText(MpWidthField, tmp, false);

	radius = (double)Preferences.atzRadius/8;
	if (DisplayUnits.distance == MI_UNITS)
		radius *= MI_PER_NM;
	else if (DisplayUnits.distance == KM_UNITS) 
		radius *= KM_PER_NM;

	StrPrintF(tmp, "%s",DoubleToStr(radius,1));
	GUIFieldSetText(MpATZRadiusField, tmp, false);

}


/*******************************************************************************
 *
 * public functions
 *
 */

/*
 * function : MorePreferencesDialogHandleEvent
 *
 */

Boolean MorePreferencesDialogHandleEvent(EventPtr event) {

	Boolean handled      = false;

	switch (PFEventGetType(event)) {

	case frmOpenEvent:
		GUIFormResize(true, true);
		SetupDialog();
		GUIFormDraw();
		handled = true;
		break;

	case frmCloseEvent:
		handled = false;
		break;

	case winDisplayChangedEvent:
		GUIFormResize(true, true);
		GUIFormDraw();
		handled = true;
		break;

	case ctlSelectEvent:
		switch (PFEventGetControlSelectID(event)) {
		case MpOKButton:
			if (ValidateAndSetPrefs()) {

				GUIFormReturn();

			}

			handled = true;

			break;

		case MpCancelButton:
			GUIFormReturn();
			handled = true;
			break;
		}

	default:
		break;
	}
	return handled;
}

