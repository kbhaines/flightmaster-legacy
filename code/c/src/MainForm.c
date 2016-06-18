/******************************************************************************
 *
 * MainForm.c
 *
 * (c) 2002 Blackhawk Systems Ltd.
 *
 */

#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif

#include "Platform.h"
#include <Rect.h>
#include <DLServer.h>

#include "FMStrings.h"

#include "ResourceDefines.h"
#include "MainForm.h"

#include "Utils.h"
#include "GlobalTypes.h"
#include "AvCalcs.h"
#include "MathLib.h"

#include "HeadingIndicator.h"

#include "CpInterface.h"

/* yes, we're including a .c file !!!
 * This is to allow inlining of the registration
 * generator
 */
#include "RegistrationCode.c"

#include "SelectCopilotFlightDialog.h"
#include "WPInfoDialog.h"
#include "Modules.h"
#include "DiversionMgr.h"
#include "AIGauge.h"
#include "Fixed.h"
#include "Polygon.h"

#include "NavManager.h"

#include "Instruments.h"

#include "FMPreferences.h"

/**************************************************************************
 *
 * global variables
 *
 */

extern Boolean            GPSState;
extern GPSType        GPS;
extern FMPreferencesType  Preferences;
extern Boolean            CopilotInstalled;

extern const SatConstType SatConst;

/*
 * these can be modified by SetDisplayUnits
 *
 */

extern UserConversionType UC;
extern DisplayUnitsType   DisplayUnits;

extern FlightPlanType     FlightPlan;
extern FlightPlanStackType FlightPlanStack;

extern const char        *CompassText[];

extern const AppColourPrefsType AppColourPrefs;
extern const char *Dashes[];

extern ScreenInfoType    pfScreen;



/**************************************************************************
 *
 * module variables
 *
 */

#define MfErrThrow ErrThrow(MainFormModuleID | __LINE__)

static Coord AIRadius;

/*
 * variables that control the position and size of the displays
 *
 */

static PFScreenRectType centreHotspot;
static PFScreenRectType timeHotspot;
static PFScreenRectType bearingHotspot;

static const Coord bottomReadoutY = StandardPageHeight - 32;
static const Coord toFromFlagY    = 18;
static const Coord rangeY         = 32;

static const Coord gaugeX         = 26;
static const Coord gaugeY         = 34;
static const Coord gaugeWidth     = 268;

/*
 * see above for where these numbers come from
 *
 */

static const PointType hsiZoomLabel = { 2*(13+134/2), 2*(17+134*23/32) };
static const PointType backCourseFlag = { 2*(13+134/2-35), 2*(17+134*5/8) };


static const PointType vdopFlagPos = { 50, 65 };
static const PointType hzFlagPos = { 100, 65 };

/* 
 * Flag to determine showing of To Flag or From Flag
 * These are the characters in the Symbol7 font
 *
 */

#define ToFlag 1
#define FromFlag 2
#define ToFromFlagFont symbol7Font

static UInt8 toFromFlag = ToFlag;

#define WARN_NO_POSITION_FIX 0
#define WARN_GPS_NOT_STARTED 1

#define MAX_ZOOM 7
static const float hsiScale[] = { 3.0, 2.5, 2.0, 1.5, 1.0, 0.5, 0.25 };


/*
 * Indicates if AI Gauge is in use or not
 *
 */

static Boolean showAI = false;

static HSIType hsi;	// horizontal situation indicator
static VSIType vsi;	// VSI indicator
static GSIType gsi;	// Glideslope indicator

/**************************************************************************
 *
 * private functions
 *
 * are declared here to allow easy relocation to another code section
 *
 */

static void AIGaugeUpdate(void) MAINFORM_SECTION;
static void UpdateDisplay(void) MAINFORM_SECTION;
static Boolean HandleKeyEvent(EventPtr event) MAINFORM_SECTION;
static Boolean HandlePenEvent(EventPtr event) MAINFORM_SECTION;
static void MainFormInit(void) MAINFORM_SECTION;
static void MainFormDeinit(void) MAINFORM_SECTION;

/*
 * function : AIGaugeUpdate
 *
 */


static void AIGaugeUpdate(void) {

	double bankAngle;
	double pitchAngle;


	LOGENTRY;

	/*
	 * Bank Angle calculation
	 *
	 * Rate 1 turn = 3 deg per second
	 *
	 * 1.5*(speed/10) = angle of bank for rate 1 turn
	 *
	 * 1. How far we've turned (d)
	 * 2. How long it took to turn that far (d/s)
	 *
	 */

	bankAngle = -NavGetFloatValue(navBankAngle);
	
	//bankAngle = -((7 + GPS.posn.speed/10) / 3) * GPS.posn.deltaHeading;

	/*
	 * stops at 60 degrees of bank
	 *
	 */

	if (bankAngle > 60.0)
		bankAngle = 60.0; 
	else if (bankAngle < -60.0)
		bankAngle = -60.0; 

	/*
	 * pitch angle calculation
	 *
	 * 1. How far we've climbed/descended (f)
	 * 2. How long to climb that far (f/s)
	 * 3. Relate to horizontal speed (climb angle)
	 *
	 * A strong 3-d fix is required for good pitch information !!
	 *
	 */
	
	if (GPS.sat.fixType == 3) {

		float  velocity;

		/*
		 * knots to feet per second... 6076 feet per NM
		 *
		 */

		velocity = GPS.posn.speed * 6076.0 / 3600.0;
		if (velocity>10.0) {
			pitchAngle = atan(GPS.posn.deltaAltitude/velocity);

			/*
			 * stops at 40 degrees
			 *
			 */

			if (pitchAngle > DEG_TO_RAD(30))
				pitchAngle = DEG_TO_RAD(30);
			else if (pitchAngle < -DEG_TO_RAD(30))
				pitchAngle = -DEG_TO_RAD(30);
		} else  {
			pitchAngle = 0.0;
		}
	} else {
		pitchAngle = 0.0;
	}

	AIGaugeDraw(gaugeX,gaugeY,DEG_TO_RAD(bankAngle),pitchAngle);

	if (GPS.sat.fixType == 2 || GPS.sat.vdop >= 2.5) {

		/*
		 * warn that the gauge doesn't show pitch
		 *
		 */

		DrawFlag(&vdopFlagPos, StrVP, AppColourPrefs.red);

	}

	if (GPS.posn.deltaTime > 1.5) {

		/*
		 * warn that the data isn't current enough for accurate
		 * pitch & roll
		 *
		 */

		DrawFlag(&hzFlagPos, StrHz, AppColourPrefs.red);

	}

	DUMP_INT16((Int16)(bankAngle*100),30,50);

	LOGEXIT;
}


/*
 * function : UpdateDisplay
 *
 * Draws the display, consisting of the readout data around the
 * edges and the heading indicator in the centre.
 *
 */

static void UpdateDisplay(void) {
	char               temp[10];
	Coord              pos;

	LOGENTRY;

	PFDrawStatePush();
	PFScreenLock(false);
	
	/*
	 * draw warning messages according to satellite fix status
	 *
	 */

	if (GPS.sat.fixType < 2) {
		FntSetFont(largeBoldFont);
		DrawAlignedChars(StrWarnNoPosition,ALIGNCENTRE,pfScreen.xcentre,pfScreen.ycentre-FntLineHeight());
		if (GPS.sat.fixType == 0) {
			DrawAlignedChars(StrWarnGPSNotDetected, ALIGNCENTRE,pfScreen.xcentre,pfScreen.ycentre);
		}
	}

	if (GPS.sat.fixType < 2) goto updateExit;

	/*
	 * draw rest of display, assumption now is that we have at least a 2-d
	 * fix.
	 *
	 */
	
	FntSetFont(boldFont);
	DrawAlignedChars(NavGetStrValue(navToIdent), ALIGNCENTRE, StandardPageWidth/2, StandardPageHeight - pfScreen.boldHeight);

	/*
	 * draw the heading or attitude indicator
	 *
	 */

	if (showAI) {

		AIGaugeUpdate();

	} else {

		if (FpGetNumLegs(FlightPlan)) {

			// TODO - HSI needs mi/km scales!
			
			HSIDraw(hsi, NavGetIntValue(navTrack),
					NavGetIntValue(navBearingTo),
					NavGetIntValue(navCourse), 
					NavGetFloatValue(navXTRK), hsiScale[Preferences.hsiZoom]);

		} else {

			/*
			 * draw HSI with no nav data 
			 *
			 */

			HSIDraw(hsi, NavGetIntValue(navTrack),
					NavGetIntValue(navTrack),
					NavGetIntValue(navTrack), 0.0, hsiScale[Preferences.hsiZoom]);

		}

		FntSetFont(stdFont);
		StrPrintF(temp, "<%s%s>", FloatToStr(hsiScale[Preferences.hsiZoom]*2,1), UC.distanceUnits);
		PFDrawOutlineChars(temp, ALIGNCENTRE, hsiZoomLabel.x, hsiZoomLabel.y);

	}

	if (GPS.sat.fixType > 1) {

		/*
		 * draw hdop and vdop markers
		 *
		 */

		PointType p;
		IndexedColorType colour = AppColourPrefs.red;

		p.x = 0;
		p.y = pfScreen.ledHeight + pfScreen.largeBoldHeight - 5;

		if (GPS.sat.hdop < 30.0) {

			if (GPS.sat.hdop < 15.0)

				colour = AppColourPrefs.green;

			else 

				colour = AppColourPrefs.yellow;

		}

		DrawFlag(&p, StrH, colour);
		if (GPS.sat.fixType > 2) {

			p.x = 10;
			colour = AppColourPrefs.red;
			if (GPS.sat.vdop < 30.0) {

				if (GPS.sat.vdop < 15.0)

					colour = AppColourPrefs.green;

				else 

					colour = AppColourPrefs.yellow;

			}
			DrawFlag(&p, StrV, colour);

		}

		p.x = 0;
		p.y = pfScreen.ycentre + 36;
		if (GPS.sat.waas) DrawFlag(&p, StrWA, AppColourPrefs.green);

		if (NavGetIntValue(navBackcourse)) DrawFlag(&backCourseFlag, StrBackCourse, AppColourPrefs.red);

	}

	/* 
	 * TO/FROM Display
	 *
	 * If magnetic bearings needed then show TO based on local variation
	 * and FROM based on waypoint variation.
	 *
	 * Compass bearings are *always* based on true bearings, not magnetic,
	 * so do this first before adding on MV
	 *
	 * The convoluted formula below resolves to a number from 0 to 11,
	 * which gives us an index into the CompassText array - multiplying
	 * everything by 4 keeps us in integer land.
	 *
	 * NB: Any info based on the current waypoint is filtered out (according
	 * to the state of FpGetNumLegs(FlightPlan)).
	 *
	 */
	
	PFSetDrawMode(blockMode);

	if (FpGetNumLegs(FlightPlan)) {

		pos = NavDrawField(toFromFlag == ToFlag ? navBearingTo:navBearingFrom ,
				0,0, ALIGNLEFT,ledFont);

		FntSetFont(ToFromFlagFont);
		PFPaintChars(&toFromFlag,1,pos-10,toFromFlagY);

		NavDrawField(navDistanceTo, 0, rangeY, ALIGNLEFT, largeBoldFont);

		FntSetFont(largeBoldFont);
		DrawAlignedChars(NavGetStrValue(navXTRK),ALIGNRIGHT,StandardPageWidth,rangeY);

	} else {

		/*
		 * no range, bearing & xtrack information so draw
		 * invalid data fields
		 *
		 */

		FntSetFont(ledFont);
		DrawAlignedChars(Dashes[3], ALIGNLEFT, 0,0);
		DrawAlignedChars(Dashes[3], ALIGNLEFT, 0,rangeY);
		DrawAlignedChars(Dashes[3], ALIGNRIGHT, StandardPageWidth, rangeY);

	}

	NavDrawField(navTrack,StandardPageWidth / 2,0, ALIGNCENTRE,ledFont);
	NavDrawField(navGS,StandardPageWidth,0, ALIGNRIGHT,ledFont);
	
	FlightDirectorDraw(StandardPageWidth/2, 18, 30);
			
	/*
	 * Altitude & VSI and time-to-VNAV event
	 *
	 */

	if (GPS.sat.fixType > 2) {

		Int32 vs = NavGetIntValue(navVSI);

		NavDrawField(navAltitude, StandardPageWidth, bottomReadoutY, ALIGNRIGHT, ledFont);

		VSIDraw(vsi, vs, NavGetIntValue(navVNAVRequiredVS));
		GSIDraw(gsi, NavGetIntValue(navVNAVAltError));

		if (NavGetIntValue(navVNAVETE) < 60) {

			NavDrawField(navVNAVETE, StandardPageWidth, 
					5+StandardPageHeight-pfScreen.ledHeight-pfScreen.largeBoldHeight,
					ALIGNRIGHT, largeBoldFont);

		}

	} else {
		
		/*
		 * No altitude available, draw a warning
		 *
		 */

		Coord pos;
		
		FntSetFont(largeBoldFont);
		pos = StandardPageHeight - FntLineHeight();
		DrawAlignedChars(Str2DOnly,ALIGNRIGHT,StandardPageWidth,pos);

	}

	NavDrawField(navETE + Preferences.hsiTimeField, 0, bottomReadoutY, ALIGNLEFT, ledFont);

	/*
	 * TurnETE time
	 *
	 */

	if (NavGetIntValue(navTurnETE) < 60) {

		NavDrawField(navTurnETE, 0, 6+StandardPageHeight-pfScreen.ledHeight-pfScreen.largeBoldHeight,
				ALIGNLEFT, largeBoldFont);

	}

updateExit:

	if (SatConst) SatConstDraw(SatConst);

	PFScreenUnlock();
	PFDrawStatePop();

	PFSendSimpleEvent(evtScreenRedrawn);

	LOGEXIT;
}

static Boolean HandlePenEvent(EventPtr event) {

	Coord screenX = PFEventGetX(event) * 2;
	Coord screenY = PFEventGetY(event) * 2;
	
	Boolean handled = false;

	if (PFScreenPointInRectangle(screenX, screenY, &bearingHotspot)) {
		
		/*
		 * user tapped on the Bearing display
		 *
		 */
		
		FntSetFont(ledFont);
		toFromFlag=toFromFlag==ToFlag?FromFlag:ToFlag;

		handled = true;

	}  else if (PFScreenPointInRectangle(screenX, screenY, &timeHotspot)) {
		
		/*
		 * user tapped on the time display
		 *
		 */

		if (navETE + Preferences.hsiTimeField<navUTC)
			Preferences.hsiTimeField++;
		else
			Preferences.hsiTimeField=0;
		handled = true;
		
	} else if (PFScreenPointInRectangle(screenX, screenY, &centreHotspot ) ) {

		showAI = !showAI;
		handled = true;

	}
	
	return handled;

}


/* 
 * function : HandleKeyEvent
 *
 * Checks if the user has pressed either of the scroll keys
 *
 * 
 */

static Boolean HandleKeyEvent(EventPtr event) {

	LOGENTRY;

	switch (PFEventGetKeyChr(event)) {
	case vchrRockerLeft:

		Preferences.hsiZoom = MAX(0, Preferences.hsiZoom - 1);
		break;

	case vchrRockerRight:
		Preferences.hsiZoom = MIN(MAX_ZOOM-1, Preferences.hsiZoom + 1);
		break;

	}
	
	LOGEXIT;

	return true;
	
}

/*
 * function : MainFormInit
 *
 * Initialises the form when it is opened.
 */
static void MainFormInit(void)
{
	char uid[dlkUserNameBufSize];

	LOGENTRY;

	DlkGetSyncInfo(NULL,NULL,NULL,uid,NULL,NULL);

	/*  warning-- don't do any drawing in this routine. */
	/*  Also, don't call FrmSetFocus from here (it must be called *after* */
	/*  FrmDrawForm) */


	/*
	 * read the preferences and set display units accordingly
	 *
	 */

	SetDisplayUnits(Preferences.units);

	/*
	 * set up hotspots
	 *
	 */

	FntSetFont(ledFont);
	PFScreenRectangleSetRel(&centreHotspot, StandardPageWidth/2-30, StandardPageHeight/2-30, 60,60);
	PFScreenRectangleSetRel(&timeHotspot, 0, StandardPageHeight - pfScreen.ledHeight,
			FntCharWidth('0')*12,pfScreen.ledHeight);
	PFScreenRectangleSetRel(&bearingHotspot, 0,0, FntCharWidth('0')*6,pfScreen.ledHeight);

	vsi = VSINew(StandardPageWidth - 12, StandardPageHeight/2 - 70, 
			10, 140, false, DisplayUnits.altitude == METRE_UNITS ? 300:800);
	gsi = GSINew(0,StandardPageHeight/2 - 70, 12,140, DisplayUnits.altitude == METRE_UNITS ? 200:400, 100);
	hsi = HSINew(gaugeX, gaugeY, gaugeWidth/2);

	AIGaugeInit(&AIRadius);

	LOGEXIT;
	
}

/* 
 * function : MainFormDeinit
 *
 * Deinitialises the form when it is closed
 */
static void MainFormDeinit(void)
{

	LOGENTRY;

	HSIFree(hsi);
	AIGaugeDeInit();

	VSIFree(vsi);
	GSIFree(gsi);


	LOGEXIT;

}


/**************************************************************************
 *
 * public functions
 *
 */

/* 
 * function : MainFormHandleEvent
 *
 */

Boolean MainFormHandleEvent(EventPtr event)
{
	Boolean handled = false;
	static UInt16 cycle;

	LOGENTRY;

	LOGINT16((Int16)(PFEventGetType(event)));

	switch (PFEventGetType(event)) 
	{
	case frmOpenEvent:
		GUIFormResize(false, false);
		MainFormInit();
		GUIFormDraw();		
		UpdateDisplay();
		handled = true;
		break;

	case winDisplayChangedEvent:

		if ( GUIFormResize(false, false)) {

			MainFormDeinit();
			MainFormInit();
			UpdateDisplay();
			
		}

		handled = true;
		break;

	case frmUpdateEvent:
		GUIFormDraw();		UpdateDisplay();
		handled = true;
		break;
			
	case penDownEvent:

		handled = HandlePenEvent(event);
		if (handled)
			UpdateDisplay();

		break;

	case evtGPSFixLost:
	case evtGPSPositionUpdate:

		/* 
		 * received when new GPS data is available, usually once per
		 * second.  
		 *
		 */
		
		if (!GUIMenuIsDisplayed()) UpdateDisplay();
		cycle ++;

		handled = true;
		break;

	case menuEvent:

		switch (PFEventGetMenuID(event)) {

		case MnNextLeg:
		case MnPreviousLeg:
			UpdateDisplay();
			handled = true;
			break;

		case MnEditPlan:

			/*
			 * diversion management
			 *
			 * (AppHandleEvent handles cancel and emergency diversions)
			 *
			 */

			if (!FpIsBlank(FlightPlan) && PFEventGetMenuID(event) == MnEditPlan) {

				const FlightPlanLegWaypointType *wc = FpGetCurrentWaypoint(FlightPlan);
				
				FpStackPush(FlightPlanStack, FlightPlan);
				FpSetNewFirstLeg(FlightPlan, GPS.posn.lat32, GPS.posn.lon32, 
						"GPS", GPS.posn.magVarn,
						wc->lat, wc->lon, wc->ident, wc->magVar); 
			}

			UpdateDisplay();
			handled = true;
			break;
			
		case MnInformation:
			if (!FpIsBlank(FlightPlan)) {

				WPInfoSetWaypointInfo(NavGetIDOfPlanWaypoint(FpGetCurrentLeg(FlightPlan)+1));
				GUIFormPopup(WPInfoDialog);
				
			}
			handled = true;
			break;

		}

		break;

	case keyDownEvent:
		handled = HandleKeyEvent(event);
		break;

	case evtCrsOverride:
		handled = true;
		break;

	case frmCloseEvent:
		MainFormDeinit();
		handled = false;
		break;

	default:
		break;
	}	
	LOGEXIT;
	return handled;
}

/*
 * function : SetDisplayUnits
 *
 */

void SetDisplayUnits(UInt8 units) {

	/* 
	 * speed, distance and height formats, and conversion factors. Position in
	 * array corresponds to the indexes held in DisplayUnits structure
	 *
	 * NB 
	 *
	 * Conversions were specified as const but seemed to cause
	 * problems when compiled.
	 *
	 */

	const char *DistanceFmt[] = { "nm", "mi", "km" };
	const char *SpeedFmt[] = { "Kt","Mh","Kh" };
	const char *HeightFmt[] = { "ft", "m" };
	const char *HeadingFmt[] = { "\260T", "\260m" };

	/* distance converted from radians */
	const float DistanceConversionFactors[] = { NM_PER_RADIAN, MI_PER_RADIAN, KM_PER_RADIAN };

	/* speed converted from knots */
	const float SpeedConversionFactors[]= { 1.0, /* GPS speed is already knots! */
					MI_PER_NM, KM_PER_NM };

	/* alt converted from feet */
	const float AltConversionFactors[] = { 1.0,  METRES_PER_FOOT }; 

	switch (units) {
	case NM_UNITS:	DisplayUnits.distance = NM_UNITS;
		       	DisplayUnits.altitude = FEET_UNITS;
		       	DisplayUnits.speed = NM_UNITS;
		       	break;

	case MI_UNITS:	DisplayUnits.distance = MI_UNITS;
			DisplayUnits.altitude = FEET_UNITS;
			DisplayUnits.speed = MI_UNITS;
			break;

	case KM_UNITS:	DisplayUnits.distance = KM_UNITS;
			DisplayUnits.altitude = METRE_UNITS;
			DisplayUnits.speed = KM_UNITS;
			break;

	case CP_UNITS:	
			CpInitialise();
			CpGetUnitPreferences(&DisplayUnits);
			CpClose();
			break;

	default:
			MfErrThrow;
	}

	UC.altitudeConv = AltConversionFactors[DisplayUnits.altitude];
	UC.speedConv = SpeedConversionFactors[DisplayUnits.speed];
	UC.distanceConv= DistanceConversionFactors[DisplayUnits.distance];
	UC.minorUnits  = DisplayUnits.altitude == METRE_UNITS ? 1000.0 : 6076.0;

	UC.altitudeUnits = HeightFmt[DisplayUnits.altitude];
	UC.speedUnits    = SpeedFmt[DisplayUnits.speed];
	UC.distanceUnits = DistanceFmt[DisplayUnits.distance];

	UC.heading = HeadingFmt[Preferences.useMagnetic ? 1 : 0];

}
