/*
 * NavManager.c
 *
 * Navigation Management Module.
 * 
 *
 */

#include "Platform.h"
#include "ResourceDefines.h"
#include "NavManager.h"
#include "GlobalTypes.h"
#include "MathLib.h"
#include "AvCalcs.h"
#include "WDManager.h"
#include "Utils.h"
#include "Modules.h"
#include "MathLib.h"
#include "AlarmManager.h"
#include "OBSManager.h"
#include "FMStrings.h"

#include "FMPreferences.h"

#define ModuleID NavManagerModuleID

/*****************************************************************************
 *
 * global variables
 *
 */

extern const FMPreferencesType Preferences;
extern const char *CompassText[];

extern WDMHandle WPDataset;
extern const UserConversionType UC;
extern FlightPlanType FlightPlan;


/*****************************************************************************
 *
 * module variables
 *
 */

#define ModuleID NavManagerModuleID

//VAR:

/*
 * navigation information structure
 *
 * A = start waypoint
 * B = end waypoint
 * D = present position
 * P = point along crsAB, abeam our craft i.e. Along track distance
 *
 *
 *				D
 * 			    | distPD
 *              |
 * A -----------P----- B
 *
 */

static struct {

	/*
	 * NavManager keeps a copy of the current flight plan version, and watches
	 * for it changing, so it knows when to do some initial leg-based
	 * calculations
	 *
	 */

	Int16 fpVersion;
	Boolean obsState;

	Int16 leg;

	float verticalSpeed; // user units
	float trackError; // user units
	float groundSpeed; // user units
	Int32 altitude; // user units

	/*
	 * course information
	 *
	 * crsAB is constant per leg of the route. crsPB is updated as the
	 * craft moves along the great circle of crsAB.
	 *
	 * The craft is abeam a point P, which is along the course AB.
	 *
	 */

	double latA;	// lat & lon of point A
	double lonA;
	double latB;	// lat & lon of point B
	double lonB;
	
	double crsAB; // radians, course from A to B
	double crsPB; // radians, course from P (along track) to B
	double distAB; // length of leg, radians

	double crsDB;   // radians, course from aircraft to B
	double distDB;  // distance from craft to B, radians
	double distPD;  // track error in radians

	double distAP;	// along track distance


	/*
	 * turn anticipation variables
	 *
	 * Method:
	 *
	 * Next leg is BC, present position is D:
	 *
	 * B+---------------+C
	 *	    |     a/| 
	 *	    |     / |
	 *	    |    /b | r
	 *	  X |   /\  | 
	 *	    |  /  \a| 
	 *      | /    \|
	 *      |/a     +
	 *	   D+----
	 *
	 * 	a = angle to turn (must be < 90 degrees)
	 * 	b = 90 degrees
	 *
	 * 	X = XTRK error (D to BC)
	 *	r = radius of turn at speed v: r = (v^2) / (g.tan a) 
	 *
	 *		 	 		  X
	 *	(1) dist.tp	 =	-----	-  r.sin a	
	 *					sin a
	 *
	 *
	 *  (2) dist.tp   = distDC - r.sin a
	 *
	 * (1) causes unwanted leg advances when there is a small angle between subsequent
	 * legs, so equation (2) is used
	 *
	 */

	double crsBC;			// radians, course of next leg BC
	float  turnRadius;		// in NM, based on present speed
	float  distanceToTurn;	// in user units
	float  turnAngle;		// "a", in radians
	Boolean leftTurn;		// true, if the turn required is left
	Int32  timeToTurn;		// in seconds
	Boolean turnArmed;		// true if <2 minutes to turn

	float  bankAngle;			// angle of bank in degrees (-ve = left)
	float  flightDirectorRoll; 	// angle (degrees) to turn to intercept current leg

	Boolean backCourse; // true if crsPB is the backcourse, i.e. AP > AB

	Int16  degreesCrsPB; // degrees, user units (true/magnetic)

	Int16 bearingTo;	// degrees, user units
	Int16 bearingFrom;	// degrees, user units

	float   distanceTo; // user units
	Int32   timeTo;     // seconds

	Int16  track;

	char fromIdent[MAX_IDENT_CHARS+1];
	char toIdent[MAX_IDENT_CHARS+1];

	Int16 gpsHour, gpsMin, gpsSec;

	/*
	 * VNAV data
	 *
	 */

	Boolean vnavSet;
	const VNAVDataType *vnav;	// copied in from the flight plan current leg
	float   targetAltitude;		// feet, AMSL

	Int32 requiredVS;		// user units - feet or metres per minute

	/*
	 * distance & time to VNAV measure either up until capture point, or
	 * up until end of the VNAV - depends on vnavCapture
	 *
	 */

	float radDistanceToVNAV;// radians
	float radDistanceToDescend;// radians

	float distanceToVNAV;	// user units.
	Int32 timeToVNAV;		// seconds

	Boolean vnavCaptured;	// in descent if true
	Boolean vnavArmed;		// true if within 2mins of top of VNAV
	float vnavEnd;			// distance of end-of-vnav from the waypoint
	float glideSlopeRatio;	// after VNAV capture point
	Int32 altError;			// user units, above or below glideslope

} navInfo;

/*****************************************************************************
 *
 * private functions
 *
 *
 */

static void VNAVCalculations(float gpsSpeed, float gpsAltitude) ADT_SECTION;
static void CalculateETAHMS(Int32 secsToGo, Int16 *hour, Int16 *min, Int16 *sec ) ADT_SECTION;

/*
 * function : CalculateETAHMS
 * 
 */

static void CalculateETAHMS(Int32 secsToGo, Int16 *hour, Int16 *min, Int16 *sec ) {
	
	Int32 secsTime = navInfo.timeTo + GPSGetDaySeconds()
		+(Int32)Preferences.localTimeZone * 30 * 60;

	if (secsTime<0) secsTime += 86400;
	else if (secsTime>86400) secsTime -= 86400;

	*sec = secsTime % 60;
	*min = (secsTime / 60) % 60;
	*hour = (secsTime/3600);
	
}

/*
 * VNAVCalculations
 *
 * Performs the VNAV calculations
 *
 */

static void VNAVCalculations(float gpsSpeed, float gpsAltitude) {

	Int32 altd = (Int32)(navInfo.targetAltitude - gpsAltitude);

	LOGENTRY;

	if (abs(altd) < 100) {

		LOGTAG("EO-VNAV (alt acq.)");
		navInfo.vnavCaptured = false;
		navInfo.vnavArmed = false;
		navInfo.vnavSet = false;
		LOGEXIT;
		return;

	}

	gpsSpeed = MAX(gpsSpeed,1);

	if (navInfo.vnavCaptured) {

		/*
		 * in descent
		 *
		 * (1)	ae =	gpsAlt - (ta + gsr . (distDB - distBefore))
		 *
		 * where ae = alt error, ta = target altitude, gsr = glide slope ratio
		 *
		 */

		float distanceToRun = (float)(navInfo.distDB - navInfo.vnav->distanceBefore);

		//if (distanceToRun < 0.0) distanceToRun = 0.0;

		navInfo.distanceToVNAV = distanceToRun * UC.distanceConv;
		distanceToRun = RAD_TO_NM(distanceToRun);
		navInfo.timeToVNAV = (Int32) ((distanceToRun / gpsSpeed)*3600.0);

		navInfo.requiredVS = (60*(navInfo.targetAltitude - gpsAltitude)) / MAX(navInfo.timeToVNAV,1);

		navInfo.altError =(Int32) ((gpsAltitude - (navInfo.targetAltitude + 
				navInfo.glideSlopeRatio * distanceToRun * FEET_PER_NM)) * UC.altitudeConv);

		LOGINT32(navInfo.targetAltitude);
		LOGINT32(navInfo.glideSlopeRatio*100);
		LOGINT32(navInfo.altError);

	} else {

		/*
		 * pre-capture, calculate distance-to-descend, distance-to-VNAV and time 
		 * to VNAV
		 *
		 * VNAV point of descent is calculated with the basic formula:
		 *
		 * (1) 	dtd =	 dAlt
		 * 				------
		 * 				 tan(a)
		 *
		 * where dtd = distance to descend, dAlt is altitude change, a is glideslope
		 *
		 * Since tan(a) = 	vs	(vertical speed)
		 * 					--
		 * 					gs	(ground speed)
		 *
		 * 	then we can re-write (1) as:
		 *
		 * 	(2)	dtd = 	dAlt.gs
		 * 				-------
		 * 				  vs
		 *
		 * Hence we only need to calculate appropriate vs for each method of descent:
		 * 
		 * 	(3)	vs = 	60.feet_per_min		(feet per minute mode)
		 * 	
		 * 	(4) vs = 	gs.feet_per_nm
		 * 				--------------		(feet per NM mode)
		 * 					6076
		 * 	
		 * 	(5) vs = 	gs.tan(a)			(glideslope mode)
		 *
		 * Once we've calculated dtd, we need only to subtract it from the distance
		 * to the waypoint, and then subtract the 'by' distance to get the distance
		 * from our position to the start of VNAV.
		 *
		 */

		float dtd;				// distance to descend in NM
		float distToVNAV; 		// distance to VNAV point in radians
		float vs;				// required vertical speed (nm/hour)

		float deltaAlt = (gpsAltitude - navInfo.targetAltitude) / FEET_PER_NM; // NM
		Boolean climb = deltaAlt < 0.0;

		if (climb) deltaAlt = - deltaAlt;
		
		switch (navInfo.vnav->rateType) {

		case vnavRatePerMin:
			vs = (navInfo.vnav->targetRate * 60) / FEET_PER_NM;
			break;

		case vnavRatePerMile:
			vs = (gpsSpeed * (navInfo.vnav->targetRate / FEET_PER_NM) );
			break;

		case vnavDegrees:
			vs = gpsSpeed * tan((double)navInfo.vnav->targetRate);
			break;

		}

		dtd = (deltaAlt * gpsSpeed) / vs;

		/*
		 * how far to VNAV?
		 *
		 * If within 2minutes, then start calculating the glideslope so
		 * it can be displayed thus allowing the user to intercept it.
		 *
		 */

		navInfo.radDistanceToDescend = NM_TO_RAD(dtd);
		distToVNAV = navInfo.distDB - navInfo.radDistanceToDescend - navInfo.vnav->distanceBefore;
		navInfo.radDistanceToVNAV = distToVNAV;

		navInfo.distanceToVNAV = distToVNAV * UC.distanceConv;
		navInfo.timeToVNAV = (Int32) ( (RAD_TO_NM(distToVNAV) / gpsSpeed)*3600.0);

		if (navInfo.timeToVNAV <= 120) {
			
			float distanceToRun = (float) RAD_TO_NM(navInfo.distDB - navInfo.vnav->distanceBefore);
			Int32 timeToEndOfVNAV = (Int32) ((distanceToRun / gpsSpeed)*3600.0);

			/*
			 * send a message
			 *
			 */

			if (!navInfo.vnavArmed) {
				
				MessageDialogDataType *md;

				md = MessageDialogDataNew(VNAVIn2MinutesString, 1,
						VNAVOptionString, MnVNAV,
						NULL, 0, NULL, 0, NULL, 0);

				PFPlaySound(FILEROOT "snd-vnav.fma", Preferences.voiceVolume);
				AlarmSetCondition(md, alarmMessage);
				navInfo.vnavArmed= true;

			}

			if (navInfo.vnav->rateType == vnavDegrees)

				navInfo.glideSlopeRatio = (float) tan(navInfo.vnav->targetRate);

			else
			
				navInfo.glideSlopeRatio = deltaAlt / dtd;

			if (climb) navInfo.glideSlopeRatio = -navInfo.glideSlopeRatio;

			navInfo.altError =(Int32) ((gpsAltitude - (navInfo.targetAltitude + 
					navInfo.glideSlopeRatio * distanceToRun * FEET_PER_NM)) * UC.altitudeConv);

			navInfo.requiredVS = (60*(navInfo.targetAltitude - gpsAltitude)) / timeToEndOfVNAV;

			if (distToVNAV <= 0.0) {

				char altStr[12];
				Int16 j;
				LOGTAG("VNAV Capture");

				distToVNAV = 0.0;
				navInfo.vnavCaptured = true;

				PFPlaySound(climb ? FILEROOT"snd-climb.fma" : FILEROOT"snd-descend.fma", Preferences.voiceVolume);

				StrPrintF(altStr, "%ld", (Int32)(navInfo.targetAltitude*UC.altitudeConv));
				for (j=0;j<StrLen(altStr);j++) {

					char fileName[32];
					StrPrintF(fileName, FILEROOT"snd-%c.fma", altStr[j]);
					PFPlaySound(fileName, Preferences.voiceVolume);

				}

			}
				
		} else if (navInfo.timeToVNAV > 130) {

			navInfo.vnavArmed = false;

		}


	}

	LOGEXIT;

}

/*******************************************************************************
 *
 * public functions
 *
 */


/*
 * NavUpdatePosition
 *
 * Tells the navigation manager the latest GPS data
 *
 * If the NM notices that the current leg of the flightplan has changed, then it
 * recalculates its leg information
 *
 * A = start waypoint
 * B = end waypoint
 * D = present position
 * P = point along crsAB, abeam our craft
 *
 *				D
 * 	       		| distPD
 *              |
 * A -----------P----- B
 *
 */


void NavUpdatePosition(GPSPosnData *gps) {

	double crsAD, distAD;

	double latP, lonP;
	double r;

	float radSpeed = NM_TO_RAD(gps->speed)/3600.0;	// GPS speed in radians/second
	float feetPerSecond;

	float aspectAngle;	// Angle between PB and heading (radians)

	LOGENTRY;

	navInfo.gpsHour = gps->utc.hour;
	navInfo.gpsMin = gps->utc.minute;
	navInfo.gpsSec = gps->utc.second;

	/*
	 * Track - only updated when moving
	 *
	 */

	if ( gps->speed > 3.0 ) {

		if (Preferences.useMagnetic) {

			navInfo.track = (Int16)round(gps->magHeading);

		} else {

			navInfo.track = (Int16)round(gps->trueHeading);

		}

	}

	navInfo.groundSpeed = gps->speed * UC.speedConv;
	navInfo.altitude = (Int32)(gps->altitude * UC.altitudeConv);

	/*
	 * vertical speed, units per minute
	 *
	 */

	navInfo.verticalSpeed = UC.altitudeConv * 60 * gps->deltaAltitude;

	navInfo.bankAngle = ((1.5 * gps->speed/10) / 3) * gps->deltaHeading;

	if (FpIsBlank(FlightPlan)) {

		LOGEXIT;
		return;

	}

	/*
	 * if the plan has changed then we need to perform initial calculation
	 * of crsAB, and copy the VNAV data (if set)
	 *
	 */

	// TODO - Eliminate FpGetVersion from code
	
	if (FpGetVersion(FlightPlan, minorVersion)  != navInfo.fpVersion ||
			OBSActive() != navInfo.obsState) {

		const FlightPlanLegType *fpl;
		const FlightPlanLegWaypointType *wpA, *wpB;

		/*
		 * first, do leg-based calculations
		 *
		 */

		navInfo.obsState = OBSActive();

		navInfo.fpVersion = FpGetVersion(FlightPlan, minorVersion);
		fpl = FpGetLeg(FlightPlan, FpGetCurrentLeg(FlightPlan), false);

		if (OBSActive()) {
			
			wpA = OBSGetWaypoint(0);

		} else {

			wpA = FpGetWaypoint(FlightPlan, FpGetCurrentLeg(FlightPlan));

		}

		wpB = FpGetWaypoint(FlightPlan, FpGetCurrentLeg(FlightPlan)+1);

		navInfo.latA = INT32_TO_RAD(wpA->lat);
		navInfo.lonA = INT32_TO_RAD(wpA->lon);
		navInfo.latB = INT32_TO_RAD(wpB->lat);
		navInfo.lonB = INT32_TO_RAD(wpB->lon);

		navInfo.crsAB = AvCalcGreatCircleCourse(navInfo.latA,navInfo.lonA, 
				navInfo.latB, navInfo.lonB, &navInfo.distAB);

		navInfo.leg = FpGetCurrentLeg(FlightPlan);

		StrCopy(navInfo.fromIdent, wpA->ident);
		StrCopy(navInfo.toIdent, wpB->ident);

		/*
		 * setup turn anticipation
		 *
		 */

		navInfo.turnArmed = false;
		if (FpGetCurrentLeg(FlightPlan) < FpGetNumLegs(FlightPlan) - 1) {

			fpl = FpGetLeg(FlightPlan, FpGetCurrentLeg(FlightPlan)+1, false);
		
			navInfo.crsBC = (double)fpl->track;

		}

		/*
		 * set up VNAV data
		 *
		 */

		navInfo.vnavSet = FpVNAVIsSet(FlightPlan, navInfo.leg);
		navInfo.vnavCaptured = false;
		navInfo.vnavArmed = false;

		if (navInfo.vnavSet) {

			/*
			 * link to the VNAV data
			 *
			 */

			navInfo.vnav = FpGetVNAV(FlightPlan, navInfo.leg);
			navInfo.targetAltitude = navInfo.vnav->targetAltitude;

			if (navInfo.vnav->altType == vnavAltAboveWP) {

				/*
				 * get the waypoint to determine its elevation
				 *
				 */

				WaypointIDType wpID = NavGetIDOfPlanWaypoint(FpGetCurrentLeg(FlightPlan)+1);

				if (wpID != wpNotFound) {

					Waypoint *wp = WDMGetWaypoint(WPDataset, wpID);

					navInfo.targetAltitude += wp->elevation;
					PFMemFree(wp);

				}

			}

		}

	}

	/*
	 * calculate the bearing and range between present position and
	 * the destination waypoint.
	 *
	 */

	LOGTAG("Range/Bearing");
	LOGTIMESTART;

	navInfo.crsDB = AvCalcGreatCircleCourse(gps->latitude, gps->longitude, 
			navInfo.latB, navInfo.lonB, &navInfo.distDB);

	navInfo.distanceTo = navInfo.distDB * UC.distanceConv;

	LOGINT32(navInfo.distanceTo*10);

	if (gps->speed > 1.0) {

		navInfo.timeTo = (Int32) (navInfo.distDB / radSpeed); 

	} else {

		navInfo.timeTo = 86401; 	// longer than 1 day

	}

	/* 
	 * TO/FROM in degrees.
	 *
	 */
	
	DEG_MAGVAR_GPS(navInfo.bearingTo, navInfo.crsDB, *gps);
	navInfo.bearingFrom = navInfo.bearingTo+180;
	WRAPMAX(navInfo.bearingFrom,360);

	LOGTIMESTOP;

	/*
	 * calculate the off-track error. D is our current position, A is the
	 * start waypoint
	 *
	 */

	LOGTAG("XTRK");
	LOGTIMESTART;

	crsAD = AvCalcGreatCircleCourse(navInfo.latA, navInfo.lonA, gps->latitude, gps->longitude, &distAD);
	navInfo.distPD = AvCalcOffTrackError(navInfo.crsAB, crsAD, distAD); 
	navInfo.trackError = navInfo.distPD * UC.distanceConv;
		
	LOGTIMESTOP;

	/*
	 * every few cycles work out what the along track distance is
	 * and check it against the backCourse flag to see if it
	 * needs to change
	 *
	 */

	LOGTAG("ATD");
	LOGTIMESTART;

	navInfo.distAP = acos(cos(distAD)/cos(navInfo.distPD));
	navInfo.distAP = MAX(0.0, navInfo.distAP);

	if (navInfo.distAP > navInfo.distAB && !navInfo.backCourse)  {
		
		navInfo.backCourse = true;
		
	} else if (navInfo.distAP < navInfo.distAB && navInfo.backCourse) {
		
		navInfo.backCourse = false;

	}

	/*
	 * position of P, bearing crsAB from A, distance atd
	 *
	 */

	latP = navInfo.latA;
	lonP = navInfo.lonA;

	AvCalcShiftPoint(&latP, &lonP, navInfo.crsAB, navInfo.distAP);

	LOGTIMESTOP;

	/*
	 * course from P to B
	 *
	 */

	LOGTAG("CRS at ATD");
	LOGTIMESTART;

	navInfo.crsPB = AvCalcGreatCircleCourse(latP,lonP, navInfo.latB, navInfo.lonB, &r);

	if (navInfo.backCourse) {
		
		navInfo.crsPB += PI;
		if (navInfo.crsPB > 2*PI)
			navInfo.crsPB -= 2*PI;

	}
	
	DEG_MAGVAR_GPS(navInfo.degreesCrsPB, navInfo.crsPB, *gps);

	LOGTIMESTOP;

	/*
	 * Turn Anticipation
	 *
	 * Radius calculations based in feet
	 *
	 * Turn radius r:
	 *
	 * (1) 	   v^2
	 * 		----------
	 * 		g * tan(b)
	 *
	 * Where b = angle of bank, g = 32.185 ft/(s^2)
	 *
	 * Constant 15 degree angle of bank is assumed
	 * tan(15) = 0.2679
	 *
	 */

#define GTAN15 (32.185 *  0.267949192)

	LOGTAG("Turn Anticipation");
	LOGTIMESTART;

	feetPerSecond = (radSpeed * NM_PER_RADIAN * FEET_PER_NM); 
	navInfo.turnRadius = NM_TO_RAD(FEET_TO_NM((feetPerSecond * feetPerSecond) / GTAN15));

	if (!Preferences.turnAnticipation) {

		 navInfo.turnArmed = false;

	} else if (FpGetCurrentLeg(FlightPlan) < FpGetNumLegs(FlightPlan)-1) {

		navInfo.turnAngle = navInfo.crsBC - DEG_TO_RAD(gps->trueHeading);
		
		if (navInfo.turnAngle > PI) 
			
			navInfo.turnAngle = navInfo.turnAngle - 2*PI; 

		else if (navInfo.turnAngle < -PI) 
			
			navInfo.turnAngle = 2*PI + navInfo.turnAngle;

		LOGINT16(RAD_TO_DEG(navInfo.crsBC));
		LOGINT16(RAD_TO_DEG(navInfo.turnAngle));

		/*
		 * only works when turn is -90 to +90 degrees
		 *
		 */

		if (navInfo.turnAngle < 0.0) {
			
			navInfo.turnAngle = -navInfo.turnAngle;
			navInfo.leftTurn  = true;

		} else {

			navInfo.leftTurn = false;

		}

		if (navInfo.turnAngle > DEG_TO_RAD(5.0) && navInfo.turnAngle < PI/2) {

			double sinta = sin(navInfo.turnAngle);

			/*
			 * simple turn anticipation model, must get within turning distance of
			 * the destination waypoint
			 *
			 */

			navInfo.distanceToTurn = navInfo.distDB - navInfo.turnRadius * sinta;

#ifdef XXXX

			/*
			 * this code does turn-anticipation for a point anywhere along the
			 * next course, but it causes too many leg advances when the turns
			 * between the legs are shallow. I've left it in as an example that
			 * can be followed
			 *
			 */

			xtrkBC = AvCalcOffTrackError(navInfo.crsBC, navInfo.crsDB, navInfo.distDB);
			if (xtrkBC < 0.0) xtrkBC = -xtrkBC;

			navInfo.distanceToTurn = (xtrkBC / sinta) - navInfo.turnRadius * sinta;

#endif

			/*
			 * advance to next leg if reached the turn point, or if past the
			 * end of the current leg (PB > AB)
			 *
			 */

			if (navInfo.distanceToTurn <= 0.0 || (navInfo.timeToTurn < 60 && navInfo.turnArmed && navInfo.distAP > navInfo.distAB)) {

				
				/*
				 * An active VNAV event forces us to hang on until it's done
				 *
				 */

				if (!navInfo.vnavCaptured) {

					if (FpNextLeg(FlightPlan)) {
						
						/*
						 * leg has advanced, which way do we turn
						 *
						 */

						const FlightPlanLegType *fpl = FpGetLeg(FlightPlan,FpGetCurrentLeg(FlightPlan),false);
						Int16 newHeading;
						char newHeadingStr[4];
						char fileName[64];
						Int16 j;

						if (navInfo.leftTurn)

							PFPlaySound(FILEROOT"snd-turnleft.fma", Preferences.voiceVolume);

						else 

							PFPlaySound(FILEROOT"snd-turnright.fma", Preferences.voiceVolume);

						DEG_MAGVAR_GPS(newHeading, fpl->track, *gps);

						StrPrintF(newHeadingStr, "%03d", newHeading);
						for (j=0;j<3;j++) {

							StrPrintF(fileName, FILEROOT"snd-%c.fma", newHeadingStr[j]);
							PFPlaySound(fileName, Preferences.voiceVolume);

						}

						PFPlaySound(FILEROOT"snd-degrees.fma", Preferences.voiceVolume);

					}

				} else {

					navInfo.distanceToTurn = 0;
					navInfo.timeToTurn = 0;

				}

			} else {

				navInfo.timeToTurn = (Int32) (navInfo.distanceToTurn / radSpeed);
				navInfo.distanceToTurn *= UC.distanceConv;

				if (navInfo.timeToTurn < 120 && !navInfo.turnArmed) {

					/*
					 * don't bother sending the message in case where distance
					 * < 0, because the user has probably just turned-on the TA
					 * setting, and would get an unnecessary message
					 *
					 */

					if (navInfo.distanceToTurn > 0.0) {

						MessageDialogDataType *md;

						md = MessageDialogDataNew(TurnIn2MinutesString, 1,
								TurnAnticipateOffOption, MnTurnAnticipationOff,
								NULL, 0, NULL, 0, NULL, 0);

						PFPlaySound(FILEROOT "snd-turn.fma", Preferences.voiceVolume);
						AlarmSetCondition(md, alarmMessage);

					}

					navInfo.turnArmed = true;

				}

			}

			LOGINT16(RAD_TO_DEG(navInfo.turnAngle));
			//LOGINT16(RAD_TO_NM(xtrkBC*10));
			LOGINT16(RAD_TO_NM(navInfo.turnRadius*10));
			LOGINT16(navInfo.distanceToTurn*10);
			LOGINT16(navInfo.timeToTurn);
			LOGINT16((PI*feetPerSecond)*10.0/(180*GTAN15));

		} else {

			/*
			 * large turn angle - is the aircraft passed the end of the leg?
			 *
			 */

			if (navInfo.turnArmed && navInfo.distAP > navInfo.distAB) {

				FpNextLeg(FlightPlan);

			}
	
		}

	}

	/*
	 * Flight Director Roll
	 *
	 * Calculates the roll angle required to get back onto the current
	 * leg. Only functions when heading > +- 10 degrees off current crsPB
	 *
	 */

	aspectAngle = DEG_TO_RAD(gps->trueHeading) - navInfo.crsPB;
	if (aspectAngle > PI) 
		aspectAngle -= 2*PI;
	else if (aspectAngle < -PI)
		aspectAngle += 2*PI;
   
	if (aspectAngle > DEG_TO_RAD(1.0) || aspectAngle < DEG_TO_RAD(-1.0)) {
		
		/*
		 * special cases of aircraft (+) heading away from the leg, but in the same
		 * general direction:
		 *
		 */

		/*
		 *			B
		 * 			|
		 *  \		|     /
		 * 	 \		|    /
		 * 	  +		|   +
		 * 	  1		|   2
		 * 			A
		 */

		/*
		 * In case (1), XTRK < 0 and -PI/2 < aspectAngle < 0
		 *
		 * In case (2), XTRK > 0 and 0 < aspectAngle < PI/2
		 * 
		 */
		
		if (navInfo.distPD < 0 && aspectAngle > -PI/2 && aspectAngle < 0.0) {

			//navInfo.flightDirectorRoll = MIN(20.0,(aspectAngle*80.0)/(-PI/2));	// right turn
			navInfo.flightDirectorRoll = 20;

		} else if (navInfo.distPD > 0 && aspectAngle < PI/2 && aspectAngle > 0.0) {

			//navInfo.flightDirectorRoll = - MIN(20.0,aspectAngle*80.0/(PI/2));	// left turn
			navInfo.flightDirectorRoll = -20;

		} else {

			float r;
		
			/*
			 * calculate turn radius (in feet):
			 *
			 *  r 	= 		   xtrk
			 *  		-------------------
			 *  		tan(aa/2) * sin(aa)
			 *
			 *  where aa = aspectAngle
			 *
			 */

			r = navInfo.distPD / ( tan(aspectAngle/2) * sin(aspectAngle));
			r = NM_TO_FEET(RAD_TO_NM(r));

			/*
			 * now, what angle of bank do we need to make the turn?
			 *
			 * tan(b) =  v^2
			 * 			-----
			 * 			r * g
			 *
			 * 	where g= 32.185 ft/(s^2)
			 */

			navInfo.flightDirectorRoll = RAD_TO_DEG(atan(
					(feetPerSecond*feetPerSecond)/(r * 32.185)));
			
		}

	} else {

		navInfo.flightDirectorRoll = 0.0;

	}


	LOGTIMESTOP;

	if (navInfo.vnavSet) VNAVCalculations(gps->speed, gps->altitude);

	LOGEXIT;
}

/*
 * NavGetFloatValue
 *
 * Returns the specified value, last calculated during call to NavUpdatePosition
 *
 */

float NavGetFloatValue(NavDataType field) {

	switch (field) {

	case navXTRK:
		return navInfo.trackError;

	case navVSI:
		return navInfo.verticalSpeed;

	case navAltitude:
		return navInfo.altitude;
		
	case navFlightDirectorRoll:
		return navInfo.flightDirectorRoll;

	case navBankAngle:
		return navInfo.bankAngle;

	case navDistanceTo:
		return navInfo.distanceTo;
		
	default:
		ModErrThrow();
		return 0.0;

	}

}

/*
 * NavGetDoubleValue
 *
 */

double NavGetDoubleValue(NavDataType field) {

	switch (field) {

	case navRadBearingTo:
		return navInfo.crsDB;

	default:
		ModErrThrow();
		return 0.0;

	}

}


/*
 * NavGetIntValue
 *
 * Returns the specified value, last calculated during call to NavUpdatePosition
 *
 */

Int32 NavGetIntValue(NavDataType field) {

	switch (field) {

	case navBearingTo:
		return navInfo.bearingTo;

	case navBearingFrom:
		return navInfo.bearingFrom;

	case navTrack:
		return navInfo.track;

	case navCourse:
		return navInfo.degreesCrsPB;

	case navBackcourse:
		return (Int32)navInfo.backCourse;

	case navVSI:
		return (Int32)navInfo.verticalSpeed;

	case navVNAVRequiredVS:
		return navInfo.vnavArmed ? (Int32)navInfo.requiredVS : 0;

	case navVNAVAltError:
		return navInfo.vnavArmed ? (Int32)navInfo.altError : 0;
		
	case navTurnETE:
		return navInfo.turnArmed ? (Int32)navInfo.timeToTurn : 86401;

	case navVNAVETE:
		return navInfo.vnavArmed ? (Int32)navInfo.timeToVNAV : 86401;

	default:
		ModErrThrow();
		return 0;
		break;

	}

}

/*
 * NavGetStrValue
 *
 * Returns specified value as a formatted string. The string is volatile, the
 * next call to the function will overwrite its contents.
 *
 */

const char *NavGetStrValue(NavDataType field) {

	static char str[MAX_IDENT_CHARS+1];
	float trackError;
	char   trackDir;
	Int16 localHour, localMin;
	Boolean done = true;

	/*
	 * take care of field types that don't depend on us having
	 * a flight plan
	 *
	 */

	switch (field) {

	case navLocal:
		localHour = navInfo.gpsHour + Preferences.localTimeZone / 2;
		WRAPMAX(localHour, 24);
		localMin = navInfo.gpsMin + (Preferences.localTimeZone % 2) * 30;
		WRAPMAX(localMin, 60);
		StrPrintF(str, "%02d.%02d", localHour, localMin);
		break;
				
	case navUTC:
		StrPrintF(str, "%02d.%02d", navInfo.gpsHour, navInfo.gpsMin);
		break;

	case navTrack:
		StrPrintF(str, "%03d", navInfo.track);
		break;

	case navGS:
		StrPrintF(str,"%d",(Int16)round(navInfo.groundSpeed));
		break;


	case navAltitude:
		StrPrintF(str, "%ld", navInfo.altitude);
		break;

	case navFlightDirectorRoll:
		StrPrintF(str, "%s", FloatToStr(navInfo.flightDirectorRoll,1));
		break;

	default:

		/*
		 * none of the above!
		 *
		 */

		done = false;
		break;

	}

	if (done) return str;


	/*
	 * these values depend on having a valid flight plan
	 *
	 */

	if (FpIsBlank(FlightPlan)) return "---";

	switch (field) {

	case navToIdent:
		return navInfo.toIdent;

	case navFromIdent:
		return navInfo.fromIdent;

	case navBearingTo:
		StrPrintF(str, "%03d", navInfo.bearingTo);
		break;

	case navBearingFrom:
		StrPrintF(str, "%03d", navInfo.bearingFrom);
		break;

	case navCompassFrom:
		StrPrintF(str, "%s", CompassText[((((UInt16)round(navInfo.crsDB*DEG_PER_RAD))*4+45+180*4)%1440)/90]);
		break;

	case navDistanceTo:
		if (navInfo.distanceTo < 0.1) { 

			StrPrintF(str, "%s", FloatToStr(navInfo.distanceTo * UC.minorUnits,0));

		} else {

			StrPrintF(str, "%s", FloatToStr(navInfo.distanceTo, (navInfo.distanceTo < 100.0 ? 1:0)));

		}
		break;

	case navVNAVDistanceTo:
		StrPrintF(str, "%s", FloatToStr(navInfo.distanceToVNAV, (navInfo.distanceToVNAV < 100.0 ? 1:0)));
		break;

	case navETE:
		NavCalculateETE(str, navInfo.timeTo);
		break;

	case navVNAVETE:
		NavCalculateETE(str, navInfo.timeToVNAV);
		break;
		
	case navTurnETE:
		NavCalculateETE(str, navInfo.timeToTurn);
		break;

	case navVNAVAltError:
		StrPrintF(str,"%ld", navInfo.altError);
		break;

	case navETA:
		if (navInfo.timeTo < 86400) {

			Int16 hour, min,sec;
			
			CalculateETAHMS(navInfo.timeTo+30, &hour, &min,&sec);
			
			StrPrintF(str, "%02d.%02d",hour,min);

		} else {

			StrPrintF(str, "--.--");
			
		}
		break;

	case navXTRK:
		if (navInfo.trackError < 0) {

			trackDir = 'R';
			trackError = -navInfo.trackError;

		} else {

			trackDir = 'L';
			trackError = navInfo.trackError;

		}

		if (trackError < 999)

			StrPrintF(str, "%s%c", FloatToStr(trackError, trackError < 1 ? 2: trackError<100 ? 1 : 0), trackDir);

		else 

			StrPrintF(str, "---");

		break;

	default:

		ModErrThrow();

	}


	return str;

}

/*
 * NavDrawField
 *
 * Draws the specified navigation value at the specified location with
 * alignment, using the DrawLargeNumberSmallText function from Utils.c
 *
 */

Coord NavDrawField(NavDataType field, Coord x, Coord y, UInt8 alignment, FontID font) {

	const char *data, *label;
	char seconds[4];
	Int16 hour, min, sec;

	data = NavGetStrValue(field);

	switch (field) {

	case navBearingFrom:
	case navBearingTo:
	case navTrack:
		label = UC.heading;
		break;

	case navDistanceTo:
	case navVNAVDistanceTo:
		if (navInfo.distanceTo < 0.1)
			
			label = UC.altitudeUnits;

		else

			label = UC.distanceUnits;

		break;

	case navGS:
		label = UC.speedUnits;
		break;

	case navETE:
		label = TimeModeStr[timeETE];
		break;

	case navVNAVETE:
		label = TimeModeStr[timeETV];
		break;

	case navTurnETE:
		label = TimeModeStr[timeETT];
		break;

	case navETA:
//		label = TimeModeStr[timeETA];
		CalculateETAHMS(navInfo.timeTo, &hour, &min, &sec);
		StrPrintF(seconds,":%02d", sec);
		label = seconds;
		break;

	case navLocal:
		label = TimeModeStr[timeLocal];
		break;

	case navUTC:
		label = TimeModeStr[timeUTC];
		break;

	case navAltitude:
	case navVNAVAltError:
		label = UC.altitudeUnits;
		break;

	case navFlightDirectorRoll:
		label=UC.heading;
		break;

	default:
		label = NULL;
		ModErrThrow();

	}

	return DrawLargeNumberSmallText(data, label, x, y, alignment, font);
	
}


WaypointIDType NavGetIDOfPlanWaypoint(Int16 wpNum) {
	
	const FlightPlanLegWaypointType *wp = FpGetWaypoint(FlightPlan, wpNum);
	WaypointIDType wpID = WDMSearchForWaypointByLocation(WPDataset, wp->ident, wp->lat, wp->lon, 0);

	return wpID;
	
}

void NavCalculateETE(char *result, Int32 secondsToGo) {

	if (secondsToGo < 86400) {

		if (secondsToGo < 60) 

			StrPrintF(result,"-%02ld", secondsToGo);

		else 

		StrPrintF(result, "%02ld.%02ld", (secondsToGo+30)/3600, ((secondsToGo+30)/60) % 60);

	}

	else

		StrPrintF(result,"--.--");
	
}
