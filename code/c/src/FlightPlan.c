/*
 * FlightPlan.c
 *
 * Implementation of a FlightPlan ADT
 *
 */

#include "Platform.h"
#include "FlightPlan.h"
#include "GlobalTypes.h"
#include "AvCalcs.h"
#include "Modules.h"

#define ModuleID FlightPlanModuleID

/*******************************************************************************/

#define WAYPOINTTOLEG(n) ( (n) > 0 ? (n)-1 : 0)


struct FlightPlanType {
	
	/*
	 * version number of flight plan; incremented when the flight plan 
	 * is changed
	 *
	 * major - incremented on:
	 *
	 * 	edits, copy
	 * 
	 * minor - incremented on same as major plus:
	 *
	 * 		leg change, VNAV set
	 *
	 * IF CHANGING THIS STRUCTURE, REMEMBER TO CHANGE THE FILE MAGIC
	 * VALUE BELOW!!
	 *
	 */

	Int16 minorVersion;
	Int16 majorVersion;

	/*
	 * numLegs = -1 when the flight plan is blank
	 * numLegs = 0 when the flight plan has a starting waypoint, but nothing else
	 * 
	 */
	
	Int16 numLegs;
	
	Int16 currentLeg;

	FlightPlanLegType leg[MAX_FP_LEGS];

	// the following are deprecated, and left in only to maintain file-compatible
	// format between releases of FM
	
	Int32 gotoLat, gotoLon;	// deprecated!!
	float gotoMagVar;		// deprecated!!
	Boolean gotoActive;		// deprecated!!

};

#define FP_STACK_SIZE 256

struct FlightPlanStackType {

	Int16 stackSize;

	FlightPlanType plan[FP_STACK_SIZE];

};

//static const UInt32 fileMagic = 0x44115544;		// serial number for stack file v7.5
static const UInt32 fileMagic = 0x44115566;		// serial number for stack file v7.6

/*****************************************************************************
 *
 * private functions
 *
 */

static Boolean WaypointCompare(const FlightPlanLegWaypointType *wp1, const FlightPlanLegWaypointType *wp2) ADT2_SECTION;
static void SetLegTrack(FlightPlanType fp, Int16 legNum) ADT2_SECTION;
static void InitialiseLeg(FlightPlanLegType *leg) ADT2_SECTION;
static void ValidateAndAdjustCurrentLeg(FlightPlanType fp)  ADT2_SECTION;

/*
 * function : WaypointCompare
 *
 * Returns true if the two waypoints match
 *
 */

Boolean WaypointCompare(const FlightPlanLegWaypointType *wp1, const FlightPlanLegWaypointType *wp2) {

	if ((wp1->lat == wp2->lat) &&
			(wp1->lon == wp2->lon) ) return true;

	return false;

}

/*
 * function : SetLegTrack
 *
 * Sets track and distance information for the specified leg
 *
 */

static void SetLegTrack(FlightPlanType fp, Int16 legNum) {

	const FlightPlanLegWaypointType *wp1, *wp2;
	double distance;

	LOGENTRY;

	wp1 = FpGetWaypoint(fp, legNum);
	wp2 = FpGetWaypoint(fp, legNum+1);
	
	ModErrThrowIf(!wp1 || !wp2);

	fp->leg[legNum].track = (float) IntCalcGreatCircleCourse(
		wp1->lat, wp1->lon, 
		wp2->lat, wp2->lon, 
		&distance);

	fp->leg[legNum].distance = (float) distance;

	LOGEXIT;

}

/*
 * function : InitialiseLeg
 *
 */

static void InitialiseLeg(FlightPlanLegType *leg) {

	leg->altitude = 0.0;
	leg->distance = 0.0;
	leg->track = 0.0;
	leg->vnavSet = false;

}

/*
 * ValidateAndAdjustCurrentLeg
 * 
 */

static void ValidateAndAdjustCurrentLeg(FlightPlanType fp) {
	
	if (fp->currentLeg >= fp->numLegs) fp->currentLeg = fp->numLegs-1;
	
}


/*******************************************************************************
 *
 * public functions
 *
 */

/*
 *  function: FpLegWaypointNew
 *
 */

FlightPlanLegWaypointType *FpLegWaypointNew(Int32 lat, Int32 lon, const char *ident, float magVar) {

	FlightPlanLegWaypointType *wp = PFMalloc(sizeof(FlightPlanLegWaypointType));

	ModErrThrowIf(!wp);

	wp->lat = lat;
	wp->lon = lon;
	StrNCopy(wp->ident, ident, sizeof(wp->ident));
	wp->magVar = magVar;

	return wp;
	
}

/*
 * function : FpNew
 *
 */

FlightPlanType FpNew(void) {

	FlightPlanType fp = NULL;

	PFSafeMalloc(fp, sizeof(struct FlightPlanType));

	FpInit(fp);

	return fp;

}

/*
 * function : FpFree
 *
 */

void FpFree(FlightPlanType this) {

	PFMemFree(this);

}

/*
 * function : FpSizeOf
 *
 */

const UInt32 FpSizeOf(void) {

	return sizeof(struct FlightPlanType);

}

/*
 * function : FpInit
 *
 */

void FpInit(FlightPlanType this) {

	Int16 j;

	this->minorVersion  = PFGetTicks() & 0xffff;
	this->majorVersion  = PFGetTicks() & 0xffff;
	this->numLegs  = -1;
	this->currentLeg = 0;
	this->gotoActive = false;

	for (j=0;j < MAX_FP_LEGS;j++) InitialiseLeg(&this->leg[j]);

}

/*
 * function : FpCopy
 *
 */

void FpCopy(FlightPlanType dest, FlightPlanType src) {

	PFMemMove((void*)dest, (void*)src, sizeof(struct FlightPlanType));

	dest->minorVersion++;
	dest->majorVersion++;

}

/*
 * function : FpNextLeg
 *
 */

Boolean FpNextLeg(FlightPlanType this) {

	if (this->currentLeg < this->numLegs - 1) {

		this->currentLeg++;
		this->minorVersion++;

		return true;

	}

	return false;

}

/*
 * function : FpPreviousLeg
 *
 */

Boolean FpPreviousLeg(FlightPlanType this) {

	if (this->currentLeg > 0) {

		this->currentLeg--;

		this->minorVersion++;

		return true;

	}

	return false;

}

/*
 * function : FpSetCurrentLeg
 *
 */

void FpSetCurrentLeg(FlightPlanType this, Int16 leg) {

	ModErrThrowIf(leg >= this->numLegs || leg < 0);

 	this->currentLeg = leg;

	this->minorVersion++;

}

/*
 * function : FpGetFromWaypoint
 *
 */

const FlightPlanLegWaypointType *FpGetFromWaypoint(const FlightPlanType this, Int16 leg) {

	ModErrThrowIf(leg > this->numLegs - 1);

	return FpGetWaypoint(this, leg);

}

/*
 * function : FpGetToWaypoint
 *
 */

const FlightPlanLegWaypointType *FpGetToWaypoint(const FlightPlanType this, Int16 leg) {

	ModErrThrowIf(leg > this->numLegs - 1);

	return FpGetWaypoint(this, leg+1);

}

/*
 * function : FpInsertWaypoint
 *
 */

Boolean FpInsertWaypoint(FlightPlanType this, Int16 beforeWaypoint, Int32 lat, Int32 lon, const char *ident, float magVar, float altitude) {

	Int16 j;
	const Int16 leg = WAYPOINTTOLEG(beforeWaypoint);
	FlightPlanLegWaypointType oldLegB = this->leg[leg].b;
	FlightPlanLegWaypointType *newWp;

	LOGENTRY;

	if (FpFindWaypoint(this, lat, lon) != fpWaypointNotFound) {
		
		switch (FpFindWaypoint(this, lat, lon) - beforeWaypoint) {
		
		case -1:
		case 0:
			LOGEXIT;
			return false;
			break;
		
		default:
			// carry on
			
		}
		
	}

	if (this->numLegs == MAX_FP_LEGS-1) {

		LOGEXIT;
		return false;

	}

	this->minorVersion++;
	this->majorVersion++;

	if (this->numLegs == -1 || beforeWaypoint > this->numLegs) {
		
		FpAppendWaypoint(this, lat, lon, ident, magVar, altitude);

		LOGEXIT;
		return true;
		
	}

	newWp = FpLegWaypointNew(lat, lon, ident, magVar);

	if (this->numLegs == 0) {
		
		this->leg[0].b = this->leg[0].a;
		this->leg[0].a = *newWp;
		this->leg[0].vnavSet = false;
		this->numLegs = 1;
		SetLegTrack(this, 0);
		PFMemFree(newWp);
	
		LOGEXIT;
		return true;
		
	}

	/*
	 * check previous and next waypoints aren't the same
	 *
	 */

	if (WaypointCompare(&this->leg[leg].a, newWp) || WaypointCompare(&this->leg[leg].b, newWp)) {

		PFMemFree(newWp);

		LOGEXIT;
		return false;

	}

	/*
	 * make space for new leg
	 *
	 */

	for (j=this->numLegs; j > leg; j--)
		this->leg[j] = this->leg[j-1];
 
	/*
	 * don't use InitialiseLeg to set up the new leg - let
	 * it inherit the altitude from the existing leg
	 *
	 */

	this->numLegs++;

	if (beforeWaypoint == 0) {

		this->leg[0].a = *newWp;
		this->leg[0].b = this->leg[1].a; 	

	} else { 
	
		this->leg[leg].b = *newWp;
			
		this->leg[leg+1].a = *newWp;
		this->leg[leg+1].b = oldLegB;
		
	}

	this->leg[leg].vnavSet  = false;

	SetLegTrack(this, leg);
	if (leg < this->numLegs-1) SetLegTrack(this, leg+1);

	PFMemFree(newWp);

	LOGEXIT;
	return true;

}

/*
 * Function : FpAppendWaypoint
 *
 */

Boolean FpAppendWaypoint(FlightPlanType this, Int32 lat, Int32 lon, const char *ident, float magVar, float altitude) {

	Int16 i = this->numLegs;
	FlightPlanLegWaypointType *newWp;

	if (this->numLegs == MAX_FP_LEGS-1) return false;

	newWp = FpLegWaypointNew(lat, lon, ident, magVar);

	this->minorVersion++;
	this->majorVersion++;

	if (this->numLegs == -1) {

		this->leg[0].a = *newWp;

		this->numLegs = 0;
		PFMemFree(newWp);

		return true;

	} else if (this->numLegs == 0) {

		this->leg[0].b = *newWp;

		this->leg[0].altitude = altitude;
		this->numLegs = 1;

		this->currentLeg = 0;

		SetLegTrack(this, 0);

		PFMemFree(newWp);
		return true;

	}

	/*
	 * check previous waypoint isn't the same
	 *
	 */
	
	if (WaypointCompare(&this->leg[i-1].b, newWp)) return false;

	InitialiseLeg(&this->leg[i]);
	this->leg[i].a = this->leg[i-1].b;
	this->leg[i].b = *newWp;
	
	this->leg[i].altitude = altitude;

	this->numLegs++;
	SetLegTrack(this, i);

	PFMemFree(newWp);

	return true;

}

/*
 * function : FpDeleteWaypoint
 *
 */

Boolean FpDeleteWaypoint(FlightPlanType this, Int16 wpNum) {

	Int16 i;

	ModErrThrowIf(wpNum > this->numLegs || wpNum < 0);

	if (wpNum == 0) {

		if (this->numLegs == 0) {
			
			this->numLegs = -1;
			
		} else if (this->numLegs == 1) {

			this->leg[0].a = this->leg[0].b;
			this->numLegs = 0;

			this->minorVersion++;
			this->majorVersion++;

		} else {

			FpDeleteLeg(this, 0);
			
		}
		
		return true;

	}

	/*
	 * connect the end of the specified leg to the end of the next leg
	 *
	 */

	if (wpNum < this->numLegs) {

		/*
		 * not deleting last waypoint, so jiggle the plan around
		 *
		 */

		this->leg[wpNum-1].b = this->leg[wpNum].b;
		SetLegTrack(this, wpNum-1);

		for (i=wpNum; i<this->numLegs;i++) this->leg[i] = this->leg[i+1];

	}

	this->numLegs--;
	ValidateAndAdjustCurrentLeg(this);
	
	this->minorVersion++;
	this->majorVersion++;

	return true;

}

/*
 * function : FpFindWaypoint
 *
 * Looks for the specified waypoint in the flight plan, and returns
 * the waypoint number. Returns -1 if not found
 *
 */

Int16 FpFindWaypoint(const FlightPlanType this, Int32 lat, Int32 lon) {

	Int16 j;

	if (!this) return fpWaypointNotFound;
	
	if (this->numLegs < 0) return fpWaypointNotFound;
	
	for (j=0; j<this->numLegs+1;j++) {

		const FlightPlanLegWaypointType *wp = FpGetWaypoint(this, j);

		if (wp->lat == lat && wp->lon == lon) return j;

	}

	return fpWaypointNotFound;

}

/*
 *  function: FpSetNewFirstLeg
 *
 */

void FpSetNewFirstLeg(FlightPlanType this, Int32 fromLat, Int32 fromLon, const char *fromIdent, float fromMagVar,
		Int32 toLat, Int32 toLon, const char *toIdent, float toMagVar) {

	Int16 j;
	Int16 toWaypointInPlan = FpFindWaypoint(this, toLat, toLon);

	if (FpIsBlank(this)) {
		
		FpInit(this);
		
		FpAppendWaypoint(this, fromLat, fromLon, fromIdent, fromMagVar, 0.0);
		FpAppendWaypoint(this, toLat, toLon, toIdent, toMagVar, 0.0);
		
		return;
	}
	
	if (toWaypointInPlan != fpWaypointNotFound) {

		for (j=0; j<toWaypointInPlan;j++) FpDeleteWaypoint(this,0);

	} else {

		Int16 numToDelete = FpGetCurrentLeg(this)+1;
		
		for (j=0; j<numToDelete;j++) FpDeleteWaypoint(this,0);
		FpInsertWaypoint(this,0, toLat, toLon, toIdent, toMagVar,0.0);

	}

	FpInsertWaypoint(this, 0, fromLat, fromLon, fromIdent, fromMagVar,0.0);

}



/*
 * function : FpDeleteLeg
 *
 */

Boolean FpDeleteLeg(FlightPlanType this, Int16 legNum) {

	Int16 j;
	
	ModErrThrowIf(legNum >= FpGetNumLegs(this));

	for (j=legNum; j < FpGetNumLegs(this) - 1;j++) {

		this->leg[j] = this->leg[j+1];

	}

	this->numLegs--;
	if (this->numLegs == 0) this->numLegs = -1;
	if (this->currentLeg  > legNum) this->currentLeg--;
	
	ValidateAndAdjustCurrentLeg(this);

	this->minorVersion++;
	this->majorVersion++;

	return true;

}

Int16 FpGetCurrentLeg(const FlightPlanType this) { return this->currentLeg; }

Int16 FpGetNumLegs(const FlightPlanType this) { return this->numLegs; }

Boolean FpIsBlank(const FlightPlanType this) { return (this->numLegs < 1); }

const FlightPlanLegType *FpGetLeg(const FlightPlanType this, Int16 leg, 
		Boolean obs) {

	ModErrThrowIf((!obs && leg >= this->numLegs) || (obs && leg>1) );

	return &(this->leg[leg]);

}

/*
 * function : FpGetWaypoint
 *
 */

const FlightPlanLegWaypointType *FpGetWaypoint(const FlightPlanType this, Int16 wpNum) {

	const FlightPlanLegWaypointType *fplw;

	LOGENTRY;
	LOGINT16(wpNum);

	ModErrThrowIf(wpNum > this->numLegs);

	if (wpNum == 0) {

		fplw = &this->leg[0].a;

	} else {

		fplw = &this->leg[wpNum-1].b;

	}

	return fplw;

}

/*
 * function : FpSetVNAV
 *
 */

void FpSetVNAV(FlightPlanType this, Int16 leg, const VNAVDataType *vd) {

	ModErrThrowIf(leg >= this->numLegs);

	this->leg[leg].vnavSet = true;
	this->leg[leg].vnav = *vd;

	this->minorVersion++;

}

/*
 * function : FpVNAVIsSet
 *
 */

Boolean FpVNAVIsSet(FlightPlanType this, Int16 leg) {

	ModErrThrowIf(leg >= this->numLegs);

	return this->leg[leg].vnavSet;

}
	
/*
 * function : FPGetVNAV
 *
 */

const VNAVDataType *FpGetVNAV(FlightPlanType this, Int16 leg) {

	ModErrThrowIf(leg >= this->numLegs);

	return this->leg[leg].vnavSet ? &(this->leg[leg].vnav) : NULL;

}

/*
 * function : FpClearVNAV
 *
 */

void FpClearVNAV(const FlightPlanType this, Int16 leg) {

	ModErrThrowIf(leg >= this->numLegs);
	this->leg[leg].vnavSet = false;

	this->minorVersion++;

}

/*
 * function : FpGetVersion
 *
 */

Int16 FpGetVersion(const FlightPlanType this, FlightPlanVersionType fpv) {

	if (fpv == minorVersion) return this->minorVersion;

	return this->majorVersion;

}

/*
 * function : FpSaveToFile
 *
 */

Int32 FpSaveToFile(FlightPlanType this, PFFileRef file) {

	Int32 written;

	written = PFWriteFile(file, this, FpSizeOf());

	return written;

}

/*
 * function : FpLoadFromFile
 *
 */

Int32 FpLoadFromFile(FlightPlanType this, PFFileRef file) {

	Int32 read = PFReadFile(file, this, FpSizeOf());

	return read;

}

/*
 * function : FpStackNew
 *
 */

FlightPlanStackType FpStackNew(void) { 

	FlightPlanStackType s = NULL;

	PFSafeMalloc(s, sizeof(struct FlightPlanStackType));

	s->stackSize = 0;

	return s;

}

/*
 * function : FpStackPush
 *
 */

Boolean FpStackPush(FlightPlanStackType this, FlightPlanType fp) {

	FlightPlanType newFp;

	LOGENTRY;

	if (this->stackSize == FP_STACK_SIZE) {

		LOGEXIT;
		return false;

	}

	/*
	 * make a copy, push that onto the stack
	 *
	 */

	newFp = FpNew();
	FpCopy(newFp, fp);

	this->plan[this->stackSize] = newFp;
	this->stackSize++;

	LOGINT16(this->stackSize);
	LOGEXIT;
	return true;

}

/*
 * function : FpStackPop
 *
 */

FlightPlanType FpStackPop(FlightPlanStackType this) {

	LOGENTRY;

	if (this->stackSize) {

		LOGINT16(this->stackSize);

		this->stackSize--;

		this->plan[this->stackSize]->minorVersion = PFGetTicks() & 0xffff;
		this->plan[this->stackSize]->majorVersion = PFGetTicks() & 0xffff;

		LOGEXIT;
		return this->plan[this->stackSize];

	} else {

		LOGTAG("Empty");
		LOGEXIT;
		return NULL;

	}

}


void FpStackPopOverExisting(FlightPlanStackType this, FlightPlanType *fp) {
	
	FlightPlanType poppedFlight = FpStackPop(this);
	
	if (poppedFlight) { 
		
		FpFree(*fp);
		*fp = poppedFlight;
		
	}

}

/*
 * function : FpStackPeek
 *
 */

const FlightPlanType FpStackPeek(FlightPlanStackType this, Int16 num) {

	LOGENTRY;

	num++;

	if (this->stackSize - num >= 0) {

		return this->plan[this->stackSize - num];

	}

	return NULL;

}

/*
 * function : FpStackFree
 *
 */

void FpStackFree(FlightPlanStackType this) {

	Int16 j;

	for (j = 0; j < this->stackSize; j++) {

		FpFree(this->plan[j]);

	}

	PFMemFree(this);

}

/*
 * function : FpStackSave
 *
 */

Boolean FpStackSave(FlightPlanStackType this, const char *filename) {

	Int16 j;
	PFFileRef file;

	file = PFOpenFile(filename, pfFileTruncate);
	if (!file) return false;

	/*
	 * store magic
	 *
	 */

	PFWriteFile(file, &fileMagic, sizeof(fileMagic));

	for (j=0;j<this->stackSize;j++) {

		PFWriteFile(file, this->plan[j], sizeof(struct FlightPlanType));

	}

	PFCloseFile(file);
		
	return true;

}

/*
 * function : FpStackLoad
 *
 */

Boolean FpStackLoad(FlightPlanStackType this, const char *filename) {

	PFFileRef file;
	FlightPlanType fp;
	UInt32 magic;

	ModErrThrowIf(!this || this->stackSize);
	
	file = PFOpenFile(filename, pfFileReadOnly);
	if (!file) return false;

	/*
	 * check for magic
	 *
	 */

	PFReadFile(file, &magic, sizeof(magic));
	if (magic != fileMagic) {
		
		PFCloseFile(file);
		return false;

	}

	fp = FpNew();

	while (FpLoadFromFile(fp, file) == FpSizeOf()) {

		FpStackPush(this, fp);

	}

	FpFree(fp);

	PFCloseFile(file);

	return true;

}

/*
 * function : FpSwapLongitudes
 * 
 */

void FpSwapLongitudes(FlightPlanType fp) {

	Int16 j;
	
	for (j = 0; j < fp->numLegs; ++j) {
		
		fp->leg[j].a.lon = -fp->leg[j].a.lon;
		fp->leg[j].b.lon = -fp->leg[j].b.lon;
		
	}
}