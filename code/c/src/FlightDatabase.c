/*
 * FlightDatabase
 *
 */

#include "FlightDatabase.h"
#include "Modules.h"

#define ModuleID FlightDatabaseModuleID

/*******************************************************************************
 *
 * module variables
 *
 */

static PFFileRef flightDB = NULL;

/*******************************************************************************
 *
 * private functions
 *
 */

#define MAX_FILENAME_LENGTH 32

#define RECORDSIZE (2*FpSizeOf() + MAX_FILENAME_LENGTH)

#define FLIGHT_SEEK(j) PFSeekFile(flightDB, pfSeekStart, (((j)*RECORDSIZE)))

const UInt32 dbFileMagic = 0x32324545;

/*******************************************************************************
 *
 * functions
 *
 */

/*
 * FlightDBOpen
 *
 */

void FlightDBOpen(void)  {

	flightDB = PFOpenFile(APPNAME"-Flight", pfFileReadWrite);

	ModErrThrowIf(!flightDB);

}

/*
 * FlightDBClose
 *
 */

void FlightDBClose(void)  {

	PFCloseFile(flightDB);

}


/*
 * FlightDBGetNumRecords
 *
 */

Int16 FlightDBGetNumRecords(void)  {

	return PFFileSize(flightDB) / RECORDSIZE;

}

/*
 * FlightDBGetDescription
 *
 */

char *FlightDBGetName(Int16 flightNum)  {

	static char result[MAX_FILENAME_LENGTH];

	FLIGHT_SEEK(flightNum);
	PFReadFile(flightDB, &result, sizeof(result));

	return result;

}

/*
 * FlightDBGetFlight
 *
 */

FlightPlanType FlightDBGetFlight(Int16 flightNum, Boolean alternate) {

	FlightPlanType fp = FpNew();

	FLIGHT_SEEK(flightNum);
	PFSeekFile(flightDB, pfSeekCurrent, MAX_FILENAME_LENGTH);
	PFSeekFile(flightDB, pfSeekCurrent, alternate ? FpSizeOf() : 0);

	if (FpLoadFromFile(fp, flightDB) != FpSizeOf()) {

		FpFree(fp);
		return NULL;

	}

	return fp;

}

/*
 * FlightDBSaveFlight
 *
 */

Boolean FlightDBSaveFlight(const char *name, FlightPlanType flight, FlightPlanType alternate, Boolean overwrite) {

	Int16 j;
	Int16 numFlights = FlightDBGetNumRecords();
	
	/*
	 * does the named flight already exist?
	 *
	 */

	for (j = 0;j  < numFlights;j++) {

		if (StrCompareAscii(name, FlightDBGetName(j)) == 0) break;

	}

	if (j != numFlights && !overwrite) return false;

	FLIGHT_SEEK(j);

	PFWriteFile(flightDB, name, MAX_FILENAME_LENGTH);
	FpSaveToFile(flight, flightDB);
	FpSaveToFile(alternate, flightDB);

	return true;

}

/*
 * FlightDBDeleteFlight
 *
 */

Boolean FlightDBDeleteFlight(Int16 flightNum) {

	char name[32];
	FlightPlanType fp1 = FpNew();
	FlightPlanType fp2 = FpNew();

	Int16 j;
	Int16 numFlights = FlightDBGetNumRecords();

	ModErrThrowIf(flightNum >= numFlights);

	for (j = flightNum; j < numFlights-1; j++) {

		FLIGHT_SEEK(j+1);
		PFReadFile(flightDB, name, sizeof(name));
		PFReadFile(flightDB, fp1, FpSizeOf());
		PFReadFile(flightDB, fp2, FpSizeOf());

		FLIGHT_SEEK(j);
		PFWriteFile(flightDB, name, sizeof(name));
		PFWriteFile(flightDB, fp1, FpSizeOf());
		PFWriteFile(flightDB, fp2, FpSizeOf());

	}

	PFTruncateFile(flightDB, PFFileSize(flightDB) - RECORDSIZE);

	return true;

}

void FlightDBUpgrade(void) {
	
	Int16 j;
	
	for (j = 0; j < FlightDBGetNumRecords(); ++j) {
		
		FlightPlanType fp = FlightDBGetFlight(j, false);
		FlightPlanType fpAlt = FlightDBGetFlight(j, true);
		
		FpSwapLongitudes(fp);
		FpSwapLongitudes(fpAlt);
		FlightDBSaveFlight(FlightDBGetName(j), fp, fpAlt, true);
		FpFree(fp);
		FpFree(fpAlt);
				
	}
}
