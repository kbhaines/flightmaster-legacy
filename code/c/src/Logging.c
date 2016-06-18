#include "Platform.h"
#include "Modules.h"
#include "Utils.h"

#define ModuleID LoggingModuleID

static DmOpenRef logDb;
static UInt16 *logPtr;
static void *theLog = NULL;
static UInt16 funcLevel = 0;
static HostFILE *hostFile;
static char   *logData;
static char   *line;

static void LogData(const char *data) {

	UInt16 len;
	UInt16 newLogPtr;

	if (theLog == NULL) return;
	
	len = StrPrintF(logData, "%ld%*c%s", PFGetTicks(),(Int16)funcLevel+1,32,data)+1;

	ModErrThrowIf(len > 1024);

	DmWrite(theLog, *logPtr, logData, len);

	newLogPtr = *logPtr + len;
	if (newLogPtr > 63900) newLogPtr = sizeof(newLogPtr);
	DmWrite(theLog, 0, &newLogPtr, sizeof(newLogPtr));

	HostTraceOutputTL(0, "%s", logData);

	if (hostFile) {
		
		HostFPutS(logData,  hostFile);
		HostFFlush(hostFile);

	}

	//PFDrawChars(logData, len,0,0);
}

void LogOpen(void) {

	UInt16 recs;
	MemHandle logRec;
	
	HostTraceInit();
	HostTraceOutputTL(0, "%s", "FlightMaster");

	logData = PFMalloc1(1024);
	line = PFMalloc1(768);

	logDb = DBOpen("FlightMaster-BLOG", false, true);
	recs = DBGetNumRecords(logDb);

	if (recs == 0) {

		UInt16 index = dmMaxRecordIndex;
		UInt16 start = sizeof(UInt16);

		/*
		 * new log, create the logging record
		 *
		 */

		logRec = DmNewRecord(logDb, &index, 64000);
		theLog = MemHandleLock(logRec);

		DmSet(theLog, 0, 64000, 0);
		DmWrite(theLog, 0, &start, sizeof(start));

	} else {

		logRec = DmGetRecord(logDb, 0);
		theLog = MemHandleLock(logRec);

	}

	logPtr = (UInt16*)theLog;

	hostFile = HostLogFile();

	LogData("Built " __DATE__ " " __TIME__);
	LogData("LOG: OPEN");

}

void LogClose(void) {

	LogData("LOG: CLOSED");
	MemPtrUnlock(theLog);
	DmReleaseRecord(logDb, 0, true);
	PFMemFree(logData);
	PFMemFree(line);

	DBClose(logDb);

	HostTraceOutputTL(0, "Closing", "");

	HostTraceClose();
}

void LogEntry(const char *fname) {

	char str[60];

	StrPrintF(str,"FN: %s", fname);
	LogData(str);
	funcLevel++;

}

void LogExit(const char *fname,UInt16 line) {

	char str[60];

	funcLevel--;
	StrPrintF(str,"FX: %s:%d", fname, line);
	LogData(str);

}

void LogLine(UInt16 lineNo) {

	StrPrintF(line, "LN:%d", lineNo);
	LogData(line);
}

void LogTag(const char *tag) {

	StrPrintF(line, "TG:%s", tag);
	LogData(line);

}

void LogInt16(const char *name, Int16 val) {

	StrPrintF(line, "VR:%s = %i", name, val);
	LogData(line);
}

void LogInt32(const char *name, Int32 val) {

	StrPrintF(line, "VR:%s = %li", name, val);
	LogData(line);
}

void LogUInt32(const char *name, UInt32 val) {

	StrPrintF(line, "VR:%s = %lu", name, val);
	LogData(line);
}

void LogDouble(const char *name, double val) {

	StrPrintF(line, "VR:%s = %s", name, DoubleToStr(val,5));
	LogData(line);

}

void LogString(const char *name, const char *s) {

	StrPrintF(line, "VR:%s = %s", name, s);
	LogData(line);
}

