/*
 * Platform.c
 *
 * Platform is a platform abstraction layer module that allows FlightMaster to
 * interface to Hardware and OS-level features in an independant manner.
 *
 */

//#define ALLOW_ACCESS_TO_INTERNALS_OF_CONTROLS 1

#ifdef TEST_FRAMEWORK
static const char *testFrameworkIncluded = "TEST_FRAMEWORK";
#endif
	
#include "Platform.h"
#include "Modules.h"
#include "Utils.h"
#include "GlobalTypes.h"
#include "PalmNavigator.h"  //TODO  - remove PalmNav from extsrc
#include "GarminChars.h"
#include "ResourceDefines.h"
#include <MemGlue.h>
#include <PalmCompatibility.h>
#include <AlarmMgr.h>
#include <DLServer.h>
#include <PceNativeCall.h>
#include "CollapseUtils.h"
#include <SerialMgr.h>
#include <SerialMgrOld.h>
#include <PalmOSGlue.h>
#include <HSExt.h>

#define ModuleID PlatformModuleID

const UInt32 FMCreatorID = 'BHMN';
const UInt32 FMDBType = 'FMDB';


//const Coord DevicePixelsScalar = 320;


#define PfErrThrowIf(c) if (c) ErrThrow(__LINE__)

#define SKIP_FIELD(p) while((p)[0]>' ')(p)++;while ((p)[0]<'!') (p)++

static UInt32 appLaunchTime = 0;

static Boolean pfHasSound = false;


struct PFFileRefStructType {

	Boolean external;

	FileRef extRef;
	FileHand intRef;

};


ScreenInfoType pfScreen;

/*******************************************************************************
 * 
 * Internal/Helper Functions
 *
 */


static void SetupScreenInfo(void) PLATFORM_SECTION;
static void *GetObjectPtr2(FormPtr form, UInt16 objectID) PLATFORM_SECTION;	//2 stops clash with utils for now
#define GetFormObject(objectID) GetObjectPtr2(FrmGetActiveForm(), objectID)

static void LineShift(LineType *l, Int16 numLines, Boolean outside) PLATFORM_SECTION;

static WChar CheckForLongPress(WChar key) PLATFORM_SECTION;
static Err RomVersionCompatible(UInt32 requiredVersion, UInt16 launchFlags) PLATFORM_SECTION;

#define HardKeyDown(key) (KeyCurrentState() & (key))

/*
 * function : SetupScreenInfo
 *
 */

void SetupScreenInfo(void) {

	UInt32 version;

	WinSetCoordinateSystem(kCoordinatesNative);
	
	WinGetDisplayExtent(&pfScreen.width, &pfScreen.height);

	/*
	 * check for Sony Clie - in this case, only 160x160 is supported
	 *
	 */

	if (!FtrGet(sysFtrCreator, sysFtrNumOEMCompanyID, &version)) {

		if (version == 'sony') {

			pfScreen.width = 160;
			pfScreen.height = 160;

		}
	
	}

	pfScreen.landscape = pfScreen.width > pfScreen.height ? true : false;

	FntSetFont(stdFont);
	pfScreen.stdCharWidth = FntCharWidth('A');
	
	FntSetFont(boldFont);
	pfScreen.boldHeight = FntCharHeight();
	FntSetFont(largeBoldFont);
	pfScreen.largeBoldHeight = FntCharHeight();
	FntSetFont(ledFont);
	pfScreen.ledHeight = FntCharHeight();

	/*
	 * high density screen available?
	 *
	 */
	
	LOGLINE;
	FtrGet(sysFtrCreator, sysFtrNumWinVersion, &version);
	pfScreen.highDensity = (version >= 4) ? true : false;

	if (pfScreen.highDensity) {

		Int32 x;

		WinScreenGetAttribute(winScreenWidth, &x);
		LOGINT16((Int16)x);

		// reset highDensity related variables if x < 320

		if (x < 320) {

			pfScreen.highDensity = false;

		}

	}

	pfScreen.xcentre = pfScreen.width / 2;
	pfScreen.ycentre = pfScreen.height / 2;

	WinSetCoordinateSystem(kCoordinatesStandard);

	LOGINT16(pfScreen.highDensity);
	LOGINT16(pfScreen.width);
	LOGINT16(pfScreen.height);

}


/*
 * GetObjectPtr2
 *
 * Return pointer to the specified object located by its ID
 *
 */

static void *GetObjectPtr2(FormPtr form, UInt16 objectID) {

	return FrmGetObjectPtr(form, FrmGetObjectIndex(form, objectID));

}

/*
 * LineShift
 *
 * Move line points inside or outside of themselves
 *
 */

static void LineShift(LineType *l, Int16 numLines, Boolean outside) {

	Int16 j;

	for (j = 0; j < numLines; j++) {

		Int16 dx = l[j].x2 - l[j].x1;
		Int16 mx = dx;
		Int16 dy = l[j].y2 - l[j].y1;
		Int16 my = dy;

		/*
		 * calculate shift to inside of line
		 *
		 */

		if (mx < 0) mx =- mx;
		if (my < 0) my =- my;

		if (mx > my) {

			if (dx < 0)  {

				mx = 0;
				my = -1;

			} else {

				mx = 0;
				my = 1;

			}

		} else {

			if (dy < 0) {

				mx = 1; my = 0;

			} else {

				mx = -1; my =0;

			}
		}

		if (outside) {

			mx = -mx;
			my = -my;

		}

		l[j].x1 += mx; l[j].x2 += mx;
		l[j].y1 += my; l[j].y2 += my;

	}

}



#ifdef TEST_FRAMEWORK

static PFFileRef recordEventFile = NULL;
static PFFileRef playbackEventFile = NULL;
static PFFileRef playbackLogFile = NULL;

static void RecordEvent(UInt32 ticks, EventPtr e) PLATFORM_SECTION;
static void PlayBackEvent(PFFileRef f) PLATFORM_SECTION;
static const char *LookupResourceIdentifier(UInt16 resID) PLATFORM_SECTION;
static UInt16 LookupResourceID(const char *identifier) PLATFORM_SECTION;
static void SaveScreen(const char *name, UInt32 number) PLATFORM_SECTION;
static Boolean EventTapIsInObject(EventPtr evt) PLATFORM_SECTION;
static RecordAlertEvent(UInt16 alertID, UInt16 result) PLATFORM_SECTION;


static const char *LookupResourceIdentifier(UInt16 resID) {

	Int16 j;

	static const 
	#include "ResourceLookupTable.c"


	LOGINT16(resID);
	for (j=0; resourceLookupTable[j].token; j++ ) {
		
		if (resourceLookupTable[j].resID == resID) 
			return resourceLookupTable[j].token; 
		
	}

	LOGINT16(j);
	ModErrThrow();
	
}

static UInt16 LookupResourceID(const char *identifier) {

	Int16 j;

	static const 
	#include "ResourceLookupTable.c"

	for (j=0; resourceLookupTable[j].token; j++ ) {
		
		if (StrCompare(identifier, resourceLookupTable[j].token) == 0) 
			return resourceLookupTable[j].resID; 
		
	}

	LOGINT16(j);
	ModErrThrow();
	
}

static void SaveScreen(const char *name, UInt32 number) {
	
	PFFileRef ref;
	char filename[64];
	
	Int16 x,y;
	unsigned char *screenPtr = PFGetScreenAddressAndDimensions(NULL, NULL,NULL);
	
	unsigned char *oneRow = PFMalloc(pfScreen.width * 3);
	
	StrPrintF(filename,"/FMTest/%s-%02ld.raw", name, number);
	
	ref = PFOpenFile(filename, pfFileReadWrite);
	
	for (y=0;y < pfScreen.height; y++) {
		
		for (x=0; x < pfScreen.width; x++) {
	
			RGBColorType rgb;
			
			PFPaletteGetRGB(*screenPtr++, &rgb);
			MemMove(&oneRow[x*3], &(rgb.r),3);
			
		}

		PFWriteFile(ref, oneRow, pfScreen.width*3);
		
	}
	
	PFCloseFile(ref);
	PFMemFree(oneRow);
	
}

static Boolean EventTapIsInObject(EventPtr event) {
	
	FormPtr form = FrmGetActiveForm();
	Int16 j;
	
	for (j=0;j<FrmGetNumberOfObjects(form);j++) {

		RectangleType r2;
	
		FrmGetObjectBounds(form, j, &r2);
		
//		if (RctPtInRectangle(event->screenX, event->screenY, &r2) &&
//				FrmGetObjectType(form, j) != frmFieldObj) 
		if (RctPtInRectangle(event->screenX, event->screenY, &r2))
			return true;
		
	}
	
	return false;
	
}


static RecordAlertEvent(UInt16 alertID, UInt16 result) {

	char str[128];
	UInt32 timestamp = PFGetTicks() / PFTicksPerSecond();
	
	StrPrintF(str, "%ld alert %s %d\n", timestamp, LookupResourceIdentifier(alertID),
			(result));

	if (recordEventFile) PFWriteFile(recordEventFile, str, StrLen(str));
	

}

/*
 * function : RecordEvent
 *
 */

static void RecordEvent(UInt32 timestamp, EventPtr event) {

	char str[128] = "";

	if (!recordEventFile) return;

	timestamp /= PFTicksPerSecond();
	
	switch (event->eType) {
		
	case lstSelectEvent:
		StrPrintF(str,"%ld listselect %s %s %d\n", timestamp,
				LookupResourceIdentifier(FrmGetActiveFormID()),
				LookupResourceIdentifier(event->data.lstSelect.listID),
				event->data.lstSelect.selection);
		break;
		
	case ctlSelectEvent:
		StrPrintF(str, "%ld control %s %s\n", timestamp, 
				LookupResourceIdentifier(FrmGetActiveFormID()),
				LookupResourceIdentifier(event->data.ctlSelect.controlID));
		break;
		
	case fldEnterEvent:
		StrPrintF(str, "%ld field %s %s\n", timestamp, 
				LookupResourceIdentifier(FrmGetActiveFormID()),
				LookupResourceIdentifier(event->data.ctlSelect.controlID));
		break;
		
	case penDownEvent:
		if (!EventTapIsInObject(event)) {
			
			StrPrintF(str,"%ld tap %s %d %d\n", timestamp, 
					LookupResourceIdentifier(FrmGetActiveFormID()),
					event->screenX, event->screenY);
			
		}
		break;

	case menuEvent:
		StrPrintF(str, "%ld menu %s %s\n", timestamp, 
				LookupResourceIdentifier(FrmGetActiveFormID()),
				LookupResourceIdentifier(event->data.menu.itemID));
		break;
		
	case keyDownEvent:
		if (event->data.keyDown.chr == '#') {

			SaveScreen(LookupResourceIdentifier(FrmGetActiveFormID()), timestamp);
			
			StrPrintF(str,"%ld checkscreen %s\n", timestamp,
					LookupResourceIdentifier(FrmGetActiveFormID()));
			
		} else {
			
			StrPrintF(str,"%ld key %s %d %d %d\n", timestamp, 
					LookupResourceIdentifier(FrmGetActiveFormID()), 
					(UInt16)(event->data.keyDown.chr),	event->data.keyDown.modifiers, 
					event->data.keyDown.keyCode);

		}
		
		break;
			
	default:

		break;
	}

	if (str[0]) {

		PFWriteFile(recordEventFile, str, StrLen(str));

	}

}
static const char *playbackEventCommands[] = {
		
		"control", "tap", "menu", "key", "alert", "checkscreen", "listselect", "field", ""
		
};

typedef enum { pbControl = 0, pbTap = 1, pbMenu = 2, pbKey = 3, 

	pbAlert = 4, pbCheckScreen = 5, pbListSelect = 6, pbField = 7
	
} PlaybackEventType;

static char *StrToken(char* str) PLATFORM_SECTION;

static char *StrToken(char* str) {

	static UInt16 originalStringLen;
	static char *currentToken;
	
	char *nextSplit;
	
	if (str) {
	
		currentToken = str;
		originalStringLen = StrLen(str);
		
	} else {
		
		currentToken += (StrLen(currentToken) + 1);
		if (! *currentToken) return NULL;
		
	}
	
	nextSplit =  StrChr(currentToken, ' ');
	if (nextSplit) *nextSplit=0;
	
	return currentToken;
	
}

static char *playbackAlertName= NULL;
static UInt16 playbackAlertResult = 0;
static Boolean eventIsLoaded = false;

static void PlayBackEvent(PFFileRef f) {
	
	static UInt32 fireEventAtTime = 0;
	static char   *fireEventAtForm;
	static PlaybackEventType fireEventType;
	static char eventLine[256];

	if (!eventIsLoaded) {
		
		char *field;
		Int16 j;
		
		if (!PFReadLine(f, eventLine)) {
			
			fireEventAtTime = 99999;
			fireEventAtForm = NULL;
			eventIsLoaded = true;
			
			PFCloseFile(PFOpenFile("/FMTest/TESTCOMPLETE", pfFileTruncate));
			PFCloseFile(playbackLogFile);
			return;
			
		}
		
		LOGTAG("Loading event");
		LOGSTR(eventLine);
		PFWriteFile(playbackLogFile, eventLine, StrLen(eventLine));
		
		fireEventAtTime = StrAToI(StrToken(eventLine));
		field = StrToken(NULL);
		
		for (j=0; playbackEventCommands[j];j++)
			if (StrCompare(field, playbackEventCommands[j]) == 0) break;

		fireEventType = (PlaybackEventType)j;
		fireEventAtForm = StrToken(NULL);
		eventIsLoaded = true;
		
		LOGSTR(fireEventAtForm);
		
		if (fireEventType == pbAlert) {
			
			LOGTAG("Loading alert event");
			
			playbackAlertName = fireEventAtForm;
			playbackAlertResult = StrAToI(StrToken(NULL));
			
		} else {
			
			playbackAlertName = NULL;
			
		}
		
	}

	
	if (PFGetTicks()/100 > fireEventAtTime+10) {
		
		// timeout - fail the test
		
		SaveScreen("FAILED",0);
		PFCloseFile(PFOpenFile("/FMTest/TESTCOMPLETE", pfFileTruncate));
		PFCloseFile(playbackLogFile);

	}
	
	// do not disable timing check without altering eof code above
	if ( (PFGetTicks()/100) > fireEventAtTime && FrmGetActiveFormID() && StrCompare(LookupResourceIdentifier(FrmGetActiveFormID()), fireEventAtForm)==0) {
		
		EventType e;
		
		LOGTAG("Firing...");
		LOGSTR(fireEventAtForm);
		
		PFWriteFile(playbackLogFile, "...fired\n", 9);
		
		switch (fireEventType) {
		
		case pbControl:
			GUIObjectTap(LookupResourceID(StrToken(NULL)));
			break;
			
		case pbField:
			GUIFieldSetFocus(LookupResourceID(StrToken(NULL)));
			break;
			
		case pbListSelect:
			e.eType = lstSelectEvent;
			e.data.lstSelect.listID = LookupResourceID(StrToken(NULL));
			e.data.lstSelect.selection = StrAToI(StrToken(NULL));
			GUIListSetSelection(e.data.lstSelect.listID, e.data.lstSelect.selection);
			EvtAddEventToQueue(&e);
			break;
			
		case pbTap:
			e.eType = penDownEvent;
			e.screenX = StrAToI(StrToken(NULL));
			e.screenY = StrAToI(StrToken(NULL));
			EvtAddEventToQueue(&e);
			break;
			
		case pbMenu:
			e.eType = menuEvent;
			e.data.menu.itemID = LookupResourceID(StrToken(NULL));
			EvtAddUniqueEventToQueue(&e,0,true);
			break;
			
		case pbKey:
			e.eType = keyDownEvent;
			e.data.keyDown.chr = StrAToI(StrToken(NULL));
			e.data.keyDown.modifiers = StrAToI(StrToken(NULL));
			e.data.keyDown.keyCode = StrAToI(StrToken(NULL));
			EvtAddEventToQueue(&e);
			break;
			
		case pbAlert:
			ModErrThrow();
			break;
			
		case pbCheckScreen:
			SaveScreen(fireEventAtForm, fireEventAtTime);
			break;
			
		default:
			ModErrThrow();
			break;
		}

		eventIsLoaded = false;
		
	}
	
}


#endif

/*
 *  function: CheckForLongPress
 *
 */

static WChar CheckForLongPress(WChar key) {

	WChar newcode;
	UInt32 hardKeyCode;
	UInt32 startTime = PFGetTicks();

	switch (key) {

	case vchrF1: newcode = vchrShiftF1; hardKeyCode = keyBitHard1; break;
	case vchrF2: newcode = vchrShiftF2; hardKeyCode = keyBitHard2; break;
	case vchrF3: newcode = vchrShiftF3; hardKeyCode = keyBitHard3; break;
	case vchrF4: newcode = vchrShiftF4; hardKeyCode = keyBitHard4; break;

	default:
		return key;

	}

	while (HardKeyDown(hardKeyCode) && !PFTimerHasExpired(startTime, SysTicksPerSecond()));

	EvtFlushKeyQueue();

	if (PFTimerHasExpired(startTime, SysTicksPerSecond())) {

		while (HardKeyDown(hardKeyCode));
		EvtFlushKeyQueue();

		return newcode;

	} 

	return key;

}

/*******************************************************************************
 *
 * Sound Functions
 *
 * The following block of code has been lifted from a Palm SDK demonstration
 * of sound playback. 
 *
 * Now heavily modified to adapt it for queued sound playback.
 *
 */

#define MAX_SND_QUEUE_DEPTH 16
typedef struct oeCallbackDataType {
	FileRef      fileRef[MAX_SND_QUEUE_DEPTH];
	Int16        numFiles;
	SndStreamRef streamRef;
	Boolean      finished;
	UInt16       frameWidth; // number of bytes in each sample frame

	/*
	 * this (in practice) is a pointer to activePlayback, which allows the
	 * Cleanup routine to set that variable to NULL - thereby signalling to the
	 * next invocation of PlayRawVFSFile that a new stream needs to be created.
	 *
	 * This round-about-ness is necessary because Cleanup doesn't have access
	 * to any of our program's global variables.
	 *
	 */

	struct oeCallbackDataType **mothership;

} oeCallbackDataType;


static oeCallbackDataType *activePlayback = NULL; // communication semaphore

/*
 * CleanUp
 *
 * Called by PalmOS system when sound is finished playing, in response 
 * to SysNotifyBroadcastDeferred
 *
 */

static Err Cleanup(SysNotifyParamType* notifyParamsP) {

	oeCallbackDataType *tempP = (oeCallbackDataType*) *((oeCallbackDataType**)notifyParamsP->notifyDetailsP);
	oeCallbackDataType **ptr = tempP->mothership;
	
	SndStreamDelete(tempP->streamRef);
	MemPtrFree(tempP);

	*ptr = NULL;
	
	return errNone;

}


/*
 * readCallbackFunc
 *
 * Called by PalmOS when the sound stream needs feeding.
 *
 */

#define REQUESTED_BUFFER_SIZE 4096
static Err readCallbackFunc(void* UserDataP, SndStreamRef stream, void* bufferP, UInt32 frameCount) {
	Err                result;
	Err                result2;
	UInt32             bytesRead;
	SysNotifyParamType notifyParams;
	oeCallbackDataType *cbd = (oeCallbackDataType*)UserDataP; // callback data
	FileRef            fr = cbd->fileRef[0];
	UInt32 requestBytes = frameCount * ((oeCallbackDataType*)(UserDataP))->frameWidth;

	ErrNonFatalDisplayIf(
		(((oeCallbackDataType*)UserDataP)->finished == true),
		"Entered callback after stopping.");

	result = VFSFileRead(fr, requestBytes, bufferP, &bytesRead);
	if (result == vfsErrFileEOF && bytesRead == 0) {

		Int16 j;

		/*
		 * end of the current file, load next file (if available)
		 *
		 */

		VFSFileClose(fr);
		cbd->numFiles--;
		if (!cbd->numFiles) {

			/*
			 * stop the sound stream, broadcast a notification (you cannot call
			 * SndStreamDelete from within this handler function) which will
			 * get picked up by CleanUp (above).
			 *
			 */

			result2 = SndStreamStop(stream);
			((oeCallbackDataType*)UserDataP)->finished = true;

			MemSet(&notifyParams, sizeof(SysNotifyParamType), 0);
			notifyParams.notifyType     = FMCreatorID;
			notifyParams.broadcaster    = FMCreatorID;
			notifyParams.notifyDetailsP = &UserDataP;

			SysNotifyBroadcastDeferred(&notifyParams, sizeof(void*));
			
			return errNone;
			ErrNonFatalDisplayIf( (result2 != errNone), "Can't close sound stream.");

		}

		for (j=0;j < cbd->numFiles; j++) cbd->fileRef[j] = cbd->fileRef[j+1];

		/*
		 * read data from next file - there had better be some data in here or it
		 * won't fail gracefully
		 *
		 */

		fr=cbd->fileRef[0];
		result = VFSFileRead(fr, requestBytes, bufferP, &bytesRead);

	}

	if (bytesRead < requestBytes) {

		MemSet(bufferP+bytesRead, requestBytes-bytesRead, 0);

	}

	ErrNonFatalDisplayIf( (result != errNone), "Can't read from file.");
	return errNone;

}

static void PlayRawVFSFile(const char* filenameP, UInt32 sampleRate, SndStreamWidth sampleWidth, Int32 volume) {

	Err     result;
	UInt16  volRef;
	UInt32  volIterator = vfsIteratorStart;
	oeCallbackDataType* callbackDataP;
	Boolean createStream;
	FileRef soundFile;

	/*
	 * check that the file exists
	 *
	 */

	result = VFSVolumeEnumerate(&volRef, &volIterator);
	if (result != errNone) return;
	result = VFSFileOpen(volRef, filenameP, vfsModeRead, &soundFile);
	if (result != errNone) return;

	/*
	 * check for existing playback, add to it if necessary
	 *
	 */

	if (activePlayback) {
		
		callbackDataP = activePlayback;
		createStream = false;

	} else {

		/*
		 * new playback
		 *
		 */

		createStream = true;

		callbackDataP = MemPtrNew(sizeof(oeCallbackDataType));
		activePlayback=callbackDataP;
		callbackDataP->mothership = &activePlayback;
		callbackDataP->numFiles = 0;


	}

	/*
	 * add file to sound stream
	 *
	 */

	if (callbackDataP->numFiles == MAX_SND_QUEUE_DEPTH) {

		VFSFileClose(soundFile);
		return;

	}

	callbackDataP->fileRef[callbackDataP->numFiles++] = soundFile;
	if (createStream) {

		result = SndStreamCreate( &(callbackDataP->streamRef), sndOutput, sampleRate, sndInt16Little,
			sampleWidth, readCallbackFunc, callbackDataP, REQUESTED_BUFFER_SIZE, false);

		ErrNonFatalDisplayIf( (result != errNone), "Can't create sound stream.");

		callbackDataP->finished = false;

		if (sampleWidth == sndMono)
			callbackDataP->frameWidth = 2;
		else
			callbackDataP->frameWidth = 4; // twice the samples for stereo

		SndStreamSetVolume(callbackDataP->streamRef, volume);
		result = SndStreamStart(callbackDataP->streamRef);
		ErrNonFatalDisplayIf( (result != errNone), "Can't start sound stream.");

	}

}

/*
 * End of lifted code
 *
 *******************************************************************************/



/*******************************************************************************
 *
 * Public functions
 *
 */


/*
 * PFInit
 *
 */

void PFInit(void) {

	Err     err;
	UInt32  version;

	appLaunchTime = TimGetTicks();

	err = FtrGet(sysFileCSoundMgr, sndFtrIDVersion, &version);
	if(!err && version == sndMgrVersionNum) {

		UInt16  cardNo;
		LocalID dbID;

		pfHasSound = true;

		/*
		 * Register the sound manager notification receiver
		 *
		 */

		SysCurAppDatabase(&cardNo, &dbID);
		err = SysNotifyRegister(cardNo, dbID, FMCreatorID, Cleanup, sysNotifyNormalPriority, NULL);
		ErrNonFatalDisplayIf( (result != errNone), "can't register Cleanup");

	}

#ifdef TEST_FRAMEWORK
	playbackEventFile = PFOpenFile("/FMTest/testscript.txt", pfFileReadOnly);
	if (playbackEventFile) {
		
		playbackLogFile = PFOpenFile("/FMTest/logfile.txt", pfFileTruncate);
		
	} else {

		char filename[32];
		LOGSTR("Opening event file for recording");
		StrPrintF(filename, "/FMTest/testscript.txt.record", PFGetSeconds());
		recordEventFile = PFOpenFile(filename, pfFileTruncate);

	}
#endif

}

/*
 * PFDeinit
 *
 */

void PFDeinit(void) {

	UInt16  cardNo;
	LocalID dbID;
	Err     result;

	if (pfHasSound) {

		/*
		 * Deregister the sound manager notification receiver...
		 *
		 */

		SysCurAppDatabase(&cardNo, &dbID);
		result = SysNotifyUnregister(cardNo, dbID, FMCreatorID, sysNotifyNormalPriority);
		ErrNonFatalDisplayIf( (result != errNone), "can't de-register Cleanup");

	}

}

/*
 * Exit stub
 *
 */

void /*@noreturn@*/ PFErrExit(UInt32 ev) {

	ErrThrow(ev);

}


/*
 * PFPlaySound
 *
 */

void PFPlaySound(const char *filename, Int32 volume) {

	if (pfHasSound) {

		PlayRawVFSFile(filename, 44100, sndMono, volume);

	}

}

/*******************************************************************************
 * 
 * Memory Functions
 *
 */

/*
 * PFMalloc
 *
 */

void *PFMalloc1(UInt32 size) {

	if (size < 65535) {

		return MemPtrNew(size);

	} else {

		return MemGluePtrNew(size);

	}

	
}

void *PFMallocLogged(const char *file,UInt32 line,UInt32 size) {
	
	void *newPtr = PFMalloc1(size);
	char logline[128];
	
	StrPrintF(logline,"%ld (%ld bytes) allocated by %s:%ld", (UInt32)newPtr, size, file, line);
	LOGSTR(logline);
	return newPtr;
}

UInt32 PFMallocSize(void *ptr) {
	
	return MemPtrSize(ptr);
	
}

/*
 * PFMallocResize
 * 
 */

void PFMallocResize(void *oldPtr, UInt32 newSize) {
	
	MemPtrResize(oldPtr, newSize);
	
}

/*
 * PFMemFree1
 *
 */

void PFMemFree1(/*@only@*/ /*@out@*/ /*@null@*/ void *ptr) {

	MemPtrFree(ptr);

}

/*******************************************************************************
 * 
 * File Functions
 *
 */

/*
 * PFOpenFile
 *
 * Returns file reference, or NULL if file could not be opened
 *
 */

PFFileRef PFOpenFile(const char *filename, PFFileMode mode) {

	Err err;

	if (filename[0] == '/') {

		/*
		 * external file
		 *
		 */

		UInt16 openMode;
		UInt32 vfsMgrVersion;
		FileRef ref;

		switch(mode) {

		case pfFileReadOnly:
			openMode = vfsModeRead;
			break;

		case pfFileReadWrite:
			openMode = vfsModeReadWrite | vfsModeCreate;
			break;

		case pfFileTruncate:
			openMode = vfsModeReadWrite | vfsModeTruncate | vfsModeCreate;
			break;

		default:
			PfErrThrowIf(1);
			openMode = vfsModeRead;
			break;

		}

		err=FtrGet(sysFileCVFSMgr, vfsFtrIDVersion, &vfsMgrVersion);
		if (!err) {

			UInt16 volRefNum;
			UInt32 volIterator = vfsIteratorStart;

			while (volIterator != vfsIteratorStop) {

				err = VFSVolumeEnumerate(&volRefNum, &volIterator);
				if (err == errNone) {

					err = VFSFileOpen(volRefNum, filename, openMode , &ref);

					if (err == errNone) {

						PFFileRef pf = MemPtrNew(sizeof(struct PFFileRefStructType));

						pf->external = true;
						pf->extRef   = ref;
						return pf;

					}

				}

			}

		}

	} else {

		/*
		 * internal file
		 *
		 */

		UInt32 openMode;
		FileHand ref;

		switch(mode) {

		case pfFileReadOnly:
			openMode = fileModeReadOnly;
			break;

		case pfFileReadWrite:
			openMode = fileModeUpdate;
			break;

		case pfFileTruncate:
			openMode = fileModeReadWrite;
			break;

		default:
			PfErrThrowIf(1);
			openMode = fileModeReadOnly;
			break;

		}

		ref = FileOpen(0, filename, 0, 0, openMode, &err);

		if (err == errNone) {

			PFFileRef pf = MemPtrNew(sizeof(struct PFFileRefStructType));

			pf->external = false;
			pf->intRef   = ref;
			return pf;

		}

	}

	return NULL;

}

/*
 * PFCloseFile
 *
 */

void PFCloseFile(PFFileRef f) {

	if (f->external) {

		VFSFileClose(f->extRef);

	} else {

		FileClose(f->intRef);

	}

	MemPtrFree((void*)f);

}

/*
 * PFFileSize
 *
 */

UInt32 PFFileSize(PFFileRef f) {

	UInt32 size;

	if (f->external) {

		VFSFileSize(f->extRef, &size);

	} else {
		
		Err err;
		Int32 size1;
		
		(void)FileTell(f->intRef, &size1, &err);

		PfErrThrowIf(err != errNone);

		size = (UInt32)size1;

	}

	return  size;

}

// TODO - Add PFWriteFileString

/*
 * PFWriteFile
 *
 * Returns number of bytes written
 *
 */

Int32 PFWriteFile(PFFileRef f, const void *data, UInt32 size) {

	UInt32 written;

	if (f->external) {

		VFSFileWrite(f->extRef, size, data, &written);

	} else {

		Err err;

		written = (UInt32) FileWrite(f->intRef, data, 1, size, &err);

	}

	return written;

}

/*
 * PFReadFile
 *
 * Returns number of bytes read, if less than maxSize then EOF has been
 * encountered
 * 
 */

Int32 PFReadFile(PFFileRef f, void *dst, UInt32 maxSize) {

	UInt32 read;
		
	if (f->external) {

		VFSFileRead(f->extRef, maxSize, dst, &read);

	} else {

		Err err;

		read = (UInt32)FileRead(f->intRef, dst, 1, maxSize, &err);

	}

	return read;

}

/*
 * PFSeekFile
 *
 */

void PFSeekFile(PFFileRef f, PFFileSeek mode, Int32 whereTo) {

	if (f->external) {

		FileOrigin o;

		switch (mode) {

		case pfSeekStart:
			o = vfsOriginBeginning;
			break;

		case pfSeekEnd:
			o = vfsOriginEnd;
			break;

		case pfSeekCurrent:
			o = vfsOriginCurrent;
			break;

		default:
			ModErrThrow();
			break;
			
		}

		VFSFileSeek(f->extRef, o, whereTo);

	} else {

		FileOriginEnum o;

		switch (mode) {

		case pfSeekStart:
			o = fileOriginBeginning;
			break;

		case pfSeekEnd:
			o = fileOriginEnd;
			break;

		case pfSeekCurrent:
			o = fileOriginCurrent;
			break;
			
		default:
			ModErrThrow();
			break;

		}

		FileSeek(f->intRef, whereTo, o);

	}
		
}

/*
 * PFReadLine
 *
 */

Boolean PFReadLine(PFFileRef f, char *result) {

	Int16 len = 0;
	UInt32 read;
	Boolean eof = false;
	char buf[256];
	Int16 ptr = 0;
	Int16 bufSize;

	*result = 0;

	read = PFReadFile(f, buf, sizeof(buf));

	bufSize = (Int16)read;


	/*
	 * skip past any \r or \n, detect the end of file if found
	 *
	 */

	while (ptr < bufSize && (buf[ptr] == 10 || buf[ptr] == 13)) ptr++;
	
	if (ptr == bufSize) {

		*result = 0;
		return false;

	}

	while (ptr < bufSize && buf[ptr] != 10 && buf[ptr] != 13) {
	
		result[len++] = buf[ptr++];

	} 
	
	if (ptr == bufSize) {

		eof = true;

	} else {

		PFSeekFile(f, pfSeekCurrent, ptr - bufSize);

	}

	result[len] = 0;
	return !eof;

}

/*
 * PFTruncateFile
 *
 */

void PFTruncateFile(PFFileRef f, UInt32 newLength) {

	PfErrThrowIf(f->external);

	if (f->external) {

	} else {

		FileTruncate(f->intRef, (Int32)newLength);

	}

}

/*******************************************************************************
 * 
 * Serial/Bluetooth I/O Functions
 * 
 */

#define MAX_SERIAL_BUFFER_SIZE 4096
static char serialBuffer[MAX_SERIAL_BUFFER_SIZE];

static Boolean portOpen = false;

PFPortRefType PFPortOpen(PFPortType port, UInt32 baudRate) {
	
	UInt32 	serPortFlags = srmSettingsFlagStopBits1 | 
			srmSettingsFlagRTSAutoM | srmSettingsFlagCTSAutoM |
			srmSettingsFlagBitsPerChar8;
	
	UInt16 flagsLen = sizeof(serPortFlags);
	UInt32 portID;
	UInt16 openPortID;
	Err error;
	
	ModErrThrowIf(portOpen);
	
	switch (port) {

	case portCradle:
		portID = serPortCradlePort;
		break;

	case portSerial:
		portID = serPortCradleRS232Port;
		break;

	case portUSB:
		portID = serPortCradleUSBPort;
		break;

	case portClie:
		portID = 'GPSd';
		break;

	default:
		return -1;

	}		   
	error = SrmOpen(portID,baudRate,&openPortID);
	if (error) return error;

	SrmControl(openPortID, srmCtlSetFlags, &serPortFlags, &flagsLen);
	SrmSetReceiveBuffer(openPortID, serialBuffer, sizeof(serialBuffer));

	portOpen = true;
	return (PFPortRefType)openPortID;

}

/*
 * function : ChooseBluetoothDevice
 *
 * Opens the choose-bluetooth device dialog to allow user
 * to select a GPS
 *
 */

static Boolean ChooseBluetoothDevice(BtLibDeviceAddressType *btID) PLATFORM_SECTION;
static Boolean ChooseBluetoothDevice(BtLibDeviceAddressType *btID) {

	UInt16 btLibRef;
	Boolean result;
	Err error;

	SysLibFind(btLibName, &btLibRef);

	error = BtLibOpen(btLibRef, false);

	if (error) {

		return false;

	}

	result = BtLibDiscoverSingleDevice(btLibRef, "Choose GPS", NULL, 0, btID, false,
			false) == btLibErrNoError; 

	SysLibClose(btLibRef);

	return result;

}

PFPortRefType PFPortOpenBluetooth(PFBluetoothAddressType *btaddr, Boolean scanIfFailToConnect) {
	
	UInt32 featureVersion;
	Err error;
	UInt16 openPortID;
	
	if (FtrGet(btLibFeatureCreator, btLibFeatureVersion, &featureVersion) != errNone) {

		return ERR_GPS_NO_BLUETOOTH;

	}

	do {

		BtLibDeviceAddressType *btID = (BtLibDeviceAddressType*) btaddr;
		SrmOpenConfigType config;
		BtVdOpenParams btParams;
		BtLibSdpUuidType   sppUuid;
		Int16 j;

		config.function = 0;
		config.drvrDataP = &btParams;
		config.drvrDataSize = sizeof(BtVdOpenParams);

		// Set up a SerialPort service class UUID
		//
		// (This is not necessary, but I've left it here to remind
		// myself how to do it!! You also need to change uuidList.len to 1
		//
		//MemSet( &sppUuid, sizeof(sppUuid), 0 );
		//sppUuid.size = btLibUuidSize16;
		//sppUuid.UUID[0] = 0x11;
		//sppUuid.UUID[1] = 0x01;

		btParams.role = btVdClient;

		for (j=0;j<btLibDeviceAddressSize;j++) 
			btParams.u.client.remoteDevAddr.address[j] = btID->address[j];

		btParams.u.client.method = btVdUseUuidList;
		btParams.u.client.u.uuidList.len = 0;
		btParams.u.client.u.uuidList.tab = &sppUuid;
		btParams.authenticate = false;
		btParams.encrypt = false;

		error = SrmExtOpen(sysFileCVirtRfComm, &config, sizeof(config), &openPortID);

		if (error) {
			
			//for (j=0;j<btLibDeviceAddressSize;j++) 
				//btID->address[j] = 0;

			if (!ChooseBluetoothDevice(btID)) {

				return ERR_GPS_NO_BLUETOOTH;

			}

		}

	} while (error) ;

	if (error) return error;

	SrmSetReceiveBuffer(openPortID, serialBuffer, sizeof(serialBuffer));
	portOpen = true;
	
	return (PFPortRefType)openPortID;

}

void PFPortClose(PFPortRefType port) {
	
	SrmSetReceiveBuffer((UInt16)port, NULL, 0);
	SrmClose((UInt16)port);
	portOpen = false;
	
}

UInt32 PFPortRead(PFPortRefType port, void *buffer, UInt32 maxLength) {

	Err error;
	UInt16 portID = (UInt16)port;
	UInt32 numAvailable;
	
	error = SrmReceiveCheck(portID, &numAvailable);
	if (error) {
		
		SrmReceiveFlush(portID,0);
		return 0;
		
	}

	if (maxLength > numAvailable) maxLength = numAvailable;
	
	return SrmReceive(portID, buffer, maxLength, 5 /*ticks*/, &error);

}


/*******************************************************************************
 * 
 * Database functions
 *
 */

/*
 * DBOpen
 *
 */

DmOpenRef DBOpen(const char *dbname, Boolean readOnly, Boolean createIfMissing) {

	DmOpenRef ref;
	UInt16 attr;
	LocalID lid = DmFindDatabase(0, dbname);

	if (!lid) {

		if (createIfMissing) {

			Err error; 

			error = DmCreateDatabase(0, dbname, FMCreatorID, FMDBType, false);
			PfErrThrowIf(error);

			lid = DmFindDatabase(0, dbname);

			DmDatabaseInfo(0,lid,NULL,&attr,NULL,NULL,NULL,NULL,NULL,
					NULL,NULL,NULL,NULL);

			attr |= dmHdrAttrBackup;

			DmSetDatabaseInfo(0,lid,NULL,&attr,NULL,NULL,NULL,NULL,
				NULL,NULL,NULL,NULL,NULL);

			return DBOpen(dbname, readOnly, false);

		} else {

			return NULL;

		}

	}

	ref = DmOpenDatabase(0, lid, readOnly ? dmModeReadOnly:dmModeReadWrite);
	if (!ref) return NULL;

	return ref;

}

/*
 * DBGetInfo
 *
 * Returns pointer to database information block. Pointer must be
 * disposed of by caller
 *
 */

MemPtr DBGetInfo(DmOpenRef ref) {

	MemPtr info;
	MemPtr newInfo;
	LocalID lid = DmGetAppInfoID(ref);

	if (lid == 0) return  NULL;

	info = MemLocalIDToLockedPtr(lid, 0);

	PfErrThrowIf(!info);

	newInfo = MemPtrNew(MemPtrSize(info));
	MemMove(newInfo, info, MemPtrSize(info));

	MemPtrUnlock(info);
	return newInfo;

}

UInt16 DBRecordCreate(DmOpenRef ref, UInt16 atIndex, void *data, UInt16 recordSize) {
	
	UInt16 newIndex = atIndex;
	MemHandle recHandle = DmNewRecord(ref, &newIndex, recordSize);
	
	ModErrThrowIf(!recordSize);
	ModErrThrowIf(!recHandle);

	if (data) {
		
		void *ptr = MemHandleLock(recHandle);
		DmWrite(ptr, 0, data, recordSize);
		MemHandleUnlock(recHandle);
		
	}
	
	DmReleaseRecord(ref, newIndex, true);
	
	return newIndex;

}

void DBRecordDelete(DmOpenRef ref, UInt16 recnum) {
	
	DmRemoveRecord(ref, recnum);
	
}

void *DBRecordGet(DmOpenRef ref, UInt16 recnum, Boolean copy) {

	MemHandle h = DmQueryRecord(ref, recnum);
	ModErrThrowIf(!h);

	if (copy) {

		void *ptr;
		
		ptr = PFMalloc(MemHandleSize(h));
		ModErrThrowIf(!ptr);
		
		MemMove(ptr, MemHandleLock(h), MemHandleSize(h));
		MemHandleUnlock(h);
		
		return ptr;
		
	} else {

		return (void*) MemHandleLock(h);
		
	}
	
}

void DBRecordUpdate(DmOpenRef ref, UInt16 recnum, UInt16 offset, void *data, UInt16 datasize) {
	
	MemHandle handle = DmGetRecord(ref, recnum);
	void *ptr;
	
	ModErrThrowIf(!handle);
	
	ptr = MemHandleLock(handle);

	DmWrite(ptr, (UInt32)offset, data, (UInt32)datasize);
	
	MemHandleUnlock(handle);
	
	DmReleaseRecord(ref, recnum, true);
	
}

Boolean DBRecordResize(DmOpenRef ref, UInt16 recnum, UInt16 newsize) {
	
	MemHandle recHandle = DmGetRecord(ref,recnum);
	ModErrThrowIf(!recHandle);
	
	recHandle = DmResizeRecord(ref, recnum, newsize);
	if (!recHandle) return false;
	
	DmReleaseRecord(ref, recnum, true);

	return true;
	
}

UInt16 DBRecordGetIndexByPalmID(DmOpenRef ref, UInt32 id) {
	
	UInt16 index;
	
	if (!DmFindRecordByID(ref, id, &index)) {
		
		return index;
		
	} else {
		
		return DBRecordNotFound;
		
	}
}


void DBRecordFree(void *rec) {
	
//	MemPtrUnlock((MemPtr)rec);
	MemHandle h = MemPtrRecoverHandle((MemPtr)rec);

	MemHandleUnlock(h);
	if (!MemHandleDataStorage(h)) {
		
		MemHandleFree(h);

	}

}

UInt16 DBRecordGetSize(DmOpenRef db, UInt16 recnum) {
	
	return MemHandleSize(DmQueryRecord(db, recnum));
	
}



/*
 * PFCloseDatabase
 *
 *
 */

void DBClose(DmOpenRef ref) {

	DmCloseDatabase(ref);

}

/*
 * DBDelete
 *
 */

void DBDelete(const char *dbname) {

	LocalID lid = DmFindDatabase(0, dbname);

	if (lid) DmDeleteDatabase(0, lid);

}

/*
 * DBGetDates
 *
 */

void DBGetDates(DmOpenRef db, UInt32 *createDate, UInt32 *modDate) {

	LocalID lid;
	UInt16 card;

	DmOpenDatabaseInfo(db, &lid, NULL,NULL,&card,NULL);

	DmDatabaseInfo(card,lid,NULL,NULL,NULL, createDate,modDate, 
			NULL,NULL, NULL,NULL,NULL,NULL);

}

/*
 * DBFRecordGet
 *
 * Read record from file-based PDB
 *
 */

void *DBFRecordGet(PFFileRef f, UInt16 recNum, Boolean copy) {

	MemHandle h;

	PfErrThrowIf(!f->external);
	VFSFileDBGetRecord(f->extRef, recNum, &h, NULL, NULL);

	ModErrThrowIf(!h);

	if (copy) {

		void *ptr;

		ptr = PFMalloc(MemHandleSize(h));
		MemMove(ptr, MemHandleLock(h), MemHandleSize(h));
		MemHandleUnlock(h);
		MemHandleFree(h);
		
		return ptr;

	} else {

		return MemHandleLock(h);
		
	}

}

/*
 * DBFGetNumRecords
 *
 * Returns number of records in file-based PDB
 *
 */

UInt16 DBFGetNumRecords(PFFileRef f) {

	UInt16 numRecords;

	PfErrThrowIf(!f->external);

	VFSFileDBInfo(f->extRef,NULL,NULL, NULL,NULL,NULL,NULL,
			NULL,NULL,NULL,NULL,NULL, &numRecords);

	return numRecords;

}

/*
 * DBFGetDates
 *
 * Returns file dates for the specified file
 *
 */

void DBFGetDates(PFFileRef f, UInt32 *createDate, UInt32 *modDate) {

	if (f->external) {

		VFSFileDBInfo(f->extRef, NULL, NULL, NULL, 
			createDate, modDate, NULL, NULL, NULL, 
			NULL, NULL, NULL, NULL);

	} else {


	}

}

/*
 * DBFGetInfo
 *
 * Returns pointer to database information block, for a file-based database.
 * Pointer must be disposed of by caller
 *
 */

MemPtr DBFGetInfo(PFFileRef f) {

	return NULL;

}

/*****************************************************************************
 *
 * GUI Functions
 *
 */

/*
 * Forms
 * 
 */

UInt16 GUIAlertShow(UInt16 alertID) {
	
#ifdef TEST_FRAMEWORK
	
	UInt16 result;
	
	if (recordEventFile) {
		
		result = FrmAlert(alertID);
		RecordAlertEvent(alertID, result);
		
	} else if (playbackEventFile) {
		
		// special case- initialise when PlayBackEvent hasn't yet been called, but
		// we need to show an alert anyway...
		
		if (!playbackAlertName) PlayBackEvent(playbackEventFile);
		
		if (playbackAlertName && StrCompare(playbackAlertName, LookupResourceIdentifier(alertID)) == 0) {
			
			LOGTAG("Alert matched");
			eventIsLoaded = false;
			return playbackAlertResult;
			
		} else {
			
			ModErrThrow();
			
		}
		
	}
	return result;
	
#else 
	
	return FrmAlert(alertID);
	
#endif
	
}

UInt16 GUICustomAlertShow(UInt16 alertID, const char *f1, const char *f2, const char *f3) {

#ifdef TEST_FRAMEWORK
	
	UInt16 result = FrmCustomAlert(alertID, f1, f2, f3);
	
	RecordAlertEvent(alertID, result);
	return result;
	
#else 
	
	return FrmCustomAlert(alertID, f1, f2, f3);

#endif
	
}

void GUIFormLoad(UInt16 formID, Boolean (*evtHandler)(EventPtr)) {

	FormPtr form = FrmInitForm(formID);
	
	ModErrThrowIf(!form);
	FrmSetActiveForm(form);
	
	if (evtHandler) {
		
		FrmSetEventHandler(form, evtHandler);

	}

	CollapseSetState(form, collapseStateUser);

}

void GUIFormGoto(UInt16 formID) {
	
	FrmGotoForm(formID);
	
}

void GUIFormPopup(UInt16 formID) {
	
	FrmPopupForm(formID);
	
}

void GUIFormReturn(void) {
	
	FrmReturnToForm(0);
	
}

UInt16 GUIFormGetActiveID(void) {
	
	return FrmGetActiveFormID();
	
}



void  GUIFormDraw(void) {

	FrmDrawForm(FrmGetActiveForm());

}

void GUIFormSetTitle(const char *newTitle) {

	FrmSetTitle(FrmGetActiveForm(), (char*)newTitle);
	
}

static Int16 formResizeXdelta, formResizeYdelta;

Boolean GUIFormResize(Boolean isModal, Boolean moveForm) {
	
	FormPtr form = FrmGetActiveForm();
	
	return CollapseResizeForm(form, isModal, moveForm, &formResizeXdelta, &formResizeYdelta); 
	
}

void GUIFormResizeGetDeltas(Int16 *xdelta, Int16 *ydelta) {
	
	*xdelta = formResizeXdelta*2;
	*ydelta = formResizeYdelta*2;
	
}

void GUIObjectMove(UInt16 controlID, Int16 xdelta, Int16 ydelta) {
	
	PFScreenRectType r;
	
	GUIObjectGetBounds(controlID, &r);
	
	r.x1 += xdelta;
	r.y1 += ydelta;
	r.x2 += xdelta;
	r.y2 += ydelta;
	
	GUIObjectSetBounds(controlID, &r);
	
}

void GUIObjectResize(UInt16 controlID, Int16 xdelta, Int16 ydelta) {
	
	PFScreenRectType r;
	
	GUIObjectGetBounds(controlID, &r);
	
	r.x2 += xdelta;
	r.y2 += ydelta;
	
	GUIObjectSetBounds(controlID, &r);
	
}

/*
 * Objects
 * 
 */

void GUIObjectDraw(UInt16 controlID) {

	FormPtr form = FrmGetActiveForm();
	
	CtlDrawControl(FrmGetObjectPtr(form, FrmGetObjectIndex(form, controlID)));

}

void GUIFocusSet(UInt16 controlID) {

	FormPtr form = FrmGetActiveForm();

	FrmSetFocus(form, FrmGetObjectIndex(form, controlID));

}

void GUIObjectHide(UInt16 controlID) {

	FormPtr form = FrmGetActiveForm();

	FrmHideObject(form, FrmGetObjectIndex(form, controlID));

}

void GUIObjectShow(UInt16 controlID) {
	
	FormPtr form = FrmGetActiveForm();

	FrmShowObject(form, FrmGetObjectIndex(form, controlID));

}

void GUIObjectGetBounds(UInt16 controlID, PFScreenRectType *r) {

	RectangleType r2;
	
	FormPtr form = FrmGetActiveForm();
	
	FrmGetObjectBounds(form, FrmGetObjectIndex(form, controlID), &r2);
	r->x1 = 2*r2.topLeft.x; 
	r->y1 = 2*r2.topLeft.y;
	r->x2 = 2*(r2.topLeft.x + r2.extent.x);
	r->y2 = 2*(r2.topLeft.y + r2.extent.y);

}

void GUIObjectSetBounds(UInt16 controlID, PFScreenRectType *r) {
	
	RectangleType r2;
	FormPtr form = FrmGetActiveForm();

	r2.topLeft.x = r->x1/2;
	r2.topLeft.y = r->y1/2;
	r2.extent.x = PFGetRectangleWidth(*r)/2;
	r2.extent.y = PFGetRectangleHeight(*r)/2;
	FrmSetObjectBounds(form, FrmGetObjectIndex(form, controlID), &r2);
	
}

Int16 GUIObjectGetValue(UInt16 controlID) {

	FormPtr form = FrmGetActiveForm();

	return CtlGetValue(GetObjectPtr2(form, controlID));

}

void GUIObjectSetValue(UInt16 controlID, Int16 value) {

	FormPtr form = FrmGetActiveForm();

	CtlSetValue(GetObjectPtr2(form, controlID), value);

}

UInt16 GUIObjectGroupGetValue(UInt16 groupID) {
	
	FormPtr form = FrmGetActiveForm();
	
	return  FrmGetObjectId(form, FrmGetControlGroupSelection(form, groupID));

}

void GUIObjectGroupSetValue(UInt16 groupID, UInt16 controlID) {
	
	FormPtr form = FrmGetActiveForm();
	
	FrmSetControlGroupSelection(form, groupID, controlID);

}

void GUIObjectSetLabelPtr(UInt16 controlID, const char *label) {

	CtlSetLabel(GetObjectPtr2(FrmGetActiveForm(), controlID), label);

}


void GUIObjectSetLabel(UInt16 controlID, const char *label) {

	ControlPtr ctl = GetObjectPtr2(FrmGetActiveForm(), controlID);
	char *ctlLabel;

	ctlLabel = (char*) CtlGetLabel(ctl);
	//StrNCopy(ctlLabel,label, StrLen(ctlLabel));
	StrCopy(ctlLabel,label);
	CtlSetLabel(ctl, ctlLabel);

}

void GUIObjectSetBitmap(UInt16 controlID, UInt16 bitmapID) {

	FormPtr form = FrmGetActiveForm();

	CtlSetGraphics(GetObjectPtr2(form, controlID), bitmapID, NULL);

}

static struct {
	
	UInt16 firstControlID, lastControlID;
	UInt16 groupID;
	
} controlGroupMapping[] = {
		
		{ StopPushButton, SimPushButton, GPSPushButtonGroup },
		{ NmPushButton, CpPushButton, UnitsPushButtonGroup },
		{ FPLegPushButton, FPCumulativePushButton, FPLegPushButtonGroup },
		{ FPETEPushButton, FPETAPushButton, FPTimePushButtonGroup },
		{ DvAirfieldPushbutton, DvAnyPushbutton, DvPushbuttonGroup },
		{ SCFCoPilotButton, SCFFMButton, SCFFlightDBGroup },
		{ TruePushButton, MagneticPushButton, HeadingsPushButtonGroup },
		{ MpGrassPushbutton, MpBothPushbutton, MpSurfacePushbuttonGroup },
		{ WPInfoAllPushbutton, WPInfoILSPushbutton, WPInfoPushbuttonGroup },
		{ MdTrackLogOff, MdTrackLog10s, MdTrackLogGroup },
		{ 0,0,0 }
		
};

void GUIObjectTap(UInt16 controlID) {

	ControlType *control = GetObjectPtr2(FrmGetActiveForm(), controlID);
	EventType e;
	Int16 j;
	Boolean isGroupControl = false;
	
	/*
	 * This is awful! PalmOS's CtlHitControl call doesn't change the state
	 * of the GUI objects!!! Hence we have to figure out if control groups
	 * or checkboxes have been tapped on and change the state *ourselves*!!
	 * 
	 * UTTER SHITE!!!
	 * 
	 */
	
	for (j=0; controlGroupMapping[j].firstControlID;j++) {
		
		if (controlID >= controlGroupMapping[j].firstControlID && controlID <= controlGroupMapping[j].lastControlID) {
			
			GUIObjectGroupSetValue(controlGroupMapping[j].groupID, controlID);
			isGroupControl = true;
			break;
			
		}
			
	}
	
	CtlHitControl(control);
	
	if (isGroupControl || CtlGlueGetControlStyle(control) == buttonCtl 
			|| CtlGlueGetControlStyle(control) == selectorTriggerCtl) return;
	
	if (GUIObjectGetValue(controlID)) {
		
		GUIObjectSetValue(controlID, 0);
		
	} else {
		
		GUIObjectSetValue(controlID,1);
		
	}

	
//	e.eType = ctlEnterEvent;
//	e.data.ctlEnter.controlID = controlID;
//	e.data.ctlEnter.pControl = control;
//	EvtAddEventToQueue(&e);
//
//	e.eType = ctlSelectEvent;
//	e.data.ctlSelect.controlID = controlID;
//	e.data.ctlSelect.pControl = control;
//	e.data.ctlSelect.on = true;
//	EvtAddEventToQueue(&e);
	
}

/*
 * Fields
 * 
 */

void GUIFieldSetFocus(UInt16 controlID) {
	
	FormPtr frm = FrmGetActiveForm();
	
	FrmSetFocus(frm, FrmGetObjectIndex(frm, controlID));
	
}

void GUIFieldSetText(UInt16 controlID, const char *text, Boolean redraw)  {

	FieldPtr field = GetObjectPtr2(FrmGetActiveForm(), controlID);
	MemHandle h;

	h = FldGetTextHandle(field);
	if (h) {
		Err err;

		FldSetTextHandle(field, NULL);
		err = MemHandleResize(h, StrLen(text) + 1);
		if (err != errNone) {
			FldSetTextHandle(field,h);
			return;
		}
	}else{
		h = MemHandleNew(StrLen(text)+1);
		if (!h)
			return ;
	}

	StrCopy((char*)MemHandleLock(h),text);
	MemHandleUnlock(h);

	FldSetTextHandle(field,h);
	if (redraw)
		FldDrawField(field);

}

char *GUIFieldGetText(UInt16 fieldID) {

	return FldGetTextPtr(GetObjectPtr2(FrmGetActiveForm(), fieldID));

}

Boolean GUIFieldIsDirty(UInt16 fieldID) {
	
	FormPtr form = FrmGetActiveForm();
	
	return FldDirty(GetObjectPtr2(form, fieldID));
	
}

void GUIFieldClean(UInt16 fieldID) {
	
	FormPtr form = FrmGetActiveForm();
	
	FldSetDirty(GetObjectPtr2(form, fieldID), false);
	
}

void GUIFieldSetSelection(UInt16 fieldID, Int16 from, Int16 to) {

	FldSetSelection(GetObjectPtr2(FrmGetActiveForm(), fieldID), from ,to);

}

/*
 * Scrollbars
 * 
 */

void GUIScrollBarSet(UInt16 scrollID, Int16 val, Int16 min, Int16 max, Int16 pageSize) {
	
	SclSetScrollBar(GetObjectPtr2(FrmGetActiveForm(), scrollID), val, min, max, pageSize);
	
}

/*
 * Lists
 * 
 */

void GUIListSetNumItems(UInt16 listID, Int16 numChoices) {

	LstSetListChoices(GetFormObject(listID), NULL, numChoices);
	
}

void GUIListSetSelection(UInt16 listID, Int16 item) {
	
	LstSetSelection(GetFormObject(listID), item);
	
}

/*
 * List callback function management
 * 
 */

#define MAX_LISTS 5
static UInt16 activeList = 0;

static struct {
	
	UInt16 listID;
	void (*callback)(Int16 itemNum, PFScreenRectType *bounds);
	
} listCallbacks[MAX_LISTS];

static void GUIListDrawWrapper(Int16 itemNum, RectangleType *bounds, char **text) {
	
	PFScreenRectType r;
	
	PFScreenRectangleSetRel(&r, bounds->topLeft.x, bounds->topLeft.y, bounds->extent.x, bounds->extent.y);
	listCallbacks[activeList].callback(itemNum, &r);
	
}

void GUIListSetDrawFunction(UInt16 listID, void (*f)(Int16 itemNum, PFScreenRectType *bounds)) {

	activeList++;
	ModErrThrowIf(activeList == MAX_LISTS);
	
	listCallbacks[activeList].listID = listID;
	listCallbacks[activeList].callback = f;
	
	LstSetDrawFunction(GetFormObject(listID), GUIListDrawWrapper);
	
}

void GUIListClearDrawFunction(UInt16 listID) {
	
	ModErrThrowIf(activeList == 0);
	activeList--;
}


void GUIListMakeItemVisibile(UInt16 listID, Int16 item) {
	
	LstMakeItemVisible(GetFormObject(listID), item);
	
}

void GUIListDraw(UInt16 listID) {
	
	LstDrawList(GetFormObject(listID));
	
}

Int16 GUIListGetSelection(UInt16 listID) {
	
	return LstGetSelection(GetFormObject(listID));
	
}

char *GUIListGetItemText(UInt16 listID, Int16 item) {
	
	return LstGetSelectionText(GetFormObject(listID), item);
	
}

Int16 GUIListGetNumItems(UInt16 listID) {
	
	return LstGetNumberOfItems(GetFormObject(listID));
	
}


Int16 GUIListGetNumVisibleLines(UInt16 listID) {
	
	return LstGetVisibleItems(GetFormObject(listID));
	
}

void GUIListScroll(UInt16 listID, Int16 lines) {
	
	UInt16 dir;
	
	if (lines < 0) {
		
		dir = winUp;
		lines = -lines;
		
	} else {
		
		dir = winDown;
		
	}
	
	LstScrollList(GetFormObject(listID), dir, lines);
	
}


/*
 * Others
 * 
 */

Boolean GUIMenuIsDisplayed(void) {

	if (FrmGetWindowHandle(FrmGetActiveForm()) != WinGetDrawWindow()) {

		if (WinGetActiveWindow()) return true;
		
		return false;

	}

	return false;

}


void GUIBitmapDraw(UInt16 bitmapID, Coord x, Coord y) {
	
        BitmapType *bmp=MemHandleLock(DmGetResource(bitmapRsc, bitmapID));

		WinPaintBitmap(bmp, x, y);
		MemPtrUnlock(bmp);

}

/***/

IndexedColorType GUIGetSystemColour(PFSystemColourType colour) {
	
	UIColorTableEntries item; 
	
	switch (colour) {
	
	case GUIFormFill:
		item = UIFormFill;
		break;
		
	case GUIObjectForeground:
		item = UIObjectForeground;
		break;
		
	case GUIObjectFill:
		item = UIObjectFill;
		break;
		
	case GUIFormFrame:
		item = UIFormFrame;
		break;
		
	default:
		ModErrThrowIf(1);
		break;
		
	}

	return UIColorGetTableEntryIndex(item);
}



/*****************************************************************************
 *
 * Graphics Functions
 *
 */

/*
 * PFSetDrawMode
 * 
 */

PFDrawModeType PFSetDrawMode(PFDrawModeType mode) {
	
	WinDrawOperation oldWinMode;
	WinDrawOperation newWinMode;
	
	newWinMode = mode == blockMode ? winPaint : winOverlay;

	oldWinMode = WinSetDrawMode(newWinMode);
	
	return oldWinMode == winPaint ? blockMode : overlayMode;
	
}

/*
 * PFDrawThickLines
 *
 */

void PFDrawThickLines(LineType *lines, Int16 numLines,
		IndexedColorType inside, IndexedColorType outlineColour, Int16 outlineThickness) {

	LineType *lines2 = MemPtrNew(numLines * sizeof(LineType));

	if (!lines2) return;

	if (outlineThickness != 0) {

		PFSetForeColour(outlineColour);

		/*
		 * thicken inside lines
		 *
		 */

		MemMove(lines2, lines, numLines * sizeof(LineType));

		/*
		 * shift twice, to get inside the main line
		 *
		 */

		LineShift(lines2, numLines, false);
		LineShift(lines2, numLines, false);
		PFDrawLines(numLines, lines2);

		if (outlineThickness > 1) {

			LineShift(lines2, numLines, false);
			PFDrawLines(numLines, lines2);

		}

		/*
		 * outside lines
		 *
		 */

		MemMove(lines2, lines, numLines * sizeof(LineType));

		LineShift(lines2, numLines, true);
		PFDrawLines(numLines, lines2);

		if (outlineThickness > 1) {

			LineShift(lines2, numLines, true);
			PFDrawLines(numLines, lines2);

		}

	}
		
	PFSetForeColour(inside);

	/*
	 * Inside lines...
	 *
	 */

	MemMove(lines2, lines, numLines * sizeof(LineType));
	PFDrawLines(numLines, lines);
	LineShift(lines2, numLines, false);
	PFDrawLines(numLines, lines2);

	MemPtrFree(lines2);

}

/*
 * PFScreenLock
 * 
 */

void PFScreenLock(Boolean copyCurrentScreen) {

	if (copyCurrentScreen) {
		
		WinScreenLock(winLockCopy);
		
	} else {
		
		/*
		 * should use winLockErase instead, but there's a bug with the
		 * ique in that it erases the dynamic input area
		 * 
		 */
		 
		// TODO - ique bug on collapsing the input area, causes loss of active window
		
		WinScreenLock(winLockCopy);
		if (WinGetActiveWindow()) WinEraseWindow();
		
	}
	
}

/*
 * PFSreenUnlock
 * 
 */

void PFScreenUnlock(void) {

	WinScreenUnlock();

}

void PFDrawStatePush(void) {

	WinPushDrawState();
	WinSetCoordinateSystem(kCoordinatesNative);
}


/*
 * function : PFDrawOutlineChars
 *
 */

Coord PFDrawOutlineChars(const char *str, UInt8 align, Coord x1, Coord y1) {

	BitmapType *bm;
	BitmapType *mask;
	Int16       len = StrLen(str);
	Coord width = FntCharsWidth(str, len)+6;
	Coord height= FntLineHeight()+6;
	Coord bmWidth;
	Coord bmHeight;
	UInt16 error;
	WinHandle oldw;
	Int16 x, y;
	UInt8 *bmbits, *maskbits;
	WinDrawOperation dm;
	Coord rowWidth;

	WinHandle drawWin, maskWin;
		
	PFSetBackColour(0);

	drawWin = WinCreateOffscreenWindow(width, height, nativeFormat, &error);
	maskWin = WinCreateOffscreenWindow(width, height, nativeFormat, &error);

	bm = WinGetBitmap(drawWin);
	mask = WinGetBitmap(maskWin);

	BmpGetDimensions(bm, &bmWidth, &bmHeight, &rowWidth);

	oldw = WinSetDrawWindow(drawWin);
	PFDrawChars(str, len, 3,3);
	WinSetDrawWindow(oldw);

	bmbits = BmpGetBits(bm) + rowWidth + 3;
	maskbits=BmpGetBits(mask) + rowWidth + 3;
	
	for (y=3;y<bmHeight-3; y++) {

		for (x=3;x<bmWidth-3;x++) {

			if ( *bmbits != 0 ) {

				*maskbits = 255;
				*(maskbits - 1) = 255;
				*(maskbits - 2) = 255;
				//*(maskbits - 3) = 255;
				//*(maskbits + 3) = 255;
				*(maskbits + 2) = 255;
				*(maskbits + 1) = 255;
				*(maskbits - rowWidth) = 255;
				*(maskbits + rowWidth) = 255;
				*(maskbits - rowWidth*2) = 255;
				*(maskbits + rowWidth*2) = 255;
				//*(maskbits - rowWidth*3) = 255;
				//*(maskbits + rowWidth*3) = 255;
				*(maskbits - rowWidth-1) = 255;
				*(maskbits - rowWidth+1) = 255;
				*(maskbits + rowWidth-1) = 255;
				*(maskbits + rowWidth+1) = 255;
				*(maskbits - rowWidth-2) = 255;
				*(maskbits - rowWidth+2) = 255;
				*(maskbits + rowWidth-2) = 255;
				*(maskbits + rowWidth+2) = 255;
				//*(maskbits - rowWidth*2-1) = 255;
				//*(maskbits + rowWidth*2-1) = 255;
				//*(maskbits - rowWidth*2+1) = 255;
				//*(maskbits + rowWidth*2+1) = 255;

			} 

			maskbits++;
			bmbits++;
		}

		bmbits+=6;
		maskbits+=6;
	}

	switch (align) {
		case ALIGNRIGHT: 
			x1 -= (width-6);
			break;

		case ALIGNCENTRE: 
			x1 -= (width-6)/2;
			break;
	}

	dm = WinSetDrawMode(winMask);
	WinPaintBitmap(mask, x1-3, y1-3);
	WinSetDrawMode(winOverlay);
	WinPaintBitmap(bm, x1-3, y1-3);
	WinSetDrawMode(dm);

	WinDeleteWindow(drawWin,false);
	WinDeleteWindow(maskWin,false);

	return width-3;
	
}

static WinHandle  olOldWindow;
static WinHandle  olWindow;

/*
 * function : PFOutlineBitmapInit
 *
 */

void PFOutlineBitmapInit(Coord width, Coord height) {

	UInt16 error;

	width += 2;
	height += 2;
	olWindow = WinCreateOffscreenWindow(width, height, nativeFormat, &error);
	olOldWindow = WinSetDrawWindow(olWindow);

}

/*
 * function : PFOutlineBitmapDraw
 *
 */


void PFOutlineBitmapDraw(Coord x1, Coord y1) {

	Coord width;
	Coord height;
	UInt16 error;
	Int16 x, y;
	UInt8 *bmbits, *maskbits;
	WinDrawOperation dm;
	UInt16 rowBytes; 
	BitmapType *olBitmap;
	BitmapType *olBitmask;
	WinHandle  olMaskWindow;

	LOGTIMESTART;

	LOGLINE;

	olBitmap = WinGetBitmap(olWindow);
	BmpGetDimensions(olBitmap, &width, &height, &rowBytes);

	olMaskWindow = WinCreateOffscreenWindow(width, height, nativeFormat, &error);
	olBitmask = WinGetBitmap(olMaskWindow);

	bmbits = BmpGetBits(olBitmap) + rowBytes + 2;
	maskbits=BmpGetBits(olBitmask) + rowBytes + 2;
	
	LOGLINE;
	for (y=1;y<height-1; y++) {

		for (x=1;x<rowBytes-1;x++) {

			if ( *bmbits != 0 ) {

				*maskbits = 255;
				*(maskbits - 1) = 255;
				*(maskbits - 2) = 255;
				*(maskbits + 1) = 255;
				*(maskbits + 2) = 255;
				*(maskbits - rowBytes) = 255;
				*(maskbits + rowBytes) = 255;

			} 

			maskbits++;
			bmbits++;
		}

		bmbits+=2;
		maskbits+=2;
	}

	LOGLINE;
	WinSetDrawWindow(olOldWindow);

	dm = WinSetDrawMode(winMask);
	WinPaintBitmap(olBitmask, x1-1, y1-1);
	WinSetDrawMode(winOverlay);
	WinPaintBitmap(olBitmap, x1-1, y1-1);
	WinSetDrawMode(dm);

	WinDeleteWindow(olMaskWindow,false);
	WinDeleteWindow(olWindow,false);

	LOGLINE;
	LOGTAG("Hee");
	LOGTIMESTOP;

}


void PFCopyRectangle(WinHandle src, PFScreenRectType *srcRect, 
		Coord x, Coord y, WinDrawOperation mode) {
	
	RectangleType r2;
	RctSetRectangle(&r2, srcRect->x1, srcRect->y1, (
			srcRect->x2 - srcRect->x1), (srcRect->y2 - srcRect->y1));

	WinCopyRectangle(src, WinGetDrawWindow(), &r2, x, y, mode);

}


void *PFGetScreenAddressAndDimensions(Coord *width, Coord *height, Coord *rowHeight) {
	
	BitmapType *bm = WinGetBitmap(WinGetDrawWindow());
	void *screen = BmpGetBits(bm);
	UInt16 scrWidth, scrHeight, myRowWidth;
	
	LOGENTRY;
	
	BmpGetDimensions(bm, &scrWidth, &scrHeight, &myRowWidth);

	WinGetDisplayExtent(&scrWidth, &scrHeight);
	LOGINT16(scrWidth);
	LOGINT16(scrHeight);
	LOGINT16(myRowWidth);
	ModErrThrowIf(myRowWidth < scrWidth);

	if (width) *width = (Coord)scrWidth;
	if (height) *height = (Coord)scrHeight;
	if (rowHeight) *rowHeight = (Coord)myRowWidth;
	
	LOGEXIT;
	
	return screen;

}

WinHandle PFBitmapLoadToWindow(UInt16 bitmapId, Coord *width, Coord *height) {
	
	WinHandle  newWin;
	WinHandle  oldWin;
	BitmapType *bitmap;
	MemHandle  handle;
	Err        error;
	UInt16     row;
	
	handle = DmGetResource(bitmapRsc, bitmapId);
	bitmap = MemHandleLock(handle);
	BmpGetDimensions(bitmap, width, height,&row);

	newWin = WinCreateOffscreenWindow(*width,*height,
			nativeFormat, &error);
	ModErrThrowIf(error);
	oldWin = WinSetDrawWindow(newWin);
	PFEraseWindow();
	WinDrawBitmap(bitmap,0,0);
	WinSetDrawWindow(oldWin);
	MemHandleUnlock(handle);

	*width *=2;
	*height *=2;
	return newWin;
	
}

BitmapType *PFBitmapLoad(UInt16 bitmapId, Coord *width, Coord *height) {
	
	Coord w,h;
	
	BitmapType *bm = MemHandleLock(DmGetResource(bitmapRsc, bitmapId));
	
	BmpGetDimensions(bm, &w, &h, NULL);

	// NB Multiply by 2, because of high-density
	if (width) *width = 2*w;
	if (height) *height = 2*h;
	
	return bm;
	
}

void PFBitmapPaint(BitmapType *bm, Coord x, Coord y) {
	
	WinPaintBitmap(bm, x, y);
	
}

void PFBitmapFree(BitmapType *bm) {
	
	MemPtrUnlock((void*)bm);
	
}

/*******************************************************************************
 * 
 * Rectangle functions
 * 
 */

void PFPaintRectangle(PFScreenRectType *r, Coord radius) {
	RectangleType r2;

	RctSetRectangle(&r2, r->x1, r->y1, (r->x2 - r->x1), (r->y2 - r->y1));
	WinPaintRectangle(&r2, radius);
	
}

void PFEraseRectangle(PFScreenRectType *r, Coord radius)  {
	RectangleType r2;

	RctSetRectangle(&r2, r->x1, r->y1, (r->x2 - r->x1), (r->y2 - r->y1));
	WinEraseRectangle(&r2, radius);
	
}

void PFInvertRectangle(PFScreenRectType *r, Coord radius)  {
	RectangleType r2;

	RctSetRectangle(&r2, r->x1, r->y1, (r->x2 - r->x1), (r->y2 - r->y1));
	WinInvertRectangle(&r2, radius);
	
}

void PFPaintRectangleFrame(PFScreenRectType *r, UInt16 style) {
	
	RectangleType r2;
	
	RctSetRectangle(&r2, r->x1, r->y1, (r->x2 - r->x1), (r->y2 - r->y1));
	WinPaintRectangleFrame(style, &r2);
	
}

/*
 * screen clipping function
 * 
 */

void PFSetClippingRectangle(const PFScreenRectType *r) {

	RectangleType r2;
	
	RctSetRectangle(&r2, r->x1, r->y1, (r->x2 - r->x1), (r->y2 - r->y1));
	WinSetClip(&r2);
	
}

void PFGetClippingRectangle(PFScreenRectType *r) {

	RectangleType r2;

	WinGetClip(&r2);
	
	PFScreenRectangleSetRel(r, r2.topLeft.x, r2.topLeft.y,	r2.extent.x, r2.extent.y);
	
}

IndexedColorType PFPaletteGetIndex(const RGBColorType *rgb) {
	
	return WinRGBToIndex(rgb);
	
}

void PFPaletteGetRGB(UInt16 index, RGBColorType *rgb) {
	
	WinIndexToRGB(index, rgb);
	
}

void PFPaletteSet(UInt16 fromIndex, UInt16 count, const RGBColorType *rgb) {
	
	WinPalette(winPaletteSet, fromIndex, count, (RGBColorType*)rgb);
	
}


static RGBColorType *oldColours = NULL;
static const UIColorTableEntries reds[17] = {

	UIObjectFrame, UIObjectForeground, UIMenuFrame,
	UIMenuForeground, UIMenuSelectedFill, UIFieldText,
	UIFieldTextLines, UIFieldCaret, UIFieldTextHighlightBackground,
	UIFormFrame, UIDialogFrame, UIAlertFrame, UIOK, UICaution,
	UIWarning, UIObjectSelectedFill, 
	
	UILastColorTableEntry

};
static const UIColorTableEntries blacks[10] = {

	UIObjectFill, UIObjectSelectedForeground, UIMenuFill,
	UIMenuSelectedForeground, UIFieldBackground, 
	UIFieldTextHighlightForeground, UIFormFill, UIDialogFill,
	UIAlertFill, 

	UILastColorTableEntry

};
static const RGBColorType red     = { 0, 200, 0, 0 };
static const RGBColorType brightRed     = { 0, 255, 48, 48 };
static const RGBColorType black        = { 0, 0,0,0};


void PFPaletteSetDay(void) {

	LOGENTRY;

	/*
	 * reset the UI colours if necessary
	 *
	 */

	if (oldColours) {
		
		RGBColorType *i = oldColours;
		Int16 j;

		/*
		 * reset palette entries
		 *
		 */

		PFPaletteSet(0, 1, i++);
		PFPaletteSet(255, 1, i++);

		for (j=0; reds[j]!=UILastColorTableEntry; j++) 
			UIColorSetTableEntry(reds[j], i++);

		for (j=0; blacks[j]!=UILastColorTableEntry; j++) 
			UIColorSetTableEntry(blacks[j], i++);

		LOGLINE;

		PFMemFree(oldColours);

	}
		
	LOGEXIT;

}

void PFPaletteSetNight(void) {

	RGBColorType c;
	Int16 j;

	RGBColorType *i;

	if (!oldColours) {

		PFSafeMalloc(oldColours, 30 * sizeof(RGBColorType));

	}

	i = oldColours;

	/*
	 * set black/white palette entries
	 *
	 */

	PFPaletteGetRGB(0,&c);
	*i++ = c;
	c.r = 0; c.g = 0; c.b = 0;
	PFPaletteSet(0, 1, &c);

	PFPaletteGetRGB(255, &c);
	*i++ = c;
	c.r = 0; c.g = 0; c.b = 0;
	PFPaletteSet(255, 1, &c);

	for (j=0; reds[j] != UILastColorTableEntry;j++) {

		UIColorGetTableEntryRGB(reds[j], i++);
		UIColorSetTableEntry(reds[j], &red);

	}

	for (j=0; blacks[j] != UILastColorTableEntry;j++) {
		
		UIColorGetTableEntryRGB(blacks[j], i++);
		UIColorSetTableEntry(blacks[j], &black);

	}

	UIColorSetTableEntry(UIObjectSelectedFill, &brightRed);

}



/*****************************************************************************
 *
 * Miscellaneous Functions
 *
 */
void PFExitApp(void) {
	
	EventType e;
	
	e.eType = keyDownEvent;
	e.data.keyDown.chr = launchChr;
	e.data.keyDown.modifiers = commandKeyMask;
	PFEventSend(&e, false);
	

}

void PFSecondsToTimeStamp(UInt32 secs, PFTimeStampType *ts) {
	
	DateTimeType dt;
	
	TimSecondsToDateTime(secs, &dt);
	
	ts->year = dt.year;
	ts->month= dt.month;
	ts->day  = dt.day;
	ts->hour = dt.hour;
	ts->minute=dt.minute;
	ts->second=dt.second;
	
	ts->milliSecond=0;
	
}

/*
 * PFGetSeconds
 *
 * Returns current platform time in seconds
 *
 */

UInt32 PFGetSeconds(void) {

	return TimGetSeconds();

}

/*
 * PFGetTicks
 *
 */

UInt32 PFGetTicks(void) {

	return TimGetTicks() - appLaunchTime;

}

/*
 * function : PFTimerHasExpired
 *
 */

Boolean PFTimerHasExpired(UInt32 startTime, UInt32 plusTicks) {

	UInt32 now=PFGetTicks();
	UInt32 expires=startTime+plusTicks;
	
	if (expires > startTime) {
		if (now >= expires)
			return true;
		else
			return false;
	}

	/* deal with wraparound system ticks */
	if (!(now & 0x80000000) && now >= expires)
		return true;

	return false;
}

/*
 * function : PFTimerTicksLeft
 *
 */

UInt32 PFTimerTicksLeft(UInt32 startTime, UInt32 plusTicks) {
	UInt32 now=PFGetTicks();
	UInt32 expires=startTime+plusTicks;
	
	if (expires > startTime) {
		return now<expires ? expires-now : 0;
	}

	/* deal with wraparound system ticks */
	if (now & 0x80000000) {
		return (0xFFFFFFFF-now) + expires;
	} else {
		return now<expires ? expires-now : 0;
	}
}

/*
 * PFGetEvent
 *
 */

extern Boolean AppHandleEvent(EventPtr evt);

Boolean PFGetEvent(EventPtr event, UInt32 maxTicks) {

	UInt32 nowTime = PFGetTicks();

	/*
	 * newer PalmOS devices send a nilEvent too fast, so an extra
	 * timing check must be made
	 *
	 */

	do {

		EvtGetEvent(event, maxTicks);

	} while (event->eType == nilEvent && !PFTimerHasExpired(nowTime, maxTicks));

	if (event->eType == winDisplayChangedEvent) {

		SetupScreenInfo();
		
	}

	if (event->eType == keyDownEvent) {

		/*
		 * intercept the vchrNavChange event, and substitute the
		 * vchrRockerLeft & Right keycodes
		 *
		 */

		if (GarminKeyIsGarmin(event)) {

			switch (event->data.keyDown.keyCode) {

			case vchrGarminThumbWheelUp:
				event->data.keyDown.chr = vchrRockerRight;
				break;

			case vchrGarminThumbWheelDown:
				event->data.keyDown.chr = vchrRockerLeft;
				break;

			case vchrGarminThumbWheelIn:
				event->data.keyDown.chr = vchrRockerCenter;
				break;

			case vchrGarminEscapeHeld:
				event->data.keyDown.chr = vchrPowerSave;
				break;
				
			default:
				event->eType = nilEvent;
				break;
				
			}
				
		}

		switch (event->data.keyDown.chr) {

		case vchrNavChange:
			if (NavKeyPressed(event, Left)) 
				
				event->data.keyDown.chr = vchrRockerLeft;

			else if (NavKeyPressed(event, Right)) 
				
				event->data.keyDown.chr = vchrRockerRight;

			else if (NavKeyPressed(event, Select))

				event->data.keyDown.chr = vchrRockerCenter;

			break;

		/*
		 * sony mappings
		 *
		 */

		//case vchrRockerDown:
		case 1700:
			event->data.keyDown.chr = vchrPageDown;
			break;

		//case vchrRockerUp:
		case 1701:
			event->data.keyDown.chr = vchrPageUp;
			break;

		}

		switch (event->data.keyDown.chr) {

		/*
		 * check for long keypresses, and convert
		 *
		 */

		case vchrF1:
		case vchrF2:
		case vchrF3:
		case vchrF4:
			event->data.keyDown.chr = CheckForLongPress(event->data.keyDown.chr);
			break;

		/*
		 * check for diagonal keypresses
		 *
		 */

		case vchrPageUp:
			if (HardKeyDown(hardLeft)) {

				event->data.keyDown.chr = vchrUpLeft;

			} else if (HardKeyDown(hardRight)) {

				event->data.keyDown.chr = vchrUpRight;

			}
			break;

		case vchrPageDown:
			if (HardKeyDown(hardLeft)) {

				event->data.keyDown.chr = vchrDownLeft;

			} else if (HardKeyDown(hardRight)) {

				event->data.keyDown.chr = vchrDownRight;

			}
			break;

		case vchrRockerLeft:
			if (HardKeyDown(hardUp)) {

				event->data.keyDown.chr = vchrUpLeft;

			} else if (HardKeyDown(hardDown)) {

				event->data.keyDown.chr = vchrDownLeft;

			}
			break;

		case vchrRockerRight:
			if (HardKeyDown(hardUp)) {

				event->data.keyDown.chr = vchrUpRight;

			} else if (HardKeyDown(hardDown)) {

				event->data.keyDown.chr = vchrDownRight;

			}
			break;

		}


	}

	/*
	 * change the Screen info structure if the user has changed
	 * display orientation or opened/closed the input area
	 *
	 */

	CollapseCheckWinEnterEvent(event);

#ifdef TEST_FRAMEWORK
	if (recordEventFile) {
		
		RecordEvent(PFGetTicks(), event);
		
	} else if (playbackEventFile) {
		
		PlayBackEvent(playbackEventFile);
		
	}
#endif
	
	return event->eType == nilEvent ? false : true;

}


void PFHandleEvent(EventPtr event, Boolean (*appLevelHandler)(EventPtr)) {
	
	if (!SysHandleEvent(event)) {

		Err error;

		if (! MenuHandleEvent(0, event, &error)) {

			if (! appLevelHandler(event)) {

				if (FrmGetActiveFormID() && !FrmDispatchEvent(event)) {
					
					LOGTAG("Done");
					
				}

			}
			
		}
		
	}
	
}

/*
 * PFEventSend
 *
 * Sends the specified event 
 *
 */

void PFEventSend(EventPtr event, Boolean unique) {

	if (unique) {
		
		EvtAddUniqueEventToQueue(event,0, false);
		
	} else {

		EvtAddEventToQueue(event);
		
	}

}
void PFEventSendMenu(UInt16 menuID) {
	
	EventType e;
	
	e.eType = menuEvent;
	e.data.menu.itemID = menuID;
	
	PFEventSend(&e,true);
	
}

void PFEventSendKey(WChar chr) {
	
	EventType e;
	
	e.eType = keyDownEvent;
	e.data.keyDown.chr = chr;
	e.data.keyDown.modifiers = 0;
	
	PFEventSend(&e,false);
	
}

Boolean PFEventAvailable(void) {
	
	return EvtEventAvail();
	
}

/*
 *  function: PFGetUserID
 *
 */

char *PFGetUserID(void) {

	char *uid = PFMalloc(dlkUserNameBufSize+1);
	DlkGetSyncInfo(NULL,NULL,NULL,uid,NULL,NULL);

	return uid;

}

/*
 *  function: PFGetSerialNumber
 *
 */

char *PFGetSerialNumber(void) {

	UInt16 serialNumSize;
	UInt8  *serialNum;
	char   *serialNumCopy;
	
	SysGetROMToken(0, sysROMTokenSnum, &serialNum, &serialNumSize);
	if (serialNumSize == 0) {
		
		Err err;
		
		serialNumSize = hsVersionStringSize+1;
		serialNumCopy = PFMalloc(serialNumSize);
		err = HsGetVersionString(hsVerStrSerialNo, serialNumCopy, &serialNumSize);

	} else {
		
		serialNumCopy = PFMalloc(serialNumSize+1);
		MemMove(serialNumCopy, serialNum, serialNumSize);
		serialNumCopy[serialNumSize] = 0;
		
	}

	return serialNumCopy;

}

void PFScreenRectangleSet(PFScreenRectType *r, Coord x1, Coord y1, Coord x2, Coord y2) {
	
	r->x1 = x1; r->y1 = y1;
	r->x2 = x2; r->y2 = y2;
	
}

void PFScreenRectangleSetRel(PFScreenRectType *r, Coord x1, Coord y1, Coord width, Coord height) {
	
	r->x1 = x1; r->y1 = y1;
	r->x2 = r->x1 + width;
	r->y2 = r->y1 + height;
	
}

void PFScreenRectangleInset(PFScreenRectType *r, Int16 insetBy) {
	
	r->x1+=insetBy; r->y1+=insetBy;
	r->x2-=insetBy; r->y2-=insetBy;
	
}

/*
 * function : PFScreenPointInRectangle
 * 
 */

Boolean PFScreenPointInRectangle(Coord x, Coord y, const PFScreenRectType *r) {
	
	return (x >= r->x1 && x <= r->x2 && y >= r->y1 && y <= r->y2);
	
}



/********************************************************************************
 *
 * PALM OS SECTION
 *
 */

static Err RomVersionCompatible(UInt32 requiredVersion, UInt16 launchFlags) {

	UInt32 romVersion;
	UInt32 supportedDepths;

	/*  See if we're on in minimum required version of the ROM or later. */
	FtrGet(sysFtrCreator, sysFtrNumROMVersion, &romVersion);
	if (romVersion < requiredVersion) {
		UInt16 safeToCallAlertFlags;
		
		safeToCallAlertFlags = 
			sysAppLaunchFlagNewGlobals | sysAppLaunchFlagUIApp;
		if ((launchFlags & (safeToCallAlertFlags)) == safeToCallAlertFlags) {
			FrmAlert (RomIncompatibleAlert);
		
			/*  Pilot 1.0 will continuously relaunch this app unless we switch to  */
			/*  another safe one. */
			if (romVersion < sysMakeROMVersion(2,0,0,sysROMStageRelease,0))
				AppLaunchWithCommand(sysFileCDefaultApp, 
					sysAppLaunchCmdNormalLaunch, NULL);
		}
		
		return (sysErrRomIncompatible);
	}

	/*
	 * check for colour support. OK if more than 4 bits per pixel
	 *
	 */

	ScrDisplayMode(scrDisplayModeGetSupportedDepths, NULL,NULL, &supportedDepths, NULL);

	return (supportedDepths > 0x0F) ? errNone : sysErrRomIncompatible;

}


extern Err FlightMaster(Boolean launchedFromCoPilot) STARTUP_SECTION;

char ErrExtra[128] = " " ;	// extra error info from ErrThrows

/*  The minimum OS version we support */
#define kOurMinVersion	sysMakeROMVersion(4,5,0,sysROMStageRelease,0)

/*
 * function : PilotMain
 *
 * Main function called by the PalmOS when FlightMaster is launched.
 *
 */

UInt32 PilotMain(UInt16 launchCode, MemPtr launchParameters, UInt16 launchFlags) {

	Err error;
	UInt32 vfsMgrVersion;
	Boolean launchedFromCoPilot = false;
	
	switch (launchCode) {
	
	case sysAppLaunchCmdSyncNotify:

		/*
		 * we've just been hotsync'd to the Palm,
		 * register ourself for .fma data files
		 *
		 */

		//ExgRegisterData(FlightMasterCreatorId, exgRegExtensionID, "FMA");

		if (!FtrGet(sysFileCVFSMgr, vfsFtrIDVersion, &vfsMgrVersion)) {

			VFSRegisterDefaultDirectory(".fma", expMediaType_Any, FILEROOT);

		}
		break;

	case sysAppLaunchCmdExgReceiveData:
		//AsImportFMAData(*(ExgSocketType**)launchParameters);
		break;

	case sysAppLaunchCmdNotify:

		if (((SysNotifyParamType*) launchParameters)->notifyType 
				== sysNotifyDisplayResizedEvent) {

			EventType e;

			MemSet(&e, sizeof(e),0);
			e.eType= winDisplayChangedEvent;
			EvtAddUniqueEventToQueue(&e, 0, true);

		}
		break;

#define sysAppLaunchCmdCoPilotLaunch 0x8116
	case sysAppLaunchCmdCoPilotLaunch:
		launchedFromCoPilot=true; 
		launchCode = sysAppLaunchCmdNormalLaunch;
		// NB : No 'break' - intentionally!

	case sysAppLaunchCmdNormalLaunch:
		error = RomVersionCompatible (kOurMinVersion, launchFlags);
		if (error) return error;

		if (!FtrGet(sysFileCVFSMgr, vfsFtrIDVersion, &vfsMgrVersion)) {

			VFSRegisterDefaultDirectory(".fma", expMediaType_Any, "/PALM/FM");

		}
		ErrTry {

			UInt32	oldScreenDepth;
#ifdef LOGGING
			LogOpen();
#endif

#ifndef AEROPALM
			AlmEnableNotification(false);
#endif
			/*
			 * set up the display
			 *
			 */

			ScrDisplayMode(scrDisplayModeGet, NULL, NULL, &oldScreenDepth, NULL);


			/*
			 * set screen to 8 bits
			 *
			 * fix for palmos 5 - 16bit colour mode doesn't work properly, set
			 * the mode to 8 bit
			 *
			 */

			if (oldScreenDepth != 8) { 

				UInt32 newDepth = 8;

				/*
				 * NB set depth uses actual number bits, not bitmask!!
				 *
				 */

				ScrDisplayMode(scrDisplayModeSet, NULL, NULL, &newDepth, NULL);

			} 

			SetupScreenInfo();
			CollapseAppStart();

			error = FlightMaster(launchedFromCoPilot);

			CollapseAppStop();
			
			ScrDisplayMode(scrDisplayModeSet, NULL, NULL, &oldScreenDepth, NULL);

#ifndef AEROPALM
			AlmEnableNotification(true);
#endif

			if (error) {
#ifdef LOGGING
				LogClose();
#endif
				return error;

			}

			FrmCloseAllForms();

		} ErrCatch(errNo) {

			switch(errNo) {

			default: {
				char s[128]; 
				StrPrintF(s,"Internal Error:%x %u",
					(UInt16)((errNo & 0xffff0000) >> 16),
					(UInt16)(errNo & 0xffff));
				FrmCustomAlert(GenericWarningAlert,s,ErrExtra,NULL);
				 }
				break;
			}

		} ErrEndCatch;

#ifdef LOGGING
		LogClose();
#endif

		break;

	default:
		break;

	}

	return errNone;
}



/*
 * function : PceNativeResourceCall
 *
 */

UInt32 PceNativeResourceCall(DmResType resType, DmResID resID,
	char *DLLEntryPointP, void *userCPB) { 
	UInt32    processorType; 
	MemHandle armH; 
	MemPtr    armP; 
	UInt32    result; 
					 
	LOGENTRY;

	//	get the processor type 
	//
	FtrGet(sysFileCSystem, sysFtrNumProcessorID, &processorType); 
      
	if (sysFtrNumProcessorIsARM(processorType)){ 

		// running on ARM; call the actual ARM resource 
		//

		LOGLINE;

		armH = DmGetResource(resType, resID); 
		armP = MemHandleLock(armH); 

		result = PceNativeCall(armP, userCPB); 

		LOGINT32(result);

		MemHandleUnlock(armH); 
		DmReleaseResource(armH); 

	} else if (processorType == sysFtrNumProcessorx86) { 

		// running on Simulator; call the DLL 
		result = PceNativeCall((NativeFuncType *)DLLEntryPointP, userCPB); 

	} else { 

		// some other processor; fail gracefully 
		ModErrThrow();

		result = -1; 

		LOGLINE;

	} 

	LOGEXIT;
	return result; 

} 


