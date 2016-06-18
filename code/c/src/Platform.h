/*
 * Platform.h
 *
 * Platform is a platform abstraction layer module that
 * allows FlightMaster to interface to Hardware and OS-level
 * features in an independant manner.
 *
 */

#ifndef PLATFORM_H
#define PLATFORM_H

#include <PalmOS.h>
#include <VFSMgr.h>
#include <BtLib.h>
#include <BtCommVdrv.h>

#include "Constants.h"

typedef enum { pfFileReadOnly, pfFileReadWrite, pfFileTruncate } PFFileMode;

typedef enum { pfSeekStart, pfSeekEnd, pfSeekCurrent} PFFileSeek;

typedef struct PFFileRefStructType *PFFileRef;

extern char ErrExtra[];

typedef struct {
	
	Coord x1,y1;
	Coord x2,y2;
	
} PFScreenRectType;


/*
 * screen information
 *
 */

typedef struct {

	/*
	 * screen capabilities
	 *
	 */

	Boolean highDensity;
	Boolean landscape;

	Coord width;
	Coord height;
	Coord xcentre;
	Coord ycentre;

	/*
	 * font height
	 *
	 */

	Coord boldHeight;
	Coord largeBoldHeight;
	Coord ledHeight;
	
	Coord stdCharWidth;

} ScreenInfoType;

/*
 * tuning variables for screen sizes
 * 
 */

#define DevicePixelsScalar 320
#define StandardPageWidth 320
#define StandardPageHeight 320

extern ScreenInfoType pfScreen;

/*******************************************************************************
 * 
 * Miscellaneous Functions
 *
 */

/*
 * PFInit/PFDeinit
 *
 * Called to initialise/de-initialise the platform module
 *
 */

extern void PFInit(void) PLATFORM_SECTION;
extern void PFDeinit(void)  PLATFORM_SECTION;

extern void /*@noreturn@*/ PFErrExit(UInt32 ev) PLATFORM_SECTION;

#define ModErrThrowIf(c) if (c)  PFErrExit(ModuleID | __LINE__);
#define ModErrThrow() PFErrExit(ModuleID | __LINE__);

/*
 * PFPlaySound
 *
 * Play the specified sound from the external card
 *
 */

extern void PFPlaySound(const char *filename, Int32 volume) PLATFORM_SECTION;

/*
 * PFIsCursorKey
 *
 */

#define PFIsCursorKey(key) ((key == vchrUpLeft) || (key == vchrUpRight) || (key == vchrDownRight) || (key == vchrDownLeft) || (key == vchrPageUp) || (key == vchrPageDown) || (key == vchrRockerLeft) || (key == vchrRockerRight))

#define vchrUpLeft 0x20F0
#define vchrUpRight 0x20F1
#define vchrDownRight 0x20F2
#define vchrDownLeft 0x20F3
#define vchrF1 vchrHard1
#define vchrF2 vchrHard2
#define vchrF3 vchrHard3
#define vchrF4 vchrHard4
#define vchrShiftF1  0x20F4
#define vchrShiftF2  0x20F5
#define vchrShiftF3  0x20F6
#define vchrShiftF4  0x20F7
#define vchrPowerSave 0x20F8

/*
 * PFKeyHeld
 *
 */

#define hardUp 0x2
#define hardDown 0x4
#define hardLeft 0x40000
#define hardRight 0x80000
#define hardCentre 0x100000

#define PFKeyHeld(k) (KeyCurrentState() & (k))

#define pfCursorKeys (hardUp | hardDown | hardLeft | hardRight | hardCentre)

/*******************************************************************************
 * 
 * Memory Functions
 *
 */


#ifndef LOG_MEMORY_ALLOCATIONS
#define PFMalloc(size) PFMalloc1(size)
#else
#define PFMalloc(size) PFMallocLogged(__FILE__,__LINE__,size)
#endif

extern /*@only@*/ /*@null@*/ void *PFMalloc1(UInt32 size) PLATFORM_SECTION;

extern void *PFMallocLogged(const char *file,UInt32 line,UInt32 size) PLATFORM_SECTION;

extern UInt32 PFMallocSize(void *ptr) PLATFORM_SECTION;

extern void PFMallocResize(void *oldPtr, UInt32 newSize) PLATFORM_SECTION;


/*
 * PFMemFree1 - do not call directly, use PFMemFree
 *
 * (Provided for splinting the code)
 *
 */

extern void PFMemFree1(/*@only@*/ /*@out@*/ /*@null@*/ void *ptr) PLATFORM_SECTION;

/*
 * use PFSafeMalloc to ensure that the pointer is free before the memory is
 * allocated, and that the memory is allocated successfully.
 *
 */

#define PFSafeMalloc(ptr, s) do { ModErrThrowIf(ptr);ptr=PFMalloc(s);ModErrThrowIf(!ptr); } while(0);

#define PFMemFree(ptr) do { ModErrThrowIf(!ptr);PFMemFree1(ptr);ptr=NULL; } while(0);

#define PFMemPtrSize(ptr) MemPtrSize(ptr)

/*******************************************************************************
 * 
 * File Functions
 *
 */

/*
 * PFOpenFile
 *
 * For external files, use filename with a leading "/", otherwise the file is
 * an internal file.
 *
 * Returns file reference, or NULL if file could not be opened
 *
 */

extern PFFileRef PFOpenFile(const char *filename, PFFileMode mode) PLATFORM_SECTION;

/*
 * PFCloseFile
 *
 */

extern void PFCloseFile(PFFileRef /*@only@*/ /*@out@*/ /*@null@*/ f) PLATFORM_SECTION;

/*
 * PFFileSize
 *
 */

extern UInt32 PFFileSize(PFFileRef f) PLATFORM_SECTION;

/*
 * PFWriteFile
 *
 * Returns number of bytes written
 *
 */

extern Int32 PFWriteFile(PFFileRef f, const void *data, UInt32 size) PLATFORM_SECTION;

/*
 * PFReadFile
 *
 * Returns number of bytes read, if less that maxSize then EOF has been
 * encountered
 * 
 */

extern Int32 PFReadFile(PFFileRef f, /*@out@*/ void *dst, UInt32 maxSize) PLATFORM_SECTION;

/*
 * PFSeekFile
 *
 * Seek to the specified position in the file
 *
 */

extern void PFSeekFile(PFFileRef f, PFFileMode mode, Int32 whereTo) PLATFORM_SECTION;

/*
 * PFReadLine
 *
 * Read a line from the specified file, returns false if the EOF is detected
 *
 */

extern Boolean PFReadLine(PFFileRef f, /*@out@*/ char *result) PLATFORM_SECTION;

/*
 * PFTruncateFile
 *
 * Shortens the file to the specified length
 *
 */

extern void PFTruncateFile(PFFileRef f, UInt32 newLength) PLATFORM_SECTION;

/*******************************************************************************
 * 
 * Serial/Bluetooth I/O Functions
 * 
 */

typedef UInt16 PFPortRefType;

typedef enum { 
	
	portCradle = 0, 
	portSerial, 
	portBluetooth, 
	portCard,
	portClie,
	portUSB

} PFPortType;

typedef BtLibDeviceAddressType PFBluetoothAddressType;


extern PFPortRefType PFPortOpen(PFPortType port, UInt32 baudRate) PLATFORM_SECTION;

extern PFPortRefType PFPortOpenBluetooth(PFBluetoothAddressType *btaddr, Boolean scanIfFailToConnect) PLATFORM_SECTION;

extern void PFPortClose(PFPortRefType port) PLATFORM_SECTION;

extern UInt32 PFPortRead(PFPortRefType port, void *buffer, UInt32 maxLength) PLATFORM_SECTION;

/*******************************************************************************
 *
 * Database functions
 *
 */

/*
 * DBOpen
 *
 * Tries to open the specified database, or creates it if it doesn't exist and
 * createIfMissing is true
 *
 */

extern DmOpenRef DBOpen(const char *dbname, Boolean readOnly, Boolean createIfMissing) PLATFORM_SECTION;

/*
 * DBGetInfo
 *
 * Returns pointer to database information block. Pointer must be
 * disposed of by caller
 *
 */

extern MemPtr DBGetInfo(DmOpenRef ref) PLATFORM_SECTION;


#define DBNewRecord dmMaxRecordIndex

extern UInt16 DBRecordCreate(DmOpenRef ref, UInt16 atIndex, void *data, UInt16 recordSize) PLATFORM_SECTION;

extern void DBRecordDelete(DmOpenRef ref, UInt16 recnum) PLATFORM_SECTION;

/*
 * PFGetRecord
 * 
 * set copy=true for a copy of the record, otherwise caller must use PFFreeRecord
 * to clean up after
 * 
 */

extern void *DBRecordGet(DmOpenRef ref, UInt16 recnum, Boolean copy) PLATFORM_SECTION;

extern void DBRecordUpdate(DmOpenRef ref, UInt16 recnum, UInt16 offset, void *data, UInt16 datasize) PLATFORM_SECTION;

extern Boolean DBRecordResize(DmOpenRef ref, UInt16 recnum, UInt16 newsize) PLATFORM_SECTION;

#define DBRecordNotFound 65535

extern UInt16 DBRecordGetIndexByPalmID(DmOpenRef ref, UInt32 id) PLATFORM_SECTION;

extern void DBRecordFree(void *rec) PLATFORM_SECTION;

extern UInt16 DBRecordGetSize(DmOpenRef ref, UInt16 recnum) PLATFORM_SECTION;

	
/*
 * PFCloseDatabase
 *
 * Close the specified database
 *
 */

extern void DBClose(DmOpenRef ref) PLATFORM_SECTION;

/*
 * DBDelete
 *
 * What it says on the tin
 *
 */

extern void DBDelete(const char *dbname)  PLATFORM_SECTION;

/*
 * DBGetNumRecords
 *
 */

#define DBGetNumRecords(db) DmNumRecordsInCategory(db, dmAllCategories)

/*
 * DBGetDates
 *
 */

void DBGetDates(DmOpenRef db, /*@out@*/ /*@null@*/ UInt32 *createDate, /*@out@*/ /*@null@*/ UInt32 *modDate)  PLATFORM_SECTION;

/*
 * DBFRecordGet
 *
 * Read record from file-based PDB
 *
 */

extern /*@shared@*/ void *DBFRecordGet(PFFileRef f, UInt16 recNum, Boolean copy) PLATFORM_SECTION;

/*
 * DBFGetNumRecords
 *
 * Returns number of records in file-based PDB
 *
 */

extern UInt16 DBFGetNumRecords(PFFileRef f) PLATFORM_SECTION;

/*
 * DBFGetDates
 *
 * Returns file dates for the specified file
 *
 */

extern void DBFGetDates(PFFileRef f, /*@out@*/ UInt32 *createDate, /*@out@*/ UInt32 *modDate) PLATFORM_SECTION;

/*
 * DBFGetInfo
 *
 * Returns pointer to database information block, for a file-based database.
 * Pointer must be disposed of by caller
 *
 */

extern MemPtr DBFGetInfo(PFFileRef f) PLATFORM_SECTION;


/*****************************************************************************
 *
 * GUI Functions
 *
 */

typedef enum { GUIFormFill, GUIObjectForeground, GUIObjectFill, GUIFormFrame } PFSystemColourType;

/* 
 * Forms
 * 
 */

extern UInt16 GUIAlertShow(UInt16 alertID) PLATFORM_SECTION;

extern UInt16 GUICustomAlertShow(UInt16 alertID, const char *f1, const char *f2, const char *f3) PLATFORM_SECTION;

extern void GUIFormLoad(UInt16 formID, Boolean (*evtHandler)(EventPtr)) PLATFORM_SECTION;

extern void GUIFormGoto(UInt16 formID) PLATFORM_SECTION;

extern void GUIFormPopup(UInt16 formID) PLATFORM_SECTION;

extern void GUIFormReturn(void) PLATFORM_SECTION;

extern UInt16 GUIFormGetActiveID(void) PLATFORM_SECTION;

extern void GUIFormDraw(void) PLATFORM_SECTION;

extern void GUIFormSetTitle(const char *newTitle) PLATFORM_SECTION;

extern Boolean GUIFormResize(Boolean isModal, Boolean moveForm) PLATFORM_SECTION;

extern void GUIFormResizeGetDeltas(Int16 *xdelta, Int16 *ydelta) PLATFORM_SECTION;

/*
 * Objects
 * 
 */

extern void GUIObjectMove(UInt16 controlID, Int16 xdelta, Int16 ydelta) PLATFORM_SECTION;

extern void GUIObjectResize(UInt16 controlID, Int16 xdelta, Int16 ydelta) PLATFORM_SECTION;

extern void GUIObjectDraw(UInt16 controlID) PLATFORM_SECTION;

extern void GUIFocusSet(UInt16 controlID) PLATFORM_SECTION;

extern void GUIObjectHide(UInt16 controlID) PLATFORM_SECTION;

extern void GUIObjectShow(UInt16 controlID) PLATFORM_SECTION;

extern void GUIObjectGetBounds(UInt16 controlID, PFScreenRectType *r) PLATFORM_SECTION;

extern void GUIObjectSetBounds(UInt16 controlID, PFScreenRectType *r) PLATFORM_SECTION;

extern Int16 GUIObjectGetValue(UInt16 controlID) PLATFORM_SECTION;

extern void GUIObjectSetValue(UInt16 controlID, Int16 value) PLATFORM_SECTION;

extern UInt16 GUIObjectGroupGetValue(UInt16 groupID) PLATFORM_SECTION;

extern void GUIObjectGroupSetValue(UInt16 groupID, UInt16 controlID) PLATFORM_SECTION;

extern void GUIObjectSetLabelPtr(UInt16 controlID, const char *label) PLATFORM_SECTION;

extern void GUIObjectSetLabel(UInt16 controlID, const char *label) PLATFORM_SECTION;

extern void GUIObjectSetBitmap(UInt16 controlID, UInt16 bitmapID) PLATFORM_SECTION;

extern void GUIObjectTap(UInt16 controlID) PLATFORM_SECTION;


/*
 * Fields
 * 
 */

extern void GUIFieldSetFocus(UInt16 controlID) PLATFORM_SECTION;

extern void GUIFieldSetText(UInt16 controlID, const char *text, Boolean redraw) PLATFORM_SECTION;

extern char *GUIFieldGetText(UInt16 fieldID) PLATFORM_SECTION;

extern Boolean GUIFieldIsDirty(UInt16 fieldID) PLATFORM_SECTION;

extern void GUIFieldClean(UInt16 fieldID) PLATFORM_SECTION;

extern void GUIFieldSetSelection(UInt16 fieldID, Int16 from, Int16 to) PLATFORM_SECTION;


/*
 * Scroll bars
 * 
 */

extern void GUIScrollBarSet(UInt16 scrollID, Int16 val, Int16 min, Int16 max, Int16 pageSize) PLATFORM_SECTION;



/*
 * Lists
 * 
 */

extern void GUIListSetNumItems(UInt16 listID, Int16 numChoices) PLATFORM_SECTION;

extern void GUIListSetSelection(UInt16 listID, Int16 item) PLATFORM_SECTION;

extern void GUIListSetDrawFunction(UInt16 listID, void (*f)(Int16 itemNum, PFScreenRectType *bounds)) PLATFORM_SECTION;
extern void GUIListClearDrawFunction(UInt16 listID) PLATFORM_SECTION;

extern void GUIListMakeItemVisibile(UInt16 listID, Int16 item) PLATFORM_SECTION;

extern void GUIListDraw(UInt16 listID) PLATFORM_SECTION;

extern Int16 GUIListGetSelection(UInt16 listID) PLATFORM_SECTION;

extern char *GUIListGetItemText(UInt16 listID, Int16 item) PLATFORM_SECTION;

extern Int16 GUIListGetNumItems(UInt16 listID) PLATFORM_SECTION;

extern Int16 GUIListGetNumVisibleLines(UInt16 listID) PLATFORM_SECTION;

extern void GUIListScroll(UInt16 listID, Int16 lines) PLATFORM_SECTION;

/*
 * Others
 * 
 */

extern void GUIBitmapDraw(UInt16 bitmapID, Coord x, Coord y) PLATFORM_SECTION;


extern Boolean GUIMenuIsDisplayed(void) PLATFORM_SECTION;

/*
 * Colour management
 * 
 */

extern IndexedColorType GUIGetSystemColour(PFSystemColourType colour) PLATFORM_SECTION;

/*****************************************************************************
 *
 * Graphics Functions
 *
 */

typedef enum { blockMode, overlayMode } PFDrawModeType;

extern PFDrawModeType PFSetDrawMode(PFDrawModeType mode) PLATFORM_SECTION;

/*
 * PFDrawThickLines
 *
 * Does what it says on the tin ;-)
 *
 * Lines are drawn in the specified colour, with the specified border colour
 *
 */

enum { noBorder = -1, darkBorder = 256, lightBorder = 257 };

#define LineType WinLineType

extern void PFDrawThickLines(LineType *lines, Int16 numLines, 
		IndexedColorType inside, IndexedColorType outlineColour, Int16 outlineThickness) PLATFORM_SECTION;


/*
 * PFDrawLines
 * 
 */

#define PFDrawLines(num, lines) WinPaintLines(num, lines)
#define PFDrawLine(x1,y1,x2,y2) WinPaintLine(x1,y1,x2,y2)
#define PFEraseLine(x1,y1,x2,y2) WinEraseLine(x1,y1,x2,y2)

#define PFSetForeColour(col) WinSetForeColor(col)
#define PFSetBackColour(col) WinSetBackColor(col)
#define PFSetTextColour(col) WinSetTextColor(col)

/*
 * PFScreenLock & PFScreenUnlock
 * 
 */

extern void PFScreenLock(Boolean copyCurrentScreen) PLATFORM_SECTION;

extern void PFScreenUnlock(void) PLATFORM_SECTION;

extern void PFDrawStatePush(void) PLATFORM_SECTION;

#define PFDrawStatePop WinPopDrawState

#define PFEraseWindow WinEraseWindow

/*
 * Character drawing functions
 * 
 */

#define PFDrawChars WinPaintChars
#define PFPaintChars WinPaintChars
#define PFDrawCharsTruncated WinDrawTruncChars

extern Coord PFDrawOutlineChars(const char *str, UInt8 align, Coord x1, Coord y1) PLATFORM_SECTION;
	// returns width of chars

/*
 * Outlining bitmaps
 * 
 */

extern void PFOutlineBitmapInit(Coord width, Coord height) PLATFORM_SECTION;
extern void PFOutlineBitmapDraw(Coord x1, Coord y1) PLATFORM_SECTION;


/*
 * Rectangle drawing functions
 * 
 */

#define PFFillPatternSetGray() WinSetPatternType(grayPattern)
extern void PFPaintRectangle(PFScreenRectType *r, Coord radius) PLATFORM_SECTION;
extern void PFEraseRectangle(PFScreenRectType *r, Coord radius) PLATFORM_SECTION;
extern void PFInvertRectangle(PFScreenRectType *r, Coord radius) PLATFORM_SECTION;
extern void PFPaintRectangleFrame(PFScreenRectType *r, UInt16 style) PLATFORM_SECTION;
 
extern void PFSetClippingRectangle(const PFScreenRectType *r) PLATFORM_SECTION;
extern void PFGetClippingRectangle(PFScreenRectType *r) PLATFORM_SECTION;

#define PFResetClipping WinResetClip

extern IndexedColorType PFPaletteGetIndex(const RGBColorType *rgb) PLATFORM_SECTION;
extern void PFPaletteGetRGB(UInt16 index, RGBColorType *rgb) PLATFORM_SECTION;
extern void PFPaletteSet(UInt16 fromIndex, UInt16 count, const RGBColorType *rgb) PLATFORM_SECTION;
extern void PFPaletteSetDay(void) PLATFORM_SECTION;
extern void PFPaletteSetNight(void) PLATFORM_SECTION;

extern void PFCopyRectangle(WinHandle src, PFScreenRectType *srcRect, 
		Coord x, Coord y, WinDrawOperation mode) PLATFORM_SECTION;

extern void *PFGetScreenAddressAndDimensions(Coord *width, Coord *height, Coord *rowWidth) PLATFORM_SECTION;

extern WinHandle PFBitmapLoadToWindow(UInt16 bitmapId, Coord *width, Coord *height) PLATFORM_SECTION;

#define PFWindowDelete WinDeleteWindow

extern BitmapType *PFBitmapLoad(UInt16 bitmapId, Coord *width, Coord *height) PLATFORM_SECTION;

extern void PFBitmapPaint(BitmapType *bm, Coord x, Coord y) PLATFORM_SECTION;

extern void PFBitmapFree(BitmapType *bm) PLATFORM_SECTION;


/*****************************************************************************
 *
 * Miscellaneous Functions
 *
 */

typedef struct {
	
	Int16 year, month, day;
	Int16 hour, minute, second;

	Int16 milliSecond;
	
} PFTimeStampType;

/*
 * PFExitApp
 * 
 */

extern void PFExitApp(void) PLATFORM_SECTION;

/*
 * PFSecondsToTimeStamp
 * 
 */

extern void PFSecondsToTimeStamp(UInt32 secs, PFTimeStampType *ts) PLATFORM_SECTION;

/*
 * PFGetSeconds
 *
 * Returns current platform time in seconds
 *
 */

extern UInt32 PFGetSeconds(void) PLATFORM_SECTION;

/*
 * PFGetTicks
 *
 */

extern UInt32 PFGetTicks(void) PLATFORM_SECTION;

#define PFTicksPerSecond SysTicksPerSecond

/*
 * PFTimerHasExpired
 *
 * Returns true if PFGetTicks() > (startTime + plusTicks)
 *
 */

extern Boolean PFTimerHasExpired(UInt32 startTime, UInt32 plusTicks) PLATFORM_SECTION;

extern UInt32 PFTimerTicksLeft(UInt32 startTime, UInt32 plusTicks) PLATFORM_SECTION;


/*
 * PFGetEvent
 *
 * Gets the next event from the OS, waiting up to 'wait' ticks. Pass zero if
 * you want to wait forever
 *
 * Returns false if no event was received.
 *
 */

extern Boolean PFGetEvent(EventPtr event, UInt32 maxTicks) PLATFORM_SECTION;

/*
 * Hook to the OS-level event handler. Pass a pointer to the application level event handler
 * and it will get called if the OS level hasn't handled the event.
 * 
 */

extern void PFHandleEvent(EventPtr event, Boolean (*appLevelHandler)(EventPtr)) PLATFORM_SECTION;

/*
 * Event Query functions
 * 
 */

#define PFEventGetType(evt) (evt->eType)
#define PFEventSetType(evt,type) (evt->eType = type)
#define PFEventGetKeyChr(evt) (evt->data.keyDown.chr)

#define PFEventGetControlSelectID(evt) (evt->data.ctlSelect.controlID)
#define PFEventSetControlSelectID(evt,id) (evt->data.ctlSelect.controlID = id)

#define PFEventGetX(evt) (evt->screenX)
#define PFEventGetY(evt) (evt->screenY)
#define PFEventIsDoubleTap(evt) (evt->tapCount > 1)
#define PFEventGetMenuID(evt) (evt->data.menu.itemID)
#define PFEventSetMenuID(evt, id) (evt->data.menu.itemID = id)

#define PFEventGetListSelection(evt) (evt->data.lstSelect.selection)
#define PFEventGetPopupSelection(evt) (evt->data.popSelect.selection)
#define PFEventGetPopupPriorSelection(evt) (evt->data.popSelect.priorSelection)
#define PFEventGetFormLoadID(evt) (evt->data.frmLoad.formID)

#define PFResetAutoOffTimer EvtResetAutoOffTimer

/*
 * PFEventSend
 *
 * Sends the specified event 
 *
 */

extern void PFEventSend(EventPtr event, Boolean unique) PLATFORM_SECTION;

extern void PFEventSendMenu(UInt16 menuID) PLATFORM_SECTION;

extern void PFEventSendKey(WChar chr) PLATFORM_SECTION;

#define PFSendSimpleEvent(e) { EventType evt; evt.eType=e; PFEventSend(&evt, false); }

extern Boolean PFEventAvailable(void) PLATFORM_SECTION;

/*
 * PFGetUserID
 *
 */

extern char *PFGetUserID(void) PLATFORM_SECTION;

/*
 *  PFGetSerialNumber
 *
 */

extern char *PFGetSerialNumber(void) PLATFORM_SECTION;


/*
 * Rectangle Functions
 * 
 */

extern void PFScreenRectangleSet(PFScreenRectType *r, Coord x1, Coord y1, Coord x2, Coord y2) PLATFORM_SECTION;
extern void PFScreenRectangleSetRel(PFScreenRectType *r, Coord x1, Coord y1, Coord width, Coord height) PLATFORM_SECTION;
extern void PFScreenRectangleInset(PFScreenRectType *r, Int16 insetBy) PLATFORM_SECTION;

extern Boolean PFScreenPointInRectangle(Coord x, Coord y, const PFScreenRectType *r) PLATFORM_SECTION;

#define PFGetRectangleWidth(r) ((r).x2 - (r).x1)
#define PFGetRectangleHeight(r) ((r).y2 - (r).y1)

#include "Logging.h"


/*
 * function : PceNativeResourceCall
 *
 * Executes the specified PNOlet, loaded as a resource and referenced
 * by resType/resID
 *
 */

UInt32 PceNativeResourceCall(DmResType resType, DmResID resID, char *DLLEntryPointP, void *userCPB)  UTILS_SECTION;

#define PFMemSet MemSet
#define PFMemMove MemMove

#endif
