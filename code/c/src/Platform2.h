#ifndef PLATFORM2_H_
#define PLATFORM2_H_

typedef signed char		Int8;
typedef signed short	Int16;	
typedef signed long		Int32;

typedef unsigned char	UInt8;
typedef unsigned short  UInt16;
typedef unsigned long   UInt32;


typedef unsigned char	Boolean;

typedef char				Char;
typedef UInt16				WChar;		// 'wide' int'l character type.
typedef UInt16				Err;
typedef UInt32				LocalID;		// local (card relative) chunk ID
typedef Int16 				Coord;		// screen/window coordinate

typedef struct RGBColorType
{
	UInt8           index;					// index of color or best match to cur CLUT or unused.
	UInt8           r;						// amount of red, 0->255
	UInt8           g;						// amount of green, 0->255
	UInt8           b;						// amount of blue, 0->255
}
RGBColorType;

typedef enum {
	nilEvent = 0,				// system level
	penDownEvent,				// system level
	penUpEvent,					// system level
	penMoveEvent,				// system level
	keyDownEvent,				// system level
	winEnterEvent,				// system level
	winExitEvent,				// system level
	ctlEnterEvent,
	ctlExitEvent,
	ctlSelectEvent,
	ctlRepeatEvent,
	lstEnterEvent,
	lstSelectEvent,
	lstExitEvent,
	popSelectEvent,
	fldEnterEvent,
	fldHeightChangedEvent,
	fldChangedEvent,
	tblEnterEvent,
	tblSelectEvent,
	daySelectEvent,
	menuEvent,
	appStopEvent = 22,			// system level
	frmLoadEvent,
	frmOpenEvent,
	frmGotoEvent,
	frmUpdateEvent,
	frmSaveEvent,
	frmCloseEvent,
	frmTitleEnterEvent,
	frmTitleSelectEvent,
	tblExitEvent,
	sclEnterEvent,
	sclExitEvent,
	sclRepeatEvent,
	tsmConfirmEvent = 35,		// system level
	tsmFepButtonEvent,			// system level
	tsmFepModeEvent,				// system level
	attnIndicatorEnterEvent,	// for attention manager's indicator
	attnIndicatorSelectEvent,	// for attention manager's indicator
	
	// add future UI level events in this numeric space
	// to save room for new system level events
	menuCmdBarOpenEvent = 0x0800,
	menuOpenEvent,
	menuCloseEvent,
	frmGadgetEnterEvent,
	frmGadgetMiscEvent,

	// <chg 2-25-98 RM> Equates added for library events
	firstINetLibEvent = 0x1000,
	firstWebLibEvent = 0x1100,
	
	// GFa, 07/20/01 : integrated the bellagio telephony events.
	telAsyncReplyEvent = 0x1200, 

	// Can't add these to the system event range because PACE won't pass them through,
	// add them to the licensee range here:
	keyUpEvent 			= 0x4000,
	keyHoldEvent 		= 0x4001,
	frmObjectFocusTakeEvent 	= 0x4002,
	frmObjectFocusLostEvent 	= 0x4003,

	// winDisplayChangedEvent	= 0x4101,	// defined below for compatibility

	// BGT, 06/24/2003 Clarify the range reserved for licensees
	firstLicenseeEvent	= 0x5000,
	lastLicenseeEvent	= 0x5FFF,

	// <chg 10/9/98 SCL> Changed firstUserEvent from 32767 (0x7FFF) to 0x6000
	// Enums are signed ints, so 32767 technically only allowed for ONE event.
	firstUserEvent = 0x6000,
	
	lastUserEvent  = 0x7FFF
} eventsEnum;


typedef UInt32 DmOpenRef;

typedef UInt32 WinHandle;

#endif /*PLATFORM2_H_*/
