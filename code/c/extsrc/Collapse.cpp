/*********************************************************************
 *
 Sample Code Disclaimer

 Copyright c 2002 PalmSource, Inc. or its subsidiaries. All rights
 reserved.

 You may incorporate this sample code (the "Code") into your
 applications for Palm OS(R) platform products and may use the Code to
 develop such applications without restriction. The Code is provided
 to you on an "AS IS" basis and the responsibility for its operation
 is 100% yours. PALMSOURCE, INC. AND ITS SUBSIDIARIES (COLLECTIVELY,
 "PALMSOURCE") DISCLAIM ALL WARRANTIES, TERMS AND CONDITIONS WITH
 RESPECT TO THE CODE, EXPRESS, IMPLIED, STATUTORY OR OTHERWISE,
 INCLUDING WARRANTIES, TERMS OR CONDITIONS OF MERCHANTABILITY, FITNESS
 FOR A PARTICULAR PURPOSE, NONINFRINGEMENT AND SATISFACTORY QUALITY.
 You are not permitted to redistribute the Code on a stand-alone basis
 and you may only redistribute the Code in object code form as
 incorporated into your applications. TO THE FULL EXTENT ALLOWED BY
 LAW, PALMSOURCE ALSO EXCLUDES ANY LIABILITY, WHETHER BASED IN
 CONTRACT OR TORT (INCLUDING NEGLIGENCE), FOR INCIDENTAL,
 CONSEQUENTIAL, INDIRECT, SPECIAL OR PUNITIVE DAMAGES OF ANY KIND, OR
 FOR LOSS OF REVENUE OR PROFITS, LOSS OF BUSINESS, LOSS OF INFORMATION
 OR DATA, OR OTHER FINANCIAL LOSS ARISING OUT OF OR IN CONNECTION WITH
 THE USE OR PERFORMANCE OF THE CODE. The Code is subject to Restricted
 Rights for U.S. government users and export regulations.

 SAMPLE NAME:  SampleCollapse

 FILE:         Collapse.c

 DATE:         July 2nd, 2003

 DESCRIPTION:  Source Code
 *
 *
 * NOTE: You will need the OS R3 SDk to compile this sample project
 ********************************************************************/


#include <PalmOS.h>
#include "CollapseRsc.h"
#include "CollapseUtils.h"

#pragma mark -
#pragma mark ==structs==
/***********************************************************************
 *
 *	Internal Structures
 *
 ***********************************************************************/
typedef struct 
	{
	UInt8 replaceme;
	} StarterPreferenceType;

typedef struct 
	{
	UInt8 replaceme;
	} StarterAppInfoType;

typedef StarterAppInfoType* StarterAppInfoPtr;

#pragma mark -
#pragma mark ==globals==
/***********************************************************************
 *
 *	Global variables
 *
 ***********************************************************************/
WinHandle g_offscreenGadgetBufWinH;


#pragma mark -
#pragma mark ==constants==
/***********************************************************************
 *
 *	Internal Constants
 *
 ***********************************************************************/
#define appFileCreator			'dsSO'	// register your own at http://www.palmos.com/dev/creatorid/
#define appVersionNum			0x01
#define appPrefID				0x00
#define appPrefVersionNum		0x01

// Define the minimum OS version we support (3.5 for now).
#define kOurMinVersion	sysMakeROMVersion(4,0,0,sysROMStageRelease,0)
#define kPalmOS10Version	sysMakeROMVersion(1,0,0,sysROMStageRelease,0)


/***********************************************************************
 *
 *	Internal Functions
 *
 ***********************************************************************/
static Boolean MainFormResize(FormPtr frmP);

#pragma mark -
/***********************************************************************
 *
 * FUNCTION:    RomVersionCompatible
 *
 * DESCRIPTION: This routine checks that a ROM version is meet your
 *              minimum requirement.
 *
 * PARAMETERS:  requiredVersion - minimum rom version required
 *                                (see sysFtrNumROMVersion in SystemMgr.h 
 *                                for format)
 *              launchFlags     - flags that indicate if the application 
 *                                UI is initialized.
 *
 * RETURNED:    error code or zero if rom is compatible
 *
 * REVISION HISTORY:
 *
 *
 ***********************************************************************/
static Err RomVersionCompatible(UInt32 requiredVersion, UInt16 launchFlags)
{
	UInt32 romVersion;

	// See if we're on in minimum required version of the ROM or later.
	FtrGet(sysFtrCreator, sysFtrNumROMVersion, &romVersion);
	if (romVersion < requiredVersion)
		{
		if ((launchFlags & (sysAppLaunchFlagNewGlobals | sysAppLaunchFlagUIApp)) ==
			(sysAppLaunchFlagNewGlobals | sysAppLaunchFlagUIApp))
			{
			FrmAlert (RomIncompatibleAlert);
		
			// Palm OS 1.0 will continuously relaunch this app unless we switch to 
			// another safe one.
			if (romVersion <= kPalmOS10Version)
				{
				AppLaunchWithCommand(sysFileCDefaultApp, sysAppLaunchCmdNormalLaunch, NULL);
				}
			}
		
		return sysErrRomIncompatible;
		}

	return errNone;
}





/***********************************************************************
 *
 * FUNCTION:    GetObjectPtr
 *
 * DESCRIPTION: This routine returns a pointer to an object in the current
 *              form.
 *
 * PARAMETERS:  formId - id of the form to display
 *
 * RETURNED:    void *
 *
 * REVISION HISTORY:
 *
 *
 ***********************************************************************/
static void * GetObjectPtr(UInt16 objectID)
{
	FormPtr frmP;

	frmP = FrmGetActiveForm();
	return FrmGetObjectPtr(frmP, FrmGetObjectIndex(frmP, objectID));
}


//////////////////////
//initialize the text in a field
//////////////////////
static Err SetTextPtr(FieldType* fldP, const Char* str, Boolean needToedraw)
{
	MemHandle textH = FldGetTextHandle(fldP);
	Char* str2;
	
	Err error;
	
	if (textH)
	{
		FldSetTextHandle(fldP, NULL);
		if ((error = MemHandleResize(textH, StrLen(str) + 1)) != 0)
		{
			FldSetTextHandle(fldP, textH);
			
			return error;
		}
		
	}
	else
	{
		textH = MemHandleNew(StrLen(str) + 1);
		if (!textH)
			return memErrNotEnoughSpace;
	
	}	
	
	str2 = (Char*) MemHandleLock(textH);
	StrCopy(str2, str);
	MemHandleUnlock(textH);
	
	FldSetTextHandle(fldP, textH);
	
	if(needToedraw)
		FldDrawField(fldP);
		
	return 0;
	
}

///////////////////
//Show an info style alert with given text
////////////////////
static void MyInfoAlert(const char* theText)
{
	//nothing special needs to be done to make use of FrmAlert or FrmCustomAlert
	//and collapsible input
	FrmCustomAlert(GenericInfoAlert, theText, NULL, NULL);
	
	//FrmAlert

}


#pragma mark -
/////////////////////
//
/////////////////
static Boolean DialogHandleEvent(EventPtr eventP)
{
	Boolean handled = false;
	FormType* frmP, *mainFrmP;
	Boolean needToRedraw;
	Int16 offsetX, offsetY;

	switch (eventP->eType)  {
		case frmOpenEvent:
			frmP = FrmGetActiveForm();
			
			CollapseSetState(frmP, collapseStateModalUser);
			needToRedraw = CollapseMoveResizeDialog(frmP, true, &offsetX, &offsetY);
			//CollapseResizeForm(frmP, true, &offsetX, &offsetY);
			
			/* initialize UI objects here */
			FrmDrawForm(frmP);
			handled = true;
			break;

		case ctlSelectEvent:
			//just return on either button being tapped
			FrmReturnToForm(0);
			handled = true;
			break;

		case winDisplayChangedEvent:
			//resize and move everything
			frmP = FrmGetActiveForm();

			needToRedraw = CollapseMoveResizeDialog(frmP, true, &offsetX, &offsetY);


			if (needToRedraw)
			{		
				if (CollapseGetAPIVersion() == sonyVersion1)
					FrmEraseForm(frmP);				
				
				//no need to adjust buttons because y extent is not changing
				
				mainFrmP = FrmGetFormPtr(MainForm);
				MainFormResize(mainFrmP);

				//the gadget handler requires the active form to be set
				FrmSetActiveForm(mainFrmP);
				
				if (CollapseGetAPIVersion() == sonyVersion1)
					FrmEraseForm(mainFrmP);
				
				FrmDrawForm(mainFrmP);
				FrmSetActiveForm(frmP);

				//the active window was unset when I drew the mainFrmP
				WinHandle winH = FrmGetWindowHandle(frmP); 
				WinSetActiveWindow(winH);
				FrmDrawForm(frmP);

				handled = true;
			}

			break;
		
		default:
			break;
	}

	return handled;
}


/////////////////////
//Instead of the full DialogHandleEvent you could use a partial event handler and
//set the dia state of the dialog up before 
/////////////////
static Boolean DialogPartialHandleEvent(EventPtr eventP)
{
	Boolean handled = false;
	FormType* frmP, *mainFrmP;
	Boolean needToRedraw;
	Int16 offsetX, offsetY;

	if (eventP->eType == winDisplayChangedEvent)
	{
	
		//resize and move everything
		frmP = FrmGetActiveForm();

		needToRedraw = CollapseMoveResizeDialog(frmP, true, &offsetX, &offsetY);


		if (needToRedraw)
		{		
			if (CollapseGetAPIVersion() == sonyVersion1)
				FrmEraseForm(frmP);				
			
			//no need to adjust buttons because y extent is not changing
			
			mainFrmP = FrmGetFormPtr(MainForm);
			MainFormResize(mainFrmP);

			//the gadget handler requires the active form to be set
			FrmSetActiveForm(mainFrmP);
			
			if (CollapseGetAPIVersion() == sonyVersion1)
				FrmEraseForm(mainFrmP);
			
			FrmDrawForm(mainFrmP);
			FrmSetActiveForm(frmP);

			//the active window was unset when I drew the mainFrmP
			WinHandle winH = FrmGetWindowHandle(frmP); 
			WinSetActiveWindow(winH);
			FrmDrawForm(frmP);

			handled = true;
		}
	}
	
	return handled;
	
}


///////////////////////////
//This is a fairly generic gadget handler. The gadget is part of this project because it represents 
//the type of object that might want to resize to take advantage of the extended sisze of the screen. 
//there is nothing in the gadget code here that is specific to the collapsible input area functionality.
//It just takes pen input and draws.
//////////////////////////

static Boolean TheGadgetHandler(FormGadgetTypeInCallback* pGadget,
  	UInt16 cmd, void* pParam)
{
  Boolean handled;
  Err error;
  FormType* frmP;
  WinHandle frmWinH;

  //   the gadget handler is called with one of 4 cmd:
  //   formGadgetDrawCmd to draw and initialize the gadget
  //   formGadgetHandleEventCmd in response to a gadget tap or an
  //	event
  //     keyed by your application
  //   formGadgetEraseCmd if there is any custom screen
  //	erasing that needs
  //     to be done when it is erased
  //   formGadgetDeletionCmd to clean up any memory
  //	used for the gadget

  //////////////////////////////////
  //use formGadgetDrawCmd as a chance to initialize, draw, or
  //redraw your gadget
  //////////////////////////////////
  switch (cmd)
  {
  case formGadgetDrawCmd:
     //initialize the gadget data or redraw the gadget
     
     WinDrawRectangleFrame(rectangleFrame, &(pGadget->rect));
     
     if (g_offscreenGadgetBufWinH)
     {
     	//set bounds to match current extent of gadget
     	RectangleType offscreenCopyBounds = {0, 0, pGadget->rect.extent.x, pGadget->rect.extent.y};
     	
     	frmP = FrmGetActiveForm();
     	frmWinH = FrmGetWindowHandle(frmP);
     	
     	WinCopyRectangle(g_offscreenGadgetBufWinH, frmWinH, &offscreenCopyBounds, 
     					pGadget->rect.topLeft.x, pGadget->rect.topLeft.y, winPaint);
     
     }
     
     handled = true;
     break;

     //////////////////////////////////
     //cmd is formGadgetHandleEventCmd when the gadget
     //is asked to handle an
     //event that has come in to it.Either it is a
     //response to a tap in the
     //gadgets bounds, or the program has sent something
     //over to be handled.
     //We can tell which one of these it is by looking at
     //the eType of pParam.
     //////////////////////////////////

  case formGadgetHandleEventCmd:
     //you need to cast pParam to an EventType* to use it
     EventType* pToEvent = (EventType*) pParam;

     //pToEvent points to the event we need to handle.
     //if it is eType
     //frmGadgetEnterEvent then there was a tap in the
     //gadget's bounds

     if (pToEvent->eType == frmGadgetEnterEvent)
     {
        //you may find it useful to track if they
        //lift the pen still
        //within the boundaries of the gadget
        Boolean isPenDown = true;
        Int16 newPointX, newPointY, oldPointX, oldPointY;
        
        
        EvtGetPen(&oldPointX, &oldPointY,
           	&isPenDown);
           	
        UInt16 ticksPerSec = SysTicksPerSecond();

        //track the pen down event
        while (isPenDown){
           EvtGetPen(&newPointX, &newPointY,
           	&isPenDown);
           
           //if both pen points are in thegadget rect, draw a line	
           if (RctPtInRectangle(oldPointX, oldPointY, &(pGadget->rect)) && 
           			RctPtInRectangle(newPointX, newPointY, &(pGadget->rect)))
           {
           		WinDrawLine(oldPointX, oldPointY, newPointX, newPointY);
           }
           
           oldPointX = newPointX;
           oldPointY = newPointY;

           //it is a good idea to put a delay such
           //that the system does
           //not need to fetch pen events more often
           //then need be
           SysTaskDelay(ticksPerSec / 10);
        }
        
        //note that the way I have done this, if the user draw in landscape
        //rotates the screen to portraite and draws again, the drawing 
        //on the right that was rotated out will be lost... This is a design decision,
        //you could instead create the buffer to be always as big left to right and top to bottom
        //as needed
        if (g_offscreenGadgetBufWinH)
        {
        	//delete old window
        	WinDeleteWindow(g_offscreenGadgetBufWinH, false);
        }
        
        g_offscreenGadgetBufWinH =  WinCreateOffscreenWindow (pGadget->rect.extent.x,
        							pGadget->rect.extent.y, nativeFormat, &error);
        							
        if (g_offscreenGadgetBufWinH)
        {
        	frmP = FrmGetActiveForm();
     		frmWinH = FrmGetWindowHandle(frmP);
     		
        	WinCopyRectangle (frmWinH, g_offscreenGadgetBufWinH, 
								&(pGadget->rect), 0, 0, winPaint);
        
        }

     }
     else if (pToEvent->eType == frmGadgetMiscEvent)
     {

     }

     handled = true;
     break;

  //////////////////////////////////
  //This is sent to erase the gadget. Generally,
  //FrmHideObject takes care of
  //hiding the object, so return false.
  ///////////////////////////////////
  case formGadgetEraseCmd:
     //erase the frame drawn around the gadget
     WinEraseRectangleFrame (rectangleFrame,
     	&(pGadget->rect));
     handled = false;
     break;

  ////////////////////////////////
  //When the gadget is being deleted... So clean up any
  //memory allocated.
  /////////////////////////////////
  case formGadgetDeleteCmd:

     handled = true;
     break;

  default:
     break;

  }//Switch(cmd)

  return handled;
}




/**************************
*Resizes the form and moves/resizes the form objects. Returns true if the form needed resizing.
***************************/
static Boolean MainFormResize(FormPtr frmP)
{
	Int16 horizontalOffset, verticalOffset;
	
	//resize the form. Note that this function returns the vert and horiz offsets the form was resized by
	Boolean formHasChanged = CollapseResizeForm(frmP, false, &horizontalOffset, &verticalOffset);
	
	if (formHasChanged)
	{
		//Vertically Move objects by offset
		//this is good for objects that are meant to be
		//"stuck" to the bottom of forms like buttons
		//These buttons are left justified so leave the horizontal offset alone
		//no resizing of button needed
		CollapseMoveResizeFormObject(frmP, MainGuessButton, 0, verticalOffset, 0, 0);
		CollapseMoveResizeFormObject(frmP, MainNoHandlerButton, 0, verticalOffset, 0, 0);
		CollapseMoveResizeFormObject(frmP, MainHandlerButton, 0, verticalOffset, 0, 0);

		//vertically and horizontally resize objects like gadgets, tables or lists that 
		//can take advantage of being larger
		//In this case the draw gadget will resize but not move.
		CollapseMoveResizeFormObject(frmP, MainDrawGadget, 0, 0, horizontalOffset, verticalOffset);
		
		//Some objects should be horizontally or vertically moved such that they stay centered on another 
		//form object. So move them by offset/2
		CollapseMoveResizeFormObject(frmP, MainDrawTextLabel, horizontalOffset/2, 0, 0, 0);
		
		//Form bitmaps need special handling because their bounds.extent will not change and
		//because trying to query their bounds before a form has draw will result in an error
		CollapseMoveFormBitmapObject(frmP, MainFaceBitMap, 0, verticalOffset/2);

		//Text fields will need to have their word wrapping recalculated after they are resized
		//For vertical resizing this is less important, but if a text field is resized horizontally
		//you will need to recalculate the word wrapping.
		CollapseMoveResizeFormObject(frmP, MainAtTheTopField, 0, 0, horizontalOffset, 0);
		FldRecalculateField((FieldType*)FrmGetObjectPtr(frmP, FrmGetObjectIndex(frmP, MainAtTheTopField)), 
							false);
		
		
	}

	return formHasChanged;
}





/***********************************************************************
 *
 * FUNCTION:    MainFormInit
 *
 * DESCRIPTION: This routine initializes the MainForm form.
 *
 * PARAMETERS:  frm - pointer to the MainForm form.
 *
 * RETURNED:    nothing
 *
 * REVISION HISTORY:
 *
 *
 ***********************************************************************/
static void MainFormInit(FormPtr frmP)
{
	//Set some default text for the field to test word wrapping.
	SetTextPtr((FieldType*) GetObjectPtr(MainAtTheTopField), "This is some sample text that should need to word wrap", false);

	MainFormResize(frmP);
}


/***********************************************************************
 *
 * FUNCTION:    MainFormDoCommand
 *
 * DESCRIPTION: This routine performs the menu command specified.
 *
 * PARAMETERS:  command  - menu item id
 *
 * RETURNED:    nothing
 *
 * REVISION HISTORY:
 *
 *
 ***********************************************************************/
static Boolean MainFormDoCommand(UInt16 command)
{
	Boolean handled = false;
	FormPtr frmP;

	switch (command)
		{
		case MainOptionsAboutStarterApp:
			MenuEraseStatus(0);					// Clear the menu status from the display.
			frmP = FrmInitForm (AboutForm);
			FrmDoDialog (frmP);					// Display the About Box.
			FrmDeleteForm (frmP);
			handled = true;
			break;

		}
	
	return handled;
}


/***********************************************************************
 *
 * FUNCTION:    MainFormHandleEvent
 *
 * DESCRIPTION: This routine is the event handler for the 
 *              "MainForm" of this application.
 *
 * PARAMETERS:  eventP  - a pointer to an EventType structure
 *
 * RETURNED:    true if the event has handle and should not be passed
 *              to a higher level handler.
 *
 * REVISION HISTORY:
 *
 *
 ***********************************************************************/
static Boolean MainFormHandleEvent(EventPtr eventP)
{
	Boolean handled = false;
	Boolean needToRedraw;
	FormPtr frmP;
	Int16 offsetX, offsetY;
	UInt16 oldPinTriggerState;
	UInt16 whichButton;
	

	switch (eventP->eType) 
		{
		case menuEvent:
			return MainFormDoCommand(eventP->data.menu.itemID);

		case frmOpenEvent:
			frmP = FrmGetActiveForm();
			
			//set the form to handle collapsible input area
			CollapseSetState(frmP, collapseStateUser);
			//CollapseSetState(frmP, collapseStateDown);
		
			MainFormInit( frmP);
			FrmDrawForm ( frmP);
			handled = true;
			break;
			
		case ctlSelectEvent:
			switch (eventP->data.ctlSelect.controlID)
				{
				case MainGuessButton:
					//Alerts are positioned and handled by the OS. Some implementations will put 
					//them flush with the bottom if the input area is down, others will float it up in
					//the square screen space or bring the input area up. If you need control of 
					//this behaivor you need to use a Modal Dialog instead (see below)
					
					MyInfoAlert("I think it looks like a fish riding a unicycle");
					
					//StatShow();
					 
					
					break;
				
				
				case MainNoHandlerButton:
					frmP = FrmInitForm(ModalDialogForm);
					
					//Dialogs can be initialized to take advantage of the the collapsed 
					//input area much the same way normal forms are

					//If you do nothing the PalmSource platform API will put the dialog into legacy 
					//mode. Meaning it will bring up the input are and lock the trigger.
					
					CollapseSetState(frmP, collapseStateModalUser);
					CollapseMoveResizeDialog(frmP, true, &offsetX, &offsetY);
					
					//When placing the dialog at the bottom of the screen lock the 
					//trigger unless the dialog has its own form handler (see below). If you do use a 
					//form handler for the dialog you can move the dialog up and down in the
					//same way you would resize a form. However, by default, 
					//if you are calling FrmDoDialog and the input area trigger is left unlocked,
					//the input area could be raised to cover up your dialog.
					oldPinTriggerState = CollapseSetTriggerStates(pinInputTriggerDisabled);
					
			      	// open the dialog, and wait until a button is pressed to close it.
			       	whichButton = FrmDoDialog(frmP);
					
					//return the old trigger state 
					CollapseSetTriggerStates(oldPinTriggerState);
					
			       	FrmDeleteForm(frmP);
			       	handled=true;					
					break;
				
				case MainHandlerButton:
					//you can use FrmPopupForm and provide an event handler that will take care of 
					//resizing realligning the modal dialog  
					frmP = FrmGetActiveForm();
					FrmPopupForm(ModalDialogForm);
			       	
			       	handled=true;
			      
					break;
				
				default:
					break;
				}
			
			break;
			
		case frmUpdateEvent:
			
			break;
			
			
		case winDisplayChangedEvent:
		
			//resize and move everything
			frmP = FrmGetActiveForm();
			needToRedraw = MainFormResize(frmP);
			
			//Redraw if there was a change in the form 
			//It is important to make sure a redraw is needed because I post
			//a winDisplayChangedEvent back to the form in response to a winEnterEvent
			//(see AppEventLoop below). If I simply always redraw it would cause problems 
			//with menus 
			if (needToRedraw || CollapseCheckForPin10NeedToRedraw())
			{
				//On the OS 4 Sony devices you have to erase the form before you redraw it
				if (CollapseGetAPIVersion() == sonyVersion1)
					FrmEraseForm(frmP);
					
				FrmDrawForm(frmP);
			}
			break;

		default:
			break;
		
		}
	
	return handled;
}

#pragma mark -
/***********************************************************************
 *
 * FUNCTION:    AppHandleEvent
 *
 * DESCRIPTION: This routine loads form resources and set the event
 *              handler for the form loaded.
 *
 * PARAMETERS:  event  - a pointer to an EventType structure
 *
 * RETURNED:    true if the event has handle and should not be passed
 *              to a higher level handler.
 *
 * REVISION HISTORY:
 *
 *
 ***********************************************************************/
static Boolean AppHandleEvent(EventPtr eventP)
{
	UInt16 formId;
	FormPtr frmP;

	if (eventP->eType == frmLoadEvent)
		{
		// Load the form resource.
		formId = eventP->data.frmLoad.formID;
		frmP = FrmInitForm(formId);
		FrmSetActiveForm(frmP);


		// Set the event handler for the form.  The handler of the currently
		// active form is called by FrmHandleEvent each time is receives an
		// event.
		switch (formId)
			{
			case MainForm:
			
				//set the gadget handler for the form
  				FrmSetGadgetHandler(frmP, FrmGetObjectIndex(frmP, MainDrawGadget), TheGadgetHandler);
  
  				//set the form handler
				FrmSetEventHandler(frmP, MainFormHandleEvent);
				break;

			case ModalDialogForm:
				FrmSetEventHandler(frmP, DialogHandleEvent);
				break;
			
			default:
//				ErrFatalDisplay("Invalid Form Load Event");
				break;

			}
		return true;
		}
	
	return false;
}


/***********************************************************************
 *
 * FUNCTION:    AppEventLoop
 *
 * DESCRIPTION: This routine is the event loop for the application.  
 *
 * PARAMETERS:  nothing
 *
 * RETURNED:    nothing
 *
 * REVISION HISTORY:
 *
 *
 ***********************************************************************/
static void AppEventLoop(void)
{
	UInt16 error;
	EventType event;

	do {
		EvtGetEvent(&event, evtWaitForever);

		CollapseCheckWinEnterEvent(&event);
		
		if (! SysHandleEvent(&event))
			if (! MenuHandleEvent(0, &event, &error))
				if (! AppHandleEvent(&event))
					FrmDispatchEvent(&event);

	} while (event.eType != appStopEvent);
}


/***********************************************************************
 *
 * FUNCTION:     AppStart
 *
 * DESCRIPTION:  Get the current application's preferences.
 *
 * PARAMETERS:   nothing
 *
 * RETURNED:     Err value 0 if nothing went wrong
 *
 * REVISION HISTORY:
 *
 *
 ***********************************************************************/
static Err AppStart(void)
{
	StarterPreferenceType prefs;
	UInt16 prefsSize;
	Int32 collapseVersion;
	

	// Read the saved preferences / saved-state information.
	prefsSize = sizeof(StarterPreferenceType);
	if (PrefGetAppPreferences(appFileCreator, appPrefID, &prefs, &prefsSize, true) != 
		noPreferenceFound)
		{
		}
	


	//make sure we are running on a system with the PinMgr
	CollapseAppStart();
	
	//check for existence of some collapse API... this call must be made after CollapseAppStart to be valid
	collapseVersion = CollapseGetAPIVersion();
	
	if (!collapseVersion) 
	{
		MyInfoAlert("Can not run on system with no collapsible input area support");
		return errNone;
	}
	
	//buffer for gadget				
	g_offscreenGadgetBufWinH = NULL;
	
	return errNone;
}


/***********************************************************************
 *
 * FUNCTION:    AppStop
 *
 * DESCRIPTION: Save the current state of the application.
 *
 * PARAMETERS:  nothing
 *
 * RETURNED:    nothing
 *
 * REVISION HISTORY:
 *
 *
 ***********************************************************************/
static void AppStop(void)
{
	StarterPreferenceType prefs;
	UInt16 cardNo;
	LocalID dbID;
	Err error;

	// Write the saved preferences / saved-state information.  This data 
	// will saved during a HotSync backup.
	PrefSetAppPreferences (appFileCreator, appPrefID, appPrefVersionNum, 
		&prefs, sizeof (prefs), true);
		
		
	// Close all the open forms.
	FrmCloseAllForms ();

	//unregister for the notification sent when the the win display changes size
	error = SysCurAppDatabase( &cardNo, &dbID );
    ErrFatalDisplayIf( ( error != errNone ), "can't get app db info" );
    
    if (g_offscreenGadgetBufWinH)
    {
    	//delete old window
    	WinDeleteWindow(g_offscreenGadgetBufWinH, false);
    }
    
    CollapseAppStop();
   

}


/***********************************************************************
 *
 * FUNCTION:    StarterPalmMain
 *
 * DESCRIPTION: This is the main entry point for the application.
 *
 * PARAMETERS:  cmd - word value specifying the launch code. 
 *              cmdPB - pointer to a structure that is associated with the launch code. 
 *              launchFlags -  word value providing extra information about the launch.
 *
 * RETURNED:    Result of launch
 *
 * REVISION HISTORY:
 *
 *
 ***********************************************************************/
static UInt32 StarterPalmMain(UInt16 cmd, MemPtr /*cmdPBP*/, UInt16 launchFlags)
{
	Err error;

	error = RomVersionCompatible (kOurMinVersion, launchFlags);
	if (error) return (error);
		
	
	switch (cmd)
		{
		case sysAppLaunchCmdNormalLaunch:
			
			error = AppStart();
			if (error) 
				return error;
				
			FrmGotoForm(MainForm);
			AppEventLoop();
			AppStop();
			break;

		default:
			break;

		}
	
	return errNone;
}


/***********************************************************************
 *
 * FUNCTION:    PilotMain
 *
 * DESCRIPTION: This is the main entry point for the application.
 *
 * PARAMETERS:  cmd - word value specifying the launch code. 
 *              cmdPB - pointer to a structure that is associated with the launch code. 
 *              launchFlags -  word value providing extra information about the launch.
 * RETURNED:    Result of launch
 *
 * REVISION HISTORY:
 *
 *
 ***********************************************************************/
UInt32 PilotMain( UInt16 cmd, MemPtr cmdPBP, UInt16 launchFlags)
{
	return StarterPalmMain(cmd, cmdPBP, launchFlags);
}
