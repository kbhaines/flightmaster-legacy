-- |A bare-metal window manager, part of OpenGLINT. The manager draws windows
-- into GraphicsCmd's as implemented in the OpenGLINT graphics-interface layer:
-- at the moment that layer only translates to OpenGL commands. 

module Window (

    -- * Functions
    -- ** Window creation
    newWindow

    -- ** Message functions
  , sendMessage
  , sendMessageTo

    -- ** Functions for drawing and selecting
  , draw
  , pickWindows

    -- ** Functions for querying and updating window states
  , readPublicVariable
  , updatePublicVariable

  , displayToggleHidden
  , displayToggleDisabled

  , position
  , dimensions
  , resize

    -- * Types
    -- ** Windows and their attributes
  , Window(..)

  , ID(..)
  , Title
  , Bounds(..)
  , Coordinate
  , WindowState(..)
  , Style(..)

    -- ** Messaging types
  , MessageHandler
  , QueryFunction
  , StateHandle

  , Message(..)
  , MessageAddress(..)
  , MessageBody(..)

  , Response(..)
  , SyncMessage(..)

    -- * Miscellaneous types/functions
  , framedStyle
  , expandWithChildren

    -- * User Guide
    -- $UserGuide

    -- ** Overview
    -- ** Creating a window heirarchy
    -- ** Message Handling
    -- $MessageHandling

    -- ** Global/shared variables
 
) where


import Graphics
import Data.Maybe
import Control.Monad.State
import Data.List

import WindowMessage

-- |Title for windows
type Title = String

-- |Coordindates for positioning and dimensioning windows
type Coordinate = Graphics2d

--------------------------------------------------------------------------------
-- |A Window represents a rectangular region of the screen

data Window = Window {

    -- NB
    -- I was tempted to parameterise Window, however that means that all
    -- instances in a list would have to have the same type, so you couldn't
    -- mix different state types.  Instead, the approach taken here is to use
    -- partial application in the handleMessage function.
    --
    -- This frees us having to parameterise the type, which would lead to
    -- limitations in the way lists of windows can be used.

    -- |Window's unique id
    ident :: ID

    -- |Title of the window
  , title :: Title

    -- |List of style attributes controlling the base appearance of the window 
  , style :: [Style]

    -- |Window position and dimensions
  , bounds :: Bounds

    -- |List of child windows 
  , children :: [Window]

    -- |Current draw/enable status of the window
  , displayState  :: WindowState

    -- |The message handling function for the window. By using a partially-applied
    -- function you can make your window have locally-available configuration 
    -- information
  , handleMessage :: MessageHandler

}

instance Eq Window where
    a == b = (ident a == ident b)

instance Show Window where
    show w = show (ident w) ++ show (title w) ++ show (bounds w) ++ show (children w)


-- : Change to Graphics.Position types
-- window rectangle bounds. Comprised (x,y),(w,h)

-- |Bounding box of window: (x,y) and (w,h)
newtype Bounds = Bounds (Coordinate,Coordinate)
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- |Windows can be in one of these states. 'Enabled' will draw the window, 'Disabled'
-- will still draw the window, but it will not respond to click-events. 'Hidden' ensures
-- the window is not drawn, nor responds to click events.

data WindowState = Enabled | Disabled | Hidden
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- |Base styles that can be applied to windows, also including attributes that control
-- the window's dimensions

data Style = 

    -- |For future enhancement
    CustomStyle (Coordinate -> [GraphicsCmd])

    -- |Background fill for the entire window area
  | Background GraphicsColour

    -- |Frame around the window, in specific colour and width
  | Frame GraphicsColour GraphicsFloat

    -- Rounded frame around the window. Not implemented
    -- RoundedFrame GraphicsColour GraphicsFloat GraphicsFloat

    -- |Lock the edges of the window to top, bottom, left and right
  | LockTop Graphics1d
  | LockBottom Graphics1d
  | LockLeft Graphics1d
  | LockRight Graphics1d

    -- |Window will fill the area bounded by its parent
  | FillParent

--------------------------------------------------------------------------------
-- |MessageAddress specifies where to send a specific message to. 

data MessageAddress = All | First | One ID
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- |ID is used to identify each window

newtype ID = ID Int deriving (Eq,Show)


-- |A message handler takes a window and message and returns a response to the
--message. The message handler is usually a partially-applied function that has
--a parameter representing state of the window. 

type MessageHandler = (Window -> MessageBody -> QueryFunction -> Response)

--------------------------------------------------------------------------------
-- |A QueryFunction is passed to windows during a message-send to allow them to query
-- the state of other windows

type QueryFunction = (ID -> StateHandle -> StateValue)

--------------------------------------------------------------------------------
-- |A StateHandle is a unique identifier used by each window to refer to all or
-- part of the state of the window; i.e. its interpretation is window-specific.
-- For example, a window can use the value '0' to mean its complete state, or
-- just a part of that state

type StateHandle = Int

--------------------------------------------------------------------------------
-- |A Message combines an address and body to enable a message to be sent
-- to specific addresses, which may be a specified window, or a set of windows,
-- or all windows

data Message = Message MessageAddress MessageBody

--------------------------------------------------------------------------------
-- |The body of a message to be sent to a window

data MessageBody = 

    -- |Mouse click at a position, and a list of OpenGL object IDs that were underneath
    LeftButtonDown GraphicsPosition [GraphicsName]

    -- |Mouse-down drag events with delta and absolute coordinates of movement
  | LeftDrag GraphicsPosition -- ^Delta of movement
             GraphicsPosition -- ^Absolute position of cursor

    -- |Request to redraw the window
  | Draw

    -- |Request for the window to return all, or part, of its state (as a StateValue response)
  | StateQuery StateHandle

    -- |Notification to the window that another window has updated its internal state
    -- ??? Remove?
  | StateUpdateNotify ID (Maybe SyncMessage)

    -- |Keypressed in the window
  | Key Char

    -- |The main application window has been resized to the specified dimensions
  | AppWindowResize Coordinate

    -- |The parent of the window has been resized to the specified dimensions
    -- ???
  | ParentResize Coordinate

    -- |A request for the window to move itself to the new coordinates
  | MoveWindow Coordinate

    -- |A request for the window to resize itself
  | ResizeWindow Coordinate

  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- |A Response is reported by a window's message handler after it has processed
-- a message sent to it.

data Response = 

    -- |The window updated itself (usually: change to the handleMessage function). The
    -- update includes the new window itself. If the SyncMessage is something, then
    -- this response may trigger further updates in response to the update of 
    -- the window that has just happened (see 'sendMessage')
    Update Window (Maybe SyncMessage) 

    -- |The window updated itself, but no follow-up broadcast to other windows should occur
  | UpdatePrivate Window

    -- |Graphics commands to draw the window (response to Draw message)
  | DrawCmds [GraphicsCmd]

    -- |Response to a state enquiry, see below
  | Response StateValue

    -- |Window ignored message
  | NoResponse

--------------------------------------------------------------------------------
-- |A SyncMessage can be sent out by a window in an 'Update' response to a Message,
-- in order to tell the rest of the windows that its state has been updated, or
-- it has been clicked.

data SyncMessage = 
    -- |The window responded to a click event
    Clicked
 
    -- |The window updated one of its internal variables
  | SyncVar (Int, StateValue)

    deriving (Eq,Show)

--------------------------------------------------------------------------------
-- |Shortcut to define a window with a white background and a black, single pixel frame
-- around it
framedStyle :: [Style]
framedStyle = [Background $ GraphicsColour3 1 1 1, Frame (GraphicsColour3 0 0 0) 1.0 ]

-------------------------------------------------------------------------------
-- |Get the position (x,y) of the window
position :: Window -> Coordinate
position w = let Bounds (pos,_) = bounds w in pos

-------------------------------------------------------------------------------
-- |Get the dimensions (w,h) of the window
dimensions :: Window -> Coordinate
dimensions w = let Bounds (_,dim) = bounds w in dim

-------------------------------------------------------------------------------
-- |Flatten the window and children into a linear list 

expandWithChildren :: Window -> [Window]
expandWithChildren topWin = topWin : concatMap (expandWithChildren) (children topWin)

-------------------------------------------------------------------------------
-- Apply the function to the window and all child windows
-- If you ever use this function, re-write to apply f to window first, then 
-- to the children of the *resulting* window.
--
--winMap :: (Window -> Window) -> Window -> Window
--winMap f win = f win { children = map (winMap f) (children win) }


-------------------------------------------------------------------------------
-- |Create a new window. 

newWindow :: ID         -- ^Unique ID of the window
          -> [Window]   -- ^List of child windows
          -> Title      -- ^Title of window
          -> [Style]    -- ^Display attributes of window
          -> Bounds     -- ^Window position and dimensions
          -> Maybe MessageHandler  -- ^Message handling function
          -> Window

newWindow winId childWindows title style pos (Just eventFunc) =
    Window winId title style pos childWindows Enabled eventFunc

-- create a window with a dummy event handler function
newWindow winId childWindows title style pos Nothing =
    Window winId title style pos childWindows Enabled (\_ _ _ -> NoResponse)

-------------------------------------------------------------------------------
-- |Calculate drawing commands to draw the window with extra adornments (furniture)
-- based on the styles list for the window

drawWithFurniture:: Window -> [GraphicsCmd] -> [GraphicsCmd]
drawWithFurniture win@(Window _ _ styles _ _ _ _) userCmds = 
  (concatMap (drawStyle (dimensions win)) styles) ++ userCmds
  where
    drawStyle :: Coordinate -> Style -> [GraphicsCmd]
    drawStyle (width,height) (Background colour) = [ Polygon colour 1 Nothing 
        (UntexturedPolygon $ map (pointToPlane (-1) ) [(0,0), (width,0), (width,height),(0,height)])]

    drawStyle (width,height) (Frame colour fw) = [Polyline colour fw Nothing $ map (pointToPlane 0) 
        [(0,0), (width,0), (width,height),(0,height),(0,0)]]

    drawStyle _ _ = []

-------------------------------------------------------------------------------
-- |Move the window to the new coordinates
move :: Window -> Coordinate -> Window
move win newPos = win { bounds = Bounds (newPos,curSize) }
    where curSize = dimensions win

-------------------------------------------------------------------------------
-- |Resize the window to the new dimensions

resize :: Window -> Coordinate -> Window
resize win newsize = win { bounds = Bounds (position win,newsize) }

-------------------------------------------------------------------------------
-- |Message handler to respond to parent-resize messages

handleParentResizeMessage :: Coordinate -> Window -> Window
handleParentResizeMessage newParentSize win = updatedWindow  { children = resizedChildren }
  where
    updatedWindow = setAutoPosition win newParentSize
    resizedChildren = map (handleParentResizeMessage $ dimensions updatedWindow) (children win)

-------------------------------------------------------------------------------
-- |This function is passed into message handlers to allow windows to query the
-- state values of other windows, by sending StateHandle messages to them.

queryFunction :: Window -> ID -> StateHandle -> StateValue
queryFunction topWin id request = 
    case sendMessageTo topWin id (StateQuery request) of
      (Response x) -> x
      _ -> error ("No response from " ++ show id)

-------------------------------------------------------------------------------
-- |Returns the value of the specified public variable from the window (or
-- child of the window). It's up to the message handler of the target window
-- to interpret the 'StateHandle' value and return the correct value.

readPublicVariable :: Window -> ID -> StateHandle -> StateValue
readPublicVariable topWin id hdl = queryFunction topWin id hdl

-------------------------------------------------------------------------------
-- |Update the specified global variable in the specified window

updatePublicVariable :: Window -> ID -> Int -> StateValue -> Window
updatePublicVariable topWin sourceWindowID varId value = 
    sendMessage topWin $ Message All (StateUpdateNotify sourceWindowID (Just $ SyncVar (varId, value)))

-------------------------------------------------------------------------------
-- |Send the message to the specified window. Not really recommended, you should
-- use 'sendMessage' with a correctly addressed message instead, but it's useful
-- if you need a 'Response' instead of an updated 'Window'.

sendMessageTo :: Window -> ID -> MessageBody -> Response
sendMessageTo topWin winId msg = 
    let win = find (\w -> ident w == winId) (expandWithChildren topWin)
        win' = fromJust win in
    if (not $ isNothing win) then 
        (handleMessage win') win' msg (queryFunction topWin)
      else
        error ("Invalid window specified "++ show winId ++ " " ++ show (map ident (expandWithChildren topWin)))

-------------------------------------------------------------------------------
-- |Send the message to the window and its children, in effect invoking
-- the messageHandler function of each window with the body of the message
-- supplied in the parameter.
--
-- This function is the main entry point into the window system. Messages should
-- be created by external processes (such as a GLUT-driven program) and sent
-- to the top-level window. The messages will be forwarded to child windows
-- as appropriate, and any child-responses that need attention will be further
-- processed, before finally returning a new top-level window

sendMessage :: Window -> Message -> Window

-- send broadcast of application window resize to all windows

sendMessage topWin (Message All (AppWindowResize (w,h))) = handleParentResizeMessage (w,h) topWin
sendMessage _ (Message _ (AppWindowResize _)) = do error "AppWindowResize must go to All"

-- send message to all windows, only updating the first that responds

sendMessage topWin (Message First body) = sendMessage topWin (Message (One targetWinId) body)
 where
    -- default to the top-level window if no other windows respond to
    -- the event
    targetWinId = if responders == [] then ID 0 else head responders
    responders = map ident $ filter (\w -> isUpdate $ (handleMessage w) w body (queryFunction topWin)) (expandWithChildren topWin) 
    isUpdate resp = case resp of
                        (Update _ _) -> True
                        (UpdatePrivate _) -> True
                        _ -> False

-- send message to all windows, gather any state-update-notify messages (via
-- State monad) and recursively send them to all windows

sendMessage topWin (Message All body) = applyAllMessages
  where
    (newWin, msgs) = runState (winMapM (applyMessage body (queryFunction topWin)) topWin) []
    applyAllMessages 
      | msgs /= [] = foldl sendMessage newWin (map (Message All) msgs)
      | otherwise  = newWin


-- send message to a specific window, and a StateUpdateNotify about the
-- affected window to all windows

sendMessage topWin (Message (One winId) body) = final
  where
    (intermed, msgs) = runState (winMapM (testFunc winId) topWin) []
    final 
      | msgs /= [] = foldl sendMessage intermed (map (Message All) msgs)
      | otherwise  = intermed

    testFunc :: ID -> Window -> State [MessageBody] Window
    testFunc id w
     | id == ident w = applyMessage body (queryFunction topWin) w
     | otherwise = return $ w



--------------------------------------------------------------------------------
-- |Apply a function to the window and all of its child windows, under 
-- control of a monad (probably StateMonad - the StateMonad allows the 
-- child windows to respond to the events, and for a list of such responses to 
-- be built as each window responds.

-- : BUGWATCH: Window problems caused by messages not making it out?

winMapM :: (Monad m) => (Window -> m Window) -> Window -> m Window
winMapM func topWin = do
    -- apply the function to the top window...
    topWin' <- func topWin

    -- ...then to the children of the *new* top window
    newChildren <- mapM (winMapM func) (children topWin')

    return $ topWin' { children = newChildren }

-------------------------------------------------------------------------------
-- |Apply the supplied message to the window, updating the StateMonad with any
-- responses that the window generates. Returns a new (updated) window

applyMessage :: MessageBody -> QueryFunction -> Window -> State [MessageBody] Window

-- don't update self in response to this message!
applyMessage (StateUpdateNotify updatedId _) _ win 
  | updatedId == ident win = return win

applyMessage body qf win = do
    case (handleMessage win) win body qf of
        Update newWin newState -> do
            msgs <- get
            put (StateUpdateNotify (ident win) newState:msgs )
            return newWin

        UpdatePrivate newWin -> return newWin

        NoResponse -> return win

        _ -> error "Invalid window response, expected update"


-------------------------------------------------------------------------------
-- |Computes the position of the window, based on elements contained in the
-- window's style setting

setAutoPosition :: Window -> Coordinate -> Window
setAutoPosition win (parentw,parenth) = win { bounds = newPosition}
  where 
    newPosition = Bounds ((foldr (flip foldXY) (oldx,oldy) styles), (foldr (flip foldWH) (myWidth,myHeight) styles))
    (oldx,oldy) = position win
    (myWidth, myHeight) = dimensions win
    styles = style win

    -- written for foldl, hence use of 'flip' above when using foldr
    foldXY :: Coordinate -> Style -> Coordinate
    foldXY _ (FillParent) = (0,0)
    foldXY (x,_) (LockTop n)  = (x,n)
    foldXY (_,y) (LockLeft n)  = (n,y) 
    foldXY (x,_) (LockBottom n) = (x,parenth-n-myHeight)
    foldXY (_,y) (LockRight n)  = (parentw-n-myWidth,y)
    foldXY xy _ = xy

    foldWH :: Coordinate -> Style -> Coordinate
    foldWH _ (FillParent) = (parentw, parenth)
    foldWH wh _ = wh 

-------------------------------------------------------------------------------
-- |Given a window and a point, calculates ID of windows containing
-- the point, and the relative position within each window of the point. Returns
-- the results in reverse order, i.e. the deepest-level window is at the front
-- of the list.

pickWindows :: Window -> GraphicsPosition -> [(ID,Coordinate)]
pickWindows topWin pos = reverse $ scanWindows pos topWin
    where 
        -- if the point is in the window, it is added to the results list along
        -- with the results of the children-scan. Otherwise, just the 
        -- results of the children-scan.
        scanWindows :: GraphicsPosition -> Window -> [(ID,Coordinate)]
        scanWindows pos w
         | displayState w /= Enabled = []
         | ptInWindow pos w = (ident w, toWinCoords pos w):childrenScan
         | otherwise = childrenScan
         where subPos = shiftTestPt pos w
               childrenScan = concatMap (scanWindows subPos) (children w)

        -- calculate window-relative coordinates of position
        toWinCoords :: GraphicsPosition -> Window -> Coordinate
        toWinCoords (GraphicsPosition (px,py)) win = (fromIntegral px - winx, fromIntegral py - winy)
          where
            (winx, winy) = position win

        -- test if the point is inside the window
        ptInWindow :: GraphicsPosition -> Window -> Bool
        ptInWindow (GraphicsPosition (px,py)) win = (boundTest (fromIntegral px - winx) winw) && (boundTest (fromIntegral py - winy) winh)
          where
            (winx,winy) = position win
            (winw,winh) = dimensions win
            boundTest a1 a2 = (a1 >=0 && a1 < a2)

        -- shift the specified point relative to the window position
        shiftTestPt (GraphicsPosition (px,py)) win = (GraphicsPosition (px-winx, py-winy))
         where (winx', winy') = position win
               (winx,winy) = (truncate winx', truncate winy')

-------------------------------------------------------------------------------
-- |Compute GraphicsCmds for the window, and all of its children

draw :: Window -> [GraphicsCmd]
draw topWin = [Scale 1.0 (-1.0) 1.0 $ redrawWindow topWin]
    where
        redrawWindow :: Window -> [GraphicsCmd]
        redrawWindow w 
          | displayState w /= Hidden = [RelTranslate (x,y,0) $ (drawWithFurniture w drawCmds)++ drawChildren]
          | otherwise = []
          where
            (x,y) = position w
            drawChildren = concatMap redrawWindow (children w)
            drawCmds = case (handleMessage w) w Draw (queryFunction topWin) of
                         (DrawCmds d) -> d
                         _ -> []

-------------------------------------------------------------------------------
-- |Toggle the display state of the window between Hidden and Enabled

displayToggleHidden :: Window -> Window
displayToggleHidden w
  | displayState w == Hidden = w { displayState = Enabled }
  | otherwise = w { displayState = Hidden }

-------------------------------------------------------------------------------
-- |Toggle the display state of the window between Disabled and Enabled

displayToggleDisabled :: Window -> Window
displayToggleDisabled w
  | displayState w == Disabled = w { displayState = Enabled }
  | otherwise = w { displayState = Disabled }


-- $UserGuide
-- Some guide text

-- $MessageHandling
-- The message handling functions are the heart of the window system, as not only do they
-- allow windows to communicate with each other and the outside world, they also are the
-- mechanism for a window to have some window-specific information or state associated with
-- it.
