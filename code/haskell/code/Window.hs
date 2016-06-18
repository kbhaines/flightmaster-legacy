-- |A bare-metal window manager, part of OpenGLINT. The manager draws windows
-- into GraphicsCmd's as implemented in the OpenGLINT graphics-interface layer:
-- at the moment that layer only translates to OpenGL commands. 

module Window (

    -- * Functions
    interface
  , position
  , dimensions
  , noHandler
  , findWindow
  , findWindowM
  , applyWindowM
  , replaceWindow
  , dim
  , hide
  , enable


    -- * Types
    -- ** Windows and their attributes
  , Window(..)
  , WindowMP

  , ID(..)
  , Title
  , Bounds(..)
  , Coordinate
  , DisplayState(..)
  , Style(..)

    -- ** Messaging types
  , MessageHandler
  , StateHandle

  , Message(..)

  , Response(..)

    -- * Miscellaneous types/functions
  , framedStyle
  --, expandWithChildren

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
import qualified MessageProcessor as MP

--------------------------------------------------------------------------------
-- |Short-hand for Window message processor

type WindowMP = MP.MessageProcessor Message Response

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
  , displayState  :: DisplayState

    -- |The message handling function for the window. By using a partially-applied
    -- function you can make your window have locally-available configuration 
    -- information
  , handleMessage :: MessageHandler

}

instance Eq Window where
    a == b = (ident a == ident b)

instance Show Window where
    show w = show (ident w) ++ show (title w) ++ show (bounds w) ++ show (children w)


-- TODO: Change to Graphics.Position types
-- window rectangle bounds. Comprised (x,y),(w,h)

-- |Bounding box of window: (x,y) and (w,h)
newtype Bounds = Bounds (Coordinate,Coordinate)
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- |Windows can be in one of these states. 'Enabled' will draw the window, 'Disabled'
-- will still draw the window, but it will not respond to click-events. 'Hidden' ensures
-- the window is not drawn, nor responds to click events.

data DisplayState = Enabled | Disabled | Hidden
  deriving (Eq, Show, Read)

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

framedStyle = [Background (GraphicsColour3 1 1 1), Frame (GraphicsColour3 0 0 0) 1.0]

--------------------------------------------------------------------------------
-- |ID is used to identify each window

newtype ID = ID Int deriving (Eq,Show, Read)


-- |A message handler takes a window and message and returns a response to the
--message. The message handler is usually a partially-applied function that has
--a parameter representing state of the window. 

type MessageHandler = (Window -> Message -> State Window Response)

--------------------------------------------------------------------------------
-- |A StateHandle is a unique identifier used by each window to refer to all or
-- part of the state of the window; i.e. its interpretation is window-specific.
-- For example, a window can use the value '0' to mean its complete state, or
-- just a part of that state

type StateHandle = Int

--------------------------------------------------------------------------------
-- |Message types supported by the Window system

data Message = 

    -- | Find the windows that contain the specified position or the specified
    -- GraphicsNames
    FindHits Coordinate [GraphicsHit]

    -- |Mouse click at a position, and a list of OpenGL object IDs that were underneath
  | LeftButtonDown ID Coordinate [GraphicsHit]

    -- |Mouse-down drag events with delta and absolute coordinates of movement
  | LeftDrag ID               -- ^Window that got dragged
             Coordinate -- ^Delta of movement
             Coordinate -- ^Absolute position of cursor

    -- |Mouse drag event has stopped over the original drag-start point, but the button is still down
  | LeftDragStop ID

    -- |Mouse drag event has ended
  | LeftDragCompleted ID Coordinate

    -- |Mouse click lifted at position, and list of OpenGL object IDs that were underneath
  | LeftButtonUp ID Coordinate [GraphicsHit]

    -- |Window has been selected; i.e. LeftButtonDown and LeftButtonUp in the same window
  | WindowSelected ID Coordinate [GraphicsHit]

    -- |Request to redraw the window, and all of its children
  | Draw ID

    -- |Request for the window to return all, or part, of its state (as a StateValue response)
  | StateQuery ID StateHandle

    -- |Send keypress to the window
  | Key ID Char

    -- |The main application window has been resized to the specified dimensions. Broadcast.
  | AppWindowResize Coordinate

    -- |A request for the window to move to the new coordinates
  | Move ID Coordinate

    -- |A request for the window to resize
  | Resize ID Coordinate

    -- |Request for the window to change its state
  | SetState ID DisplayState

    -- |Set focus to the specified window
  | SetFocus ID

    -- |Get focus
  | GetFocus ID

    -- |Set message handler
  | SetHandler ID MessageHandler


--------------------------------------------------------------------------------
-- |Responses to window messages

data Response = 

    OK

  | Failed

    -- |Graphics commands to draw the window (response to Draw message)
  | DrawCmds [GraphicsCmd]

    -- |Response to a state enquiry, see below
  | Response StateValue

    -- |Window ignored message
  | NoResponse

    -- |Window wants to run IO action
  | IORequest Response (WindowMP -> IO ())

    -- |Response to FindHits
  | Hits [(ID,Coordinate)]

--------------------------------------------------------------------------------
-- |Possible ways for a window to report or recieve its state

data StateValue = 
    StringValue String 
  | IntValue Int 
  | BoolValue Bool 
  | DoubleValue Double


--------------------------------------------------------------------------------
-- |Window interface function, to allow window system to be used as a message
-- processor.

interface :: Window -> Message -> (Window, Response)

interface topWin (Draw id) = (topWin, DrawCmds cmds)
  where

    -- Note the use of the (-1.0) scaling factor, this inverts the 'y' axis
    -- so that (0,0) is at the top left
    
    cmds = [Scale 1.0 (-1.0) 1.0 $ redrawWindow topWin]

    -- redraw a single window, taking account of the displayState
    redrawWindow :: Window -> [GraphicsCmd]
    redrawWindow w 
      | displayState w /= Hidden = [RelTranslate (x,y,0) $ (drawWithFurniture w drawCmds)++ drawChildren]
      | otherwise = []
      where
        (x,y) = position w
        drawChildren = concatMap redrawWindow (children w)
        drawCmds = case (evalState ((handleMessage w) w (Draw id)) topWin) of
                     (DrawCmds d) -> d
                     _ -> []


interface topWin (Move id newPos) = (newTopWin, OK)
  where 
    newTopWin = winMap (applyTo id (flip move newPos)) topWin


interface topWin (SetState id newState) = 
    (winMap (applyTo id (\w -> w { displayState = newState} ) ) topWin , OK)


interface topWin (SetHandler id newHandler) =
    (winMap (applyTo id (\w -> w { handleMessage = newHandler} ) ) topWin , OK)


interface topWin msg@(LeftButtonDown id _ _) = sendMessageToWindow topWin id msg
interface topWin msg@(LeftButtonUp id _ _) = sendMessageToWindow topWin id msg
interface topWin msg@(LeftDrag id _ _) = sendMessageToWindow topWin id msg
interface topWin msg@(LeftDragStop id) = sendMessageToWindow topWin id msg
interface topWin msg@(LeftDragCompleted id _) = sendMessageToWindow topWin id msg
interface topWin msg@(WindowSelected id _ _) =sendMessageToWindow topWin id msg

interface topWin (AppWindowResize newSize) = (handleParentResizeMessage newSize topWin, OK)

interface topWin (FindHits pos _) = (topWin, Hits $ pickWindows topWin pos)

interface topWin _ = (topWin, NoResponse)

--------------------------------------------------------------------------------
-- |Send a message to the window, updating it and allowing it to return a
--response

sendMessageToWindow :: Window -> ID -> Message -> (Window,Response)
sendMessageToWindow topWin id msg
 | fw /= Nothing = let (a,b) = runState ((handleMessage fw') fw' msg) topWin in
                    (b,a)
 | otherwise = (topWin, Failed)
  where
    --newWin = winMap (applyTo id (fst.callHandler)) topWin
    --winResponse = snd $ callHandler fw' 
    fw = findWindow id topWin
    fw' = fromJust fw

--------------------------------------------------------------------------------
-- |Helper function to apply the given function only to a window that
-- has the specified ID.

applyTo :: ID -> (Window -> Window) -> Window -> Window
applyTo id f w
 | id == ident w = f w
 | otherwise = w


--------------------------------------------------------------------------------
-- |Find the specified window by Id, starting at the given window

findWindow :: ID -> Window -> Maybe Window
findWindow id win = find (\w -> ident w == id) (expandWithChildren win)

findWindowM :: ID -> State Window (Maybe Window)
findWindowM id = do
    top <- get
    return $ findWindow id top

--------------------------------------------------------------------------------
-- |Apply the given function to the window

applyWindowM :: ID -> (Window -> Window) -> State Window ()
applyWindowM id f = do
    (Just win) <- findWindowM id
    replaceWindow id (f win)
    return ()

--------------------------------------------------------------------------------
-- |Apply the function to the window and all child windows, recursively

winMap :: (Window -> Window) -> Window -> Window
winMap f topWin = newTop { children = newChildren }
  where newTop = (f topWin)
        newChildren = map (winMap f) (children newTop)

--------------------------------------------------------------------------------
-- |Replace the specified window

replaceWindow :: ID -> Window -> State Window Bool
replaceWindow id newWin = do
    ws <- get
    put $ winMap (applyTo id (\_ -> newWin) ) ws
    return True

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
-- |Get the position (x,y) of the window
position :: Window -> Coordinate
position w = let Bounds (pos,_) = bounds w in pos

-------------------------------------------------------------------------------
-- |Get the dimensions (w,h) of the window
dimensions :: Window -> Coordinate
dimensions w = let Bounds (_,dim) = bounds w in dim

-------------------------------------------------------------------------------
-- |Move the window to the new coordinates

move :: Window -> Coordinate -> Window
move win newPos = win { bounds = Bounds (newPos,curSize) }
    where curSize = dimensions win

-------------------------------------------------------------------------------
-- |Resize the window to the new dimensions

resize :: Window -> Coordinate -> Window
resize win newsize = win { bounds = Bounds (position win,newsize) }

--------------------------------------------------------------------------------
-- |Move bounds 

moveBounds :: Coordinate -> Bounds -> Bounds
moveBounds (x,y) (Bounds (_,dim)) = Bounds ((x,y),dim)

--------------------------------------------------------------------------------
-- | Get dimensions of bounds

dimensionBounds :: Bounds -> Coordinate
dimensionBounds (Bounds (_,wh)) = wh

--------------------------------------------------------------------------------
-- |Get position of bounds

positionBounds :: Bounds -> Coordinate
positionBounds (Bounds (xy,_)) = xy


--------------------------------------------------------------------------------
-- |Convert the window and all children in to a flat list, easier for searching

expandWithChildren :: Window -> [Window]
expandWithChildren topWin = topWin : concatMap (expandWithChildren) (children topWin)


-------------------------------------------------------------------------------
-- |Message handler to respond to parent-resize messages

handleParentResizeMessage :: Coordinate -> Window -> Window
handleParentResizeMessage newParentSize win = updatedWindow  { children = resizedChildren }
  where
    updatedWindow = setAutoPosition win newParentSize
    resizedChildren = map (handleParentResizeMessage $ dimensions updatedWindow) (children win)


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

pickWindows :: Window -> Coordinate -> [(ID,Coordinate)]
pickWindows topWin pos = reverse $ scanWindows pos topWin
    where 
        -- if the point is in the window, it is added to the results list along
        -- with the results of the children-scan. Otherwise, just the 
        -- results of the children-scan.
        scanWindows :: Coordinate -> Window -> [(ID,Coordinate)]
        scanWindows pos w
         | displayState w /= Enabled = []
         | ptInWindow pos w = (ident w, toWinCoords pos w):childrenScan
         | otherwise = childrenScan
         where subPos = shiftTestPt pos w
               childrenScan = concatMap (scanWindows subPos) (children w)

        -- calculate window-relative coordinates of position
        toWinCoords :: Coordinate -> Window -> Coordinate
        toWinCoords ((px,py)) win = (px - winx, py - winy)
          where
            (winx, winy) = position win

        -- test if the point is inside the window
        ptInWindow :: Coordinate -> Window -> Bool
        ptInWindow ((px,py)) win = (boundTest (px - winx) winw) && (boundTest (py - winy) winh)
          where
            (winx,winy) = position win
            (winw,winh) = dimensions win
            boundTest a1 a2 = (a1 >=0 && a1 < a2)

        -- shift the specified point relative to the window position
        shiftTestPt ((px,py)) win = ((px-winx, py-winy))
         where (winx, winy) = position win


--------------------------------------------------------------------------------
-- |Auto-alignment functions 
--
--The state monad carries the Bounds of the last window to be positioned in the
--Monad, and the Coordinate carries spacing information to leave appropriate
--gaps between the edges of the windows.
--

--------------------------------------------------------------------------------
-- |Put the window bounds according to the given function, updating the state
-- accordingly

align :: (Bounds -> Bounds -> Coordinate -> Bounds) -> Window -> State (Bounds,Coordinate) Window
align boundf w = do
    (currBounds, spacing) <- get
    let newBounds = boundf currBounds (bounds w) spacing
    put (newBounds, spacing)
    return (w { bounds = newBounds })

--------------------------------------------------------------------------------
-- |Set the bounds to the absolute position

absolute :: Coordinate -> Window -> State (Bounds,Coordinate) Window
absolute coord w = align at w where
    at :: Bounds -> Bounds -> Coordinate -> Bounds
    at last new _ = moveBounds coord new

--------------------------------------------------------------------------------
-- | Set the current bounds to those specified

set :: Bounds -> State (Bounds,Coordinate) ()
set b = do
    (_, spacing) <- get
    put (b,spacing)

--------------------------------------------------------------------------------
-- | Put the bounds to the right of the last bounds.
-- Parameters are lastBounds -> winBounds -> spacing.

right :: Bounds -> Bounds -> Coordinate -> Bounds
right last newBounds (xs,_)  = moveBounds (x + w + xs,y) newBounds where
    (x,y) = positionBounds last
    (w,_) = dimensionBounds last

--------------------------------------------------------------------------------
-- | Put the bounds to the left of the last bounds.

left :: Bounds -> Bounds -> Coordinate -> Bounds
left last newBounds (xs,_)  = moveBounds (x - w - xs,y) newBounds where
    (x,y) = positionBounds last
    (w,_) = dimensionBounds newBounds

--------------------------------------------------------------------------------
-- | Put the bounds below the last bounds.

below :: Bounds -> Bounds -> Coordinate -> Bounds
below last newBounds (_,ys)  = moveBounds (x, y+h+ys) newBounds where
    (x,y) = positionBounds last
    (_,h) = dimensionBounds last

--------------------------------------------------------------------------------
-- | Put the bounds above the last bounds.

above :: Bounds -> Bounds -> Coordinate -> Bounds
above last newBounds (_,ys)  = moveBounds (x, y-h-ys) newBounds where
    (x,y) = positionBounds last
    (_,h) = dimensionBounds newBounds

--------------------------------------------------------------------------------
-- |Overlay the bounds on the last bounds

current = align (\b w _ -> moveBounds (positionBounds b) w)


test = do
    w1 <- current win1
    w2 <- align right win2
    w3 <- align right win3

    (set.bounds) w1
    w4 <- align below win4
    return [w1,w2,w3, w4]

initBounds = Bounds ((0,0),(0,0))

win1 = Window (ID 0) "Test1" [] (dim 20 10) [] Enabled undefined
win2 = Window (ID 1) "Test2" [] (dim 30 10) [] Enabled undefined
win3 = Window (ID 2) "Test3" [] (dim 40 10) [] Enabled undefined
win4 = Window (ID 3) "Test4" [] (dim 40 10) [] Enabled undefined

dim w h = Bounds ((0,0),(w,h))

hide w = w { displayState = Hidden }
enable w = w { displayState = Enabled }

noHandler :: MessageHandler
noHandler w _ = return NoResponse

