-- |Module information

module GlutManager (

    -- * Functions
    openGLInit
  
    -- * Types

) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.Time.Clock
import Data.Time.Calendar

import Graphics
import Window as Win
import MessageProcessor

import Utils

--------------------------------------------------------------------------------
-- |Drag processor, deals with the user dragging the mouse

data DragMessage = SetClicked Position Win.ID UTCTime | UpdatePosition Position | ClearClicked

type DragInfo = Maybe (Position, Win.ID,UTCTime)

-- |Response to all DragMP messages. The indication that a drag is occurring comes if a response
-- to UpdatePosition returns a result where the two positions are not equal.
type DragResult = (Position, Position, Win.ID)

type DragMP = MessageProcessor DragMessage DragResult

dragThreshold = 30^2

dragMessageProcessor :: DragInfo -> DragMessage -> (DragInfo, DragResult)
dragMessageProcessor Nothing (SetClicked pos id time) =  let r = Just $ (pos, id, time) in (r,(pos,pos,id))

dragMessageProcessor r@(Just (p,i,t)) (UpdatePosition pos) 
  | dragRangeSq p pos > dragThreshold = (r,(p,pos,i))
  | otherwise = (r,(p,p,i))

dragMessageProcessor (Just (_,id,_)) ClearClicked = let p = Position 0 0 in (Nothing,(p,p,id))

dragMessageProcessor _ _ = error "Drag processor: this shouldn't happen to me!"

--------------------------------------------------------------------------------
-- |Calculate the square of the range between the two positions
dragRangeSq :: Position -> Position -> Int
dragRangeSq (Position x1 y1) (Position x2 y2) = fromIntegral $ (x2-x1) ^2 + (y2-y1) ^2

--------------------------------------------------------------------------------
-- |Initialise the GLUT and OpenGL drivers, and enter the GLUT event loop
-- This function never returns.

openGLInit :: MessageProcessor Win.Message Win.Response -> GraphicsTextureManager -> (IO()) -> IO ()
openGLInit windowMP texMan closeDown = do
    (progname,_) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
    createWindow "FlightMaster Delta"
    windowSize $= Size 640 400

    --enterGameMode
    --fullScreen
 
    dragMp <- newMessageProcessor Nothing dragMessageProcessor 

    displayCallback $= runUserDisplay
    reshapeCallback $= Just (reshape windowMP)
    keyboardMouseCallback $= Just (keyboardCallback windowMP dragMp texMan)
    --idleCallback $= Just idle
    motionCallback $= Just (mouseMotionCallback windowMP dragMp)
    --passiveMotionCallback $= Just motion

    addTimerCallback 1000 $ timerCallback (0,0)

    closeCallback $= Just closeDown
    mainLoop

    putStrLn "Exit!!"

    where
        runUserDisplay :: IO ()
        runUserDisplay = do
            (DrawCmds gcmds) <- windowMP (Draw $ ID 0)
            screenSize@(GraphicsSize (sx,sy)) <- graphicsGetViewport
            let projection@(sx',sy',sz') = (fromIntegral sx/2, fromIntegral sy/2,4000) :: Graphics3d

            -- Translate the origin to the topleft corner of the window
            graphicsProcess texMan $ concat [[InitDrawing projection] , [RelTranslate (-sx',sy',0) gcmds] , [FinishedDrawing] ]

-- |Pick element from display using graphics subsystem
graphicsPick :: WindowMP -> GraphicsTextureManager -> GraphicsPosition -> IO [GraphicsHit]
graphicsPick windowMP texMan selection = do
    screenSize@(GraphicsSize (sx,sy)) <- graphicsGetViewport
    let projParams@(sx',sy',sz') = (fromIntegral sx/2, fromIntegral sy/2,4000) :: Graphics3d
    (DrawCmds gcmds) <- windowMP $ Draw (ID 0)
    graphicsHits texMan $ concat [ [InitPicking projParams selection] , [RelTranslate (-sx',sy',0) gcmds] ]

-- |Pick window and graphics elements from display, using window coorindates and graphics subsystem
pickHits :: WindowMP -> GraphicsTextureManager -> Position -> IO (ID, Coordinate, [GraphicsHit])
pickHits windowMP texMan pos@(Position x y) = do

    let gpos = (fromIntegral x, fromIntegral y) :: Coordinate
        [x',y'] = map fromIntegral [x,y]

    elementList <- graphicsPick windowMP texMan $ GraphicsPosition (x',y')
    GraphicsSize (w,h) <- graphicsGetViewport

    (Hits hits) <-  windowMP $ FindHits gpos elementList
    putStrLn $ show hits
    let (clickedWin, posInWin) 
         | not $ null hits = head $ hits 
         | otherwise = error "No window candidates, should not happen if you've got a top-window!"

    return (clickedWin, posInWin, elementList)

--------------------------------------------------------------------------------
-- |Handler for keyboard events

keyboardCallback :: WindowMP -> DragMP -> GraphicsTextureManager -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardCallback windowMP dragMp texMan (MouseButton LeftButton) Down _ pos@(Position x y) = do

    -- left mouse button clicked, find out which windows and which graphics
    -- elements are targetted

    (clickedWin, posInWin, elementList) <- pickHits windowMP texMan pos
    debug $ show (clickedWin, posInWin)

    -- Send out the 'LeftButtonDown' message to the target window
    resp <- windowMP $ LeftButtonDown clickedWin posInWin elementList
    processPossibleIO resp windowMP

    -- setup the dragMP
    currentTime <- getCurrentTime
    _ <- dragMp $ SetClicked pos clickedWin currentTime

    if elementList /= [] then do
        debug $ show elementList
      else
        debug "Left click"

    postRedisplay Nothing
 
keyboardCallback windowMP dragMP texMan (MouseButton LeftButton) Up _ pos@(Position x y) = do
    (p1,p2,winId) <- dragMP (UpdatePosition pos)
    (_,_,id) <- dragMP ClearClicked

    (liftedInWin, posInWin, elementList) <- pickHits windowMP texMan pos

    putStrLn $ show "Sending to "++ show id ++ show (p1,p2)

    -- p1 and p2 are different if the user has been dragging the pointer around
    -- with the button down, in this case we need to send 'LeftDragCompleted'
    -- instead of LeftButtonUp
    let msg = if p1 == p2 then
            LeftButtonUp id posInWin elementList
          else
            LeftDragCompleted id posInWin

    resp <- windowMP msg
    processPossibleIO resp windowMP

    -- Send a WindowSelected event if the mouse down and up events occurred 
    -- in the same window
    if winId == liftedInWin then do
        resp <- windowMP (WindowSelected winId posInWin elementList)
        processPossibleIO resp windowMP
      else
        return ()

    case resp of
        IORequest _ _ -> putStrLn "IORFe"
        _ -> return ()

    postRedisplay Nothing
    return ()

keyboardCallback _ _ _ _ _ _ _ = return ()


-- |Handler for mouse motion events, will send out 'LeftDrag' when the user has
-- dragged away from the click position, or 'LeftDragStop' if the user returns
-- the mouse to the original click position

mouseMotionCallback :: WindowMP -> DragMP -> Position -> IO ()
mouseMotionCallback winMP dragMP pos = do
    
    (p1,p2,winId) <- dragMP (UpdatePosition pos)

    -- dragMP reports different values for p1 and p2 only if the user is 
    -- dragging the mouse around, far enough away from the initial click position,
    -- if this is the case then we send out LeftDrag messages

    if p1 /= p2 then do
        let (Position x1 y1, Position x2 y2) = (p1,p2)
            (dx,dy) = (fromIntegral $ x2-x1, fromIntegral $ y2-y1)
        winResp <- winMP (LeftDrag winId (dx,dy) (fromIntegral x2, fromIntegral y2))
        processPossibleIO winResp winMP
        return ()
      else do
        winResp <- winMP (LeftDragStop winId)
        processPossibleIO winResp winMP
        return ()

    putStrLn $ show (p1,p2)
    postRedisplay Nothing
    return ()

processPossibleIO :: Win.Response -> WindowMP -> IO ()
processPossibleIO (Win.IORequest _ act) wmp = act wmp
processPossibleIO _ _ = return ()

--------------------------------------------------------------------------------
-- | Reshape window callback for GLUT. 
-- Need to send AppWindowResize event to all windows.

reshape :: MessageProcessor Message Response -> Size -> IO ()
reshape window s@(Size w h) = do 
    debug1 "Reshape called " s
    let [w',h'] = map fromIntegral [w,h]
    resp <- window (AppWindowResize (w',h'))
    debug "  Events sent"
    viewport $= (Position 0 0, s)


--------------------------------------------------------------------------------
-- | Idle callback for GLUT

idle :: IO ()
idle = do
    postRedisplay Nothing
    --debug " idle.."

--------------------------------------------------------------------------------
-- |Timer callback for GLUT

timerCallback :: (Int,Int) -> IO ()
timerCallback _ = do 
    postRedisplay Nothing
    addTimerCallback 1000 $ timerCallback (1000,1000)

