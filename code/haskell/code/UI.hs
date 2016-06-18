module UI where

import UID
import Window

import Map
import Control
import Control.Monad
import MapParameters as MP
import PlanEditor as PE
import Navigation as Nav
import Selection as Sel

import Coord

buttonDim = dim 40 40

--------------------------------------------------------------------------------
-- |User Interface definitions
--

topWin db edit mapp nav selection = 
    Window topWinID "Main" [FillParent] (dim 0 0) mainChildren Enabled (mapHandleEvent mapEnv)
    where
        mapEnv = MapEnvironment db edit mapp nav selection
        mainChildren = [
              zoomInButton mapp
            , zoomOutButton mapp
            , resetViewButton mapp
            , addButton edit nav
            , deleteButton edit nav
            , toButton edit nav
            , undoButton selection edit nav
            , clearButton selection edit nav
            ]

--------------------------------------------------------------------------------
-- |Zoom controls

zoomInButton mapp = buttonNew zoomInButtonID buttonDim [LockRight 0, LockBottom 50] "+" (Just $ zoomHandler (zoomGubbins mapp In))
zoomOutButton mapp =  buttonNew zoomOutButtonID buttonDim [LockRight 0, LockBottom 90] "-" (Just $ zoomHandler (zoomGubbins mapp Out))

data ZoomDirection = In | Out

zoomHandler ::  (WindowMP -> IO ()) -> MessageHandler
zoomHandler zoomfunc win (WindowSelected {}) = return $ IORequest OK zoomfunc
zoomHandler _ w m = buttonHandleMessage w m

--------------------------------------------------------------------------------
-- |Handle updates resulting from zoom requests

zoomGubbins :: MP.MapParametersMP -> UI.ZoomDirection -> WindowMP -> IO()
zoomGubbins mparams dir _ = do
    (MP.Response currentParams) <- mparams MP.GetParameters
    let newZoom = case dir of
                    UI.Out -> (MP.zoom currentParams) * 1.1
                    UI.In -> (MP.zoom currentParams) * 0.9
    _ <- mparams (SetZoom newZoom)
    return ()


--------------------------------------------------------------------------------
-- |View reset controls


resetViewButton mapp = (buttonNew resetViewButtonID buttonDim [LockRight 0, LockBottom 0] "POS" (Just $ resetViewHandler mapp) )
    { displayState = Hidden }

resetViewHandler :: MapParametersMP -> MessageHandler
resetViewHandler mapp win (WindowSelected {}) = do

    -- Disable the button straight away
    replaceWindow (ident win) (win { displayState = Hidden } )
    return $ IORequest OK $ dragResetGubbins mapp

resetViewHandler _ w m = buttonHandleMessage w m

--------------------------------------------------------------------------------
-- |Invoked when the user hits the Reset View button

dragResetGubbins :: MP.MapParametersMP -> WindowMP -> IO ()
dragResetGubbins mparams _ = do
    _ <- mparams (SetOrigin (ACOrigin originDefaults))
    return ()
    where originDefaults = coordDeg 51.5 (-2.0)

--------------------------------------------------------------------------------
-- | Planning controls

addButton editMP navMP = 
    hide $ buttonNew addButtonID buttonDim [LockRight 0, LockTop 100] "ADD" (Just $ addHandler editMP navMP)

deleteButton editMP navMP =
    hide $ buttonNew deleteButtonID buttonDim [LockRight 0, LockTop 150] "DEL" (Just $ deleteHandler editMP navMP)

toButton editMP navMP = 
    hide $ buttonNew toButtonID buttonDim [LockRight 0, LockTop 0] "To?" (Just $ toHandler editMP navMP)

undoButton selectionMP editMP navMP = buttonNew undoButtonID buttonDim [LockRight 0, LockTop 200] "UNDO" (Just $ undoHandler selectionMP editMP navMP)

clearButton selectionMP editMP navMP = buttonNew clearButtonID buttonDim [LockRight 0, LockTop 50] "CLR" (Just $ clearHandler selectionMP editMP navMP)


--------------------------------------------------------------------------------
-- | Handles messsages sent to add button
addHandler :: EditMP -> NavigationMP -> MessageHandler
addHandler edit nav win (WindowSelected _ _ _) = do
    applyWindowM deleteButtonID hide
    applyWindowM toButtonID hide
    return $ IORequest OK $ doEditAction NextAddOption edit nav

-- default
addHandler _ _ w m = buttonHandleMessage w m

--------------------------------------------------------------------------------
-- | Handles messages sent to delete button
deleteHandler :: EditMP -> NavigationMP -> MessageHandler
deleteHandler edit nav win (WindowSelected _ _ _) = do
    applyWindowM addButtonID hide
    applyWindowM toButtonID hide
    return $ IORequest OK $ doEditAction NextDeleteOption edit nav

-- default
deleteHandler _ _ w m= buttonHandleMessage w m

--------------------------------------------------------------------------------
-- |Handles messages sent to To button
toHandler :: EditMP -> NavigationMP -> MessageHandler
toHandler edit nav win (WindowSelected _ _ _) = do
    applyWindowM addButtonID hide
    applyWindowM deleteButtonID hide
    return $ IORequest OK $ doEditAction NextToOption edit nav

-- default
toHandler _ _ w m= buttonHandleMessage w m

--------------------------------------------------------------------------------
-- | Performs add/delete/to button actions

doEditAction :: PE.Message -> EditMP -> NavigationMP -> WindowMP -> IO ()
doEditAction editCmd edit nav win = do
    (PE.PlanResponse newPlan planCount) <- edit editCmd

    -- EditMP maintains a count for us of the number of requests that have been
    -- made for the next editing permutation. If this count is greater than 1
    -- then we know we need to Undo the previous SetPlan that we've done.  If
    -- the count is 1, then it's the first change to the plan using the EditMP
    -- permutations, and we don't need to Undo yet.
    
    (RequestCount numRequests) <- edit GetRequestCount
    if numRequests > 1 then do
        nav Undo
        return ()
      else
        return ()

    -- Turn off editing buttons if there is only one planning permutation
    if planCount == 1 then do
        forM_ [addButtonID, deleteButtonID, toButtonID] (\w-> win (SetState w Hidden))
      else
        return ()

    _ <- nav $ SetPlan newPlan
    (IdentListResponse ids) <- nav ReportIdents
    putStrLn $ show ids
    return ()
    
--------------------------------------------------------------------------------
-- |Undo handler
undoHandler :: SelectionMP -> EditMP -> NavigationMP -> MessageHandler
undoHandler selection edit nav w (WindowSelected _ _ _) = do
    applyWindowM addButtonID hide
    applyWindowM deleteButtonID hide
    return $ IORequest OK $ (\win -> do
        _ <- nav Undo
        _ <- updateSelection selection edit nav win
        return ()
        )

undoHandler _ _ _ w m = buttonHandleMessage w m

--------------------------------------------------------------------------------
-- |Clear plan handler

clearHandler :: SelectionMP -> EditMP -> NavigationMP -> MessageHandler
clearHandler selection edit nav win (WindowSelected _ _ _) = 
    return $ IORequest OK $ (\win -> do
        _ <- nav Nav.Clear
        _ <- updateSelection selection edit nav win
        return ()
        )

clearHandler _ _ _ w m = buttonHandleMessage w m
