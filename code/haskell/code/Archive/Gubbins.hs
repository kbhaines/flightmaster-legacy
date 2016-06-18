-- |The Gubbins module, for want of a *much* better name, handles the bridge
-- between the nice, pure monadic updates that happen under the WindowMP
-- regime, and the changes that are required within the reset of the Message
-- Processors that happens out in IO-land.
--
-- In order to do this, we need to invoke IO-callbacks. These callbacks are in
-- this module, and need to be 'wired-in' to the message handlers for the
-- appropriate windows - the functions are passed out of the WindowMP via the
-- IORequest response.

module Gubbins (

    dragResetGubbins
  , zoomGubbins

) where

import MapParameters as MP
import Selection as Sel
import GPS as GPS
import ObjectDB as DB
import Navigation as Nav
import Window as Win

import UI

--------------------------------------------------------------------------------
-- |Called as the user clicks on the map (from mapHandleEvent)

{-
plannerSelection :: WindowMP -> MapParametersMP -> SelectionMP -> EditMP -> NavigationMP -> IO ()
plannerSelection win mparam selection edit nav = do
    (CheckpointResponse curSel) <- selection GetAsCheckpoint
    plan <- nav ReportPlan
    case curSel of 
        Just cp -> do
            -- InitialiseEditor returns a list of the editing buttons that will be 
            -- valid, given the current plan and the selected checkpoint
            buttons <- edit InitialiseEditor plan cp
            mapM (\i -> win (SetState i Enabled)) buttons
        _ -> do
            -- No selection, hide the editing controls
            mapM (\i -> win (SetState i Disabled)) [UI.planButtonID, UI.deleteButtonID]
    return ()

-- |Called as the user clicks on the PLAN button (from planHandleEvent)
plannerPlanButton :: WindowMP -> MapParametersMP -> SelectionMP -> EditMP -> NavigationMP -> IO ()
plannerPlanButton win mparam selection edit nav = do
    (CheckpointResponse (Just wp)) <- selection GetAsCheckpoint
    nextPlan <- edit GetNextPlanOption
    nav SetPlan nextPlan
    win (SetState (UI.deleteButtonID) Hidden)

-- |Called as the user clicks on the delete button (from deleteHandleEvent)
plannerDeleteButton :: WindowMP -> MapParametersMP -> SelectionMP -> EditMP -> NavigationMP -> IO ()
plannerDeleteButton win mparam selection edit nav = do
    nextPlan <- edit GetNextDeleteOption
    nav SetPlan nextPlan
    win (SetState (UI.planButtonID) Hidden)
    -}

