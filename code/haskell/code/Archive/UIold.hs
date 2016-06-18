module UI where

import Window
import WindowMessage
import Control
import Graphics

import FlightPlan as FP
import Waypoint
import Utils

-- this is an arbitrary but unique ID that we can use in StateUpdateNotify
-- messages when a global is being updated

globalWindowID = ID 99999

-------------------------------------------------------------------------------

mapWindowID = ID 1200
mapResetViewButtonID = ID 1201
zoomInButtonID = ID 1202
zoomOutButtonID = ID 1203

mapResetViewButton = buttonNew mapResetViewButtonID (Bounds ((10,10),(60,40))) [LockBottom 10, LockRight 10] "Reset" Nothing

zoomInButton = buttonNew zoomInButtonID (Bounds ((10,70),(60,40))) [] "+" Nothing
zoomOutButton = buttonNew zoomOutButtonID (Bounds ((10,120),(60,40))) [] "-" Nothing

-------------------------------------------------------------------------------

airspaceClassSelectorID = ID 1000
classACheckbox = ID 1001
classBCheckbox = ID 1002
classCCheckbox = ID 1003
classDCheckbox = ID 1005
classECheckbox = ID 1006
classFCheckbox = ID 1007
suasCheckbox = ID 1008
trackUpCheckboxID = ID 1009

airspaceClassCheckboxes = [ 
    ("Class A", classACheckbox)
  , ("Class B", classBCheckbox)
  , ("Class C", classCCheckbox)
  , ("Class D", classDCheckbox)
  , ("Class E", classECheckbox)
  , ("Class F", classFCheckbox)
  , ("SUAS", suasCheckbox)
  , ("Track Up", trackUpCheckboxID)
  ]

airspaceClassSelectorWindow = newWindow airspaceClassSelectorID
    (mkCheckboxes (Bounds ((10,10),(100,25))) airspaceClassCheckboxes) 
    "Airspace Classes"
    (framedStyle ++ [LockRight 90, LockTop 10])
    (Bounds ((0,30),(120,280))) (Just $ airspaceClassSelectorHandler)
        
mkCheckboxes :: Bounds -> [(String, ID)] -> [Window]
mkCheckboxes _ [] = []
mkCheckboxes bounds@(Bounds ((x,y),(w,h))) ((label,ident):cs) = checkbox:mkCheckboxes (Bounds ((x,y+h),(w,h))) cs
  where
    checkbox = checkboxNew ident bounds label True

airspaceClassSelectorHandler :: Window -> MessageBody -> QueryFunction -> Response

airspaceClassSelectorHandler win (StateUpdateNotify buttonID (Just Clicked)) qfunc 
 | buttonID == configButtonID = Update (displayToggleHidden win) Nothing

airspaceClassSelectorHandler _ _ _ = NoResponse

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

configButtonID = ID 9000
configButton = buttonNew configButtonID (Bounds ((10,90),(60,40))) [LockTop 10, LockRight 10] "CFG" Nothing

-------------------------------------------------------------------------------

plannerWindowID = ID 1100

-- #define PLANBUTTONID (ID 1101)
-- #define PLANNERWINDOWID  (ID 1100)
-- #define GLOBAL_SELECTION (10001)
-- #define CLICKED(b) (StateUpdateNotify b (Just Clicked))

-- Planner can be in one of the following states
data  PlannerState = 
    PlannerOff          -- no waypoint selected
  | PlannerSelected     -- waypoint selected
  | PlannerPlannedWP    -- waypoint has been 'planned' (insert/delete/goto)
    deriving (Eq, Show)

data PlannerWindowState = PlannerWindowState {

    plannerState        :: PlannerState
  , plannerPermutations :: [FlightPlan]     -- list of possible plans using waypoint
  , plannerUndoPlan     :: Maybe FlightPlan -- original flight plan, before we edited it

} deriving (Eq, Show)

plannerWindow = newWindow plannerWindowID [] "Planner" 
    ([LockRight 10, LockTop 60])
    (Bounds ((0,30),(60,200))) (Just $ plannerWindowHandler initialState)
    where
        initialState = PlannerWindowState PlannerOff [] Nothing

-------------------------------------------------------------------------------
-- figure out which planning buttons are valid to show

validPlanButtons :: FlightPlan -> Waypoint -> [Window]
validPlanButtons currentPlan wayp = 
    if (waypointIsInPlan currentPlan wayp) then 
        [planButton, deleteButton]
      else
        [planButton]

-------------------------------------------------------------------------------
-- message handler
--
-- This handler manages the state machine for the planner. Local functions are named
-- according to the convention "fromState_toState"

plannerWindowHandler :: PlannerWindowState -> Window -> MessageBody -> QueryFunction -> Response

-- waypoint selected on map, where we get a global notification of the globalSelection
-- variable being updated

plannerWindowHandler (PlannerWindowState pstate _ currentUndo) win (StateUpdateNotify mapWindowID (Just (SyncVar (varID, SVWaypoint wayp)))) query
 | varID == globalSelection = UpdatePrivate any_selected

 where
    any_selected = win { displayState = Enabled
                         , handleMessage = plannerWindowHandler (PlannerWindowState PlannerSelected [] newUndo)
                         , children = if currentUndo /= Nothing then 
                                        undoButton:(validPlanButtons currentPlan wayp) 
                                      else
                                        validPlanButtons currentPlan wayp}
    newUndo = case pstate of
                PlannerOff -> Nothing
                PlannerSelected -> currentUndo
                PlannerPlannedWP -> currentUndo

    (SVFlightPlan currentPlan) = query mapWindowID globalFlightPlan

-- waypoint deselected, simply hide the planner
plannerWindowHandler pws win (StateUpdateNotify mapWindowID (Just (SyncVar (varID, BoolValue _)))) _ = 
    Update (win { displayState = Hidden 
                , handleMessage = plannerWindowHandler (PlannerWindowState PlannerOff [] Nothing) }) Nothing

-- plan/delete/goto button pressed
plannerWindowHandler (PlannerWindowState pstate perms savedPlan) win (StateUpdateNotify buttonID (Just Clicked)) query
 | pstate == PlannerSelected  && (buttonID `elem` [planButtonID, deleteButtonID]) = selected_PlannedWp
 | pstate == PlannerPlannedWP && (buttonID `elem` [planButtonID, deleteButtonID]) = plannedWp_PlannedWp
 where
 
    -- move from selected to planned state, disabling other buttons, saving the current plan
    -- and setting up the permutations according to which button was pressed. Also disable
    -- the other planning buttons and enable the undo button

    selected_PlannedWp = 
        let newPerms = case buttonID of
                b | b == planButtonID -> planPermutations currentPlan selectedWaypoint
                b | b == deleteButtonID -> deletePermutations currentPlan selectedWaypoint
                otherwise -> error "Illegal button encountered in plannerWindowHandler"

            (SVWaypoint selectedWaypoint) = query mapWindowID mapWaypointSelection
            (SVFlightPlan currentPlan) = query mapWindowID globalFlightPlan
        in 
        if newPerms == [] then 
            error "No planning permutations"
          else
            Update win { 
                handleMessage = plannerWindowHandler (PlannerWindowState PlannerPlannedWP newPerms (Just currentPlan))
              , children = [plannerButton buttonID, undoButton]
            } (Just $ SyncVar (globalFlightPlan, SVFlightPlan $ head newPerms))
    
    -- select the next permutation from the list of permutations

    plannedWp_PlannedWp = 
        let (p:ps) = perms
            newPerms = ps ++ [p]
        in
        Update win {
            handleMessage = plannerWindowHandler (PlannerWindowState PlannerPlannedWP newPerms savedPlan)
          , children = [plannerButton buttonID, undoButton]
        } (Just $ SyncVar (globalFlightPlan, SVFlightPlan $ head newPerms))

-- undo button pressed
plannerWindowHandler (PlannerWindowState pstate perms (Just savedPlan)) win (StateUpdateNotify buttonID (Just Clicked)) query
 | buttonID == undoButtonID = Update any_any restorePlan
 where
    restorePlan = Just $ SyncVar (globalFlightPlan, SVFlightPlan savedPlan)
    (SVWaypoint selectedWaypoint) = query mapWindowID mapWaypointSelection
    any_any = win {
        handleMessage = plannerWindowHandler (PlannerWindowState PlannerSelected [] Nothing)
      , children = validPlanButtons savedPlan selectedWaypoint
    }

plannerWindowHandler _ _ _ _ = NoResponse

-- map from ID to button
plannerButton :: ID -> Window
plannerButton x
 | x == planButtonID = planButton
 | x == deleteButtonID = deleteButton
 | x == undoButtonID = undoButton
 -- | x == gotoButtonID = gotoButton

planButtonID = ID 1101
planButton = buttonNew planButtonID (Bounds ((0,0),(60,40))) [LockTop 0, LockLeft 0] "PLAN" Nothing

undoButtonID = ID 1102
undoButton = buttonNew undoButtonID (Bounds ((0,50),(60,40))) [LockTop 50, LockLeft 0] "UNDO" Nothing

deleteButtonID = ID 1103
deleteButton = buttonNew deleteButtonID (Bounds ((0,100),(60,40))) [LockTop 100, LockLeft 0] "DEL" Nothing

-------------------------------------------------------------------------------

planWindowID = ID 9002
planWindow = newWindow planWindowID [] "Planning" ([LockBottom 10, LockLeft 10]) (Bounds ((0,0),(300,30))) 
                (Just $ planWindowHandleMessage blankPlan)

planWindowHandleMessage :: FlightPlan -> Window -> MessageBody -> QueryFunction -> Response

planWindowHandleMessage _ win (StateUpdateNotify _ (Just (SyncVar (gvar, SVFlightPlan fp)))) _ 
 | gvar == globalFlightPlan = UpdatePrivate $ win { handleMessage = planWindowHandleMessage fp } 

planWindowHandleMessage plan win Draw qf = DrawCmds [RasterText 3 (0,25,0) (GraphicsColour3 0 0 0) ids]
  where
    ids = concatMap ((++" : ").unSString.identifier) (FP.toList plan)

planWindowHandleMessage _ _ _ _ = NoResponse
