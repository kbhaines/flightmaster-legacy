-- |Navigation module

module Navigation where

import Coord
import Data.Maybe

import MessageProcessor as MP

type NavigationMP = MP.MessageProcessor Message Response

-- |Plan represents a flight plan. A plan can be empty (Nothing), a starting checkpoint,
-- or a starting checkpoint and list of subsequent leg checkpoints
data Plan = Plan (Maybe ( Checkpoint , [LegCheckpoint])) -- ^Starting point and subsequent legs of the plan

  deriving (Eq, Show, Read)


-- |Navigation is a flight 'Plan' combined with an active checkpoint number, an
-- undo history and a redo history

data Navigation = Navigation {
    
    navPlan    :: Plan
  , navActive  :: (Maybe CheckpointNumber)
  , navHistory :: [Plan] 
  , navRedo    :: [Plan]

}
    deriving (Eq, Show, Read)

initial = Navigation (Plan Nothing) Nothing [] []

-- |A checkpoint is the fundamental component of a 'Plan'. Checkpoints contain
-- enough  basic information so that a complete plan can be created and
-- navigated without using actual waypoints from a database.
data Checkpoint = Checkpoint {

    checkpointCoord     :: Coord
  , checkpointMagVar    :: MagneticVariation
  , checkpointElevation :: Altitude
  , checkpointIdent     :: Maybe Ident

} deriving (Show, Read)

-- |Checkpoint equality is True if their coorindates and idents are equal, no 
-- other components of checkpoint are used
instance Eq Checkpoint where
    (==) a b = (checkpointCoord a) == (checkpointCoord b) && (checkpointIdent a) == (checkpointIdent b)

-- |Instances of CheckpointAble are able to convert themselves into a checkpoint
class CheckpointAble a where
    toCheckpoint :: a -> Checkpoint

-- |'Coords' to 'Checkpoints'
instance CheckpointAble Coord where
    toCheckpoint c = Checkpoint c (magVarOf c) (AGL 0) Nothing



-- |Lookup or calculate the magnetic variation of the specified coordinate
magVarOf :: Coord -> MagneticVariation
magVarOf _ = 0.0

-- |A LegCheckpoint is a 'Checkpoint' with some altitude information, so
-- we can use it as a leg component in the 'Plan'.
data LegCheckpoint = LegCheckpoint {

    legAltitude   :: Altitude
  , legCheckpoint :: Checkpoint

} deriving (Eq, Show, Read)

data Leg = Leg {

    from :: Checkpoint,
    to :: Checkpoint,
    track :: Track,
    distance :: Range,
    altitude :: Altitude

} deriving (Eq, Read, Show)

--------------------------------------------------------------------------------

-- |CheckpointNumber is indexed from 0 (0=starting checkpoint). 
type CheckpointNumber = Int

-- |Message represents all the operations or commands that the planning
-- manager will support.

data Message = 
    -- |Return the specified checkpoint
    QueryGetCheckpoint CheckpointNumber 

    -- |Return a list of all occurences of the checkpoint from the plan
  | QueryFindOccurences Checkpoint
  | QueryFindOccurencesDeleted Checkpoint

    -- |Return a list of places in the plan that the checkpoint could be
    -- sensibly inserted into, ordering to system planning criteria,
    -- ordered according to best fit.
  | QueryBestFitInsert Checkpoint
  | QueryBestFitInsertPlans Checkpoint
  | QueryDeletePlans Checkpoint

    -- |Return list of checkpoints currently in plan
  | ReportCheckpoints

    -- |Return list of idents of checkpoints currently in plan
  | ReportIdents

    -- |Return the current plan itself
  | ReportPlan

    -- |Return number of checkpoints in the plan (as CheckpointNumberResponse)
  | ReportLength

    -- |Return a list of legs for the current plan
  | ListLegs

    -- |Insert the specified checkpoint at the position indicated. Will not 
    -- insert anything if the checkpoint either side of CheckpointNumber is
    -- the same checkpoint
  | InsertCheckpoint Checkpoint CheckpointNumber

    -- |Delete the specified checkpoint from the plan
  | DeleteCheckpoint CheckpointNumber

    -- |Set the current  plan to the specified plan
  | SetPlan Plan

    -- |Set the current plan according to the list of supplied checkpoints
  | SetPlanCheckpoints [Checkpoint]

    -- |Set the current plan according to the list of supplied identifiers. The
    -- system must be able to un-ambiguously resolve the first identifier from
    -- the list otherwise the operation fails. Subsequent ambiguities in the
    -- identifiers list are resolved by choosing the object closet to the
    -- preceding identifier.
  | SetPlanIdents [Ident]

    -- |Erase all checkpoints from the plan
  | Clear

    -- |Undo the last change to the plan
  | Undo

    -- |Modify the active checkpoint. Returns BoolResponse
  | SetActiveCheckpoint CheckpointNumber

    -- |Query the active checkpoint. Returns CheckpointNumberResponse
  | GetActiveCheckpoint

    -- |De-activate the active checkpoint. Returns BoolResponse (always true)
  | DeactivateCheckpoint

    -- |Return True if a checkpoint is active
  | IsActive

    -- |Return a list of flight plans from the history that match the specified
    -- destination checkpoint
  | SearchHistory Checkpoint

    -- |Dumps the current plan and historic state, as a String
  | DumpState

    -- |Replaces the current plan and historic state
  | RestoreState String

  deriving (Eq, Show, Read)


-- |A Response is returned from the handling function to indicate the results
-- of the operation. The return values correspond to their counterparts in the
-- 'Message' type

data Response =
    CheckpointResponse Checkpoint
  | CheckpointListResponse [Checkpoint]
  | CheckpointNumberResponse CheckpointNumber
  | CheckpointNumberListResponse [CheckpointNumber]

  | IdentListResponse [Ident]
  | PlanResponse Plan
  | PlanListResponse [Plan]
  | LegResponse [Leg]
  | BoolResponse Bool
  | DumpResponse String

  deriving (Eq, Show, Read)

--------------------------------------------------------------------------------

-- |Return the checkpoint at the specified position in the plan. Checkpoint
-- 0 is the first checkpoint.
getCheckpoint :: Plan -> CheckpointNumber -> Checkpoint

getCheckpoint (Plan Nothing) _ = error "not done getCheckpoint"
getCheckpoint plan@(Plan (Just (firstPt,_))) 0 = firstPt

getCheckpoint plan@(Plan (Just (_,cps))) i
  | i > 0 && i <= length cps = legCheckpoint (cps !! (i - 1))
  | otherwise = error "Checkpoint index error"

--------------------------------------------------------------------------------
-- |Convert the given plan into a list of Checkpoints

listCheckpoints :: Plan -> [Checkpoint]
listCheckpoints (Plan Nothing) = []
listCheckpoints (Plan (Just (first, legs))) = first : (map legCheckpoint legs)

--------------------------------------------------------------------------------
-- |Convert the given plans into a list of Legs

toLegs :: Plan -> [Leg]
toLegs (Plan Nothing) = []
toLegs (Plan (Just (first,[]))) = []
toLegs (Plan (Just (first,(l:ls)))) = firstLeg : restOfLegs (l:ls)

  where
    firstLeg = toLeg first l
    restOfLegs (l1:[]) = []
    restOfLegs (l1:l2:ls) = toLeg (legCheckpoint l1) l2 : restOfLegs (l2:ls)
    
toLeg :: Checkpoint -> LegCheckpoint -> Leg
toLeg cp1 lcp2 = Leg cp1 (cp2) (0.0::Track) 
    (coordDistance (checkpointCoord cp1) (checkpointCoord cp2)) (legAltitude lcp2)
    where cp2 = legCheckpoint lcp2

-- |Return list of checkpoint numbers where the specified checkpoint
-- is found
findOccurences :: Plan -> Checkpoint -> [CheckpointNumber]
findOccurences plan cp = map fst (filter ((==cp).snd) (zip [0..] (listCheckpoints plan)))

-- |Works out the best places to insert the specified checkpoint, returns
-- an ordered list of the insert positions.
bestFitInsert :: Plan -> Checkpoint -> [CheckpointNumber]
bestFitInsert plan chkpt = reverse [0..numCheckpoints plan]

-- |Return plan after attempting insert operation. Returns true if the
-- operation succeeds and the plan is modified
insertCheckpoint :: Plan -> Checkpoint -> CheckpointNumber -> Maybe Plan

-- Empty plan, add starting checkpoint
insertCheckpoint (Plan Nothing) chkpt 0 = Just $ Plan (Just (chkpt,[]))

-- Plan with one waypoint, add next waypoint
insertCheckpoint (Plan (Just (firstPt,[]))) chkpt 1 = Just newplan
  where
    newplan = Plan (Just (firstPt,[LegCheckpoint zeroAlt chkpt]))

-- Plan with start, inserting at position 0. Move the start checkpoint up
insertCheckpoint (Plan (Just (firstPt,legs))) chkpt 0 = Just newplan
  where
    newplan = Plan (Just (chkpt,firstPtLeg:legs))
    firstPtLeg
     | (not.null) legs = LegCheckpoint (legAltitude (head legs)) firstPt
     | otherwise = LegCheckpoint zeroAlt firstPt

-- Plan with start and at least one checkpoint, inserting elsewhere. Move the checkpoints up
insertCheckpoint (Plan (Just (firstPt,legs@(c:cs)))) chkpt insPos
 | insPos <= (length legs + 1) = Just newplan
  where
    insPos' = insPos-1
    newplan = Plan (Just (firstPt,(take insPos' legs) ++ [LegCheckpoint zeroAlt chkpt] ++ drop insPos' legs))

-- all other cases... FAIL.
insertCheckpoint plan _ _ = Nothing

zeroAlt = AMSL 0


-- |Delete the specified checkpoint from the plan
deleteCheckpoint :: Plan -> CheckpointNumber -> Maybe Plan

-- invalid delete
deleteCheckpoint (Plan Nothing) _ = Nothing

-- delete first checkpoint from plan
deleteCheckpoint (Plan (Just (firstPt, c:cs))) 0 = Just newPlan
  where newPlan = Plan $ Just (legCheckpoint c,cs)

deleteCheckpoint plan@(Plan (Just (firstPt, legs))) num 
 | num <= length legs = Just newPlan
 | otherwise = Nothing
  where newPlan = Plan $ Just (firstPt, (take (num-1) legs) ++ (drop num legs))

--------------------------------------------------------------------------------
-- |Returns number of checkpoints in the plan

numCheckpoints :: Plan -> CheckpointNumber
numCheckpoints plan = case plan of
    Plan (Just (first,chks)) -> length chks + 1
    Plan Nothing    -> 0

--------------------------------------------------------------------------------
-- |Compute a list of possible plans that use the specified checkpoint

insertPermutations :: Plan -> Checkpoint -> [Plan]
insertPermutations plan chkpt = catMaybes $ map (insertCheckpoint plan chkpt) (bestFitInsert plan chkpt)

deletePermutations :: Plan -> Checkpoint -> [Plan]
deletePermutations plan chkpt = catMaybes $ map (deleteCheckpoint plan) (findOccurences plan chkpt)

--------------------------------------------------------------------------------

-- |Message-based interface for managing navigation functions
interface :: Navigation -> Message -> (Navigation,Response)

interface np@(Navigation plan active history redo) (QueryGetCheckpoint num) = (np, CheckpointResponse $ getCheckpoint plan num)
interface np@(Navigation plan active history redo) (QueryFindOccurences chkpt) = (np, CheckpointNumberListResponse $ findOccurences plan chkpt)
interface np@(Navigation plan active history redo) (QueryBestFitInsert chkpt) = (np, CheckpointNumberListResponse $ bestFitInsert plan chkpt)

interface np@(Navigation plan active history redo) (QueryBestFitInsertPlans chkpt) = (np, PlanListResponse $ insertPermutations plan chkpt)
interface np@(Navigation plan active history redo) (QueryDeletePlans chkpt) = (np, PlanListResponse $ deletePermutations plan chkpt)

interface np@(Navigation plan active history redo) ReportCheckpoints = (np, CheckpointListResponse chks)
  where chks = listCheckpoints plan

interface np@(Navigation plan active history redo) ReportIdents = (np, IdentListResponse (catMaybes $ planIdents plan))
  where
    planIdents (Plan Nothing) = []
    planIdents (Plan (Just (first,chks))) = checkpointIdent first : map (checkpointIdent.legCheckpoint) chks

interface np@(Navigation plan active history redo) ReportPlan = (np, PlanResponse plan)
interface np@(Navigation plan active history redo) ReportLength = (np, CheckpointNumberResponse $ numCheckpoints plan)
  where 

interface np@(Navigation plan active history redo) ListLegs = (np, LegResponse $ toLegs plan)

--------------------------------------------------------------------------------
-- editing operations

interface np@(Navigation plan active history redo) (InsertCheckpoint chkpt num) = (newNav, BoolResponse result)
  where 
    newNav
      | result = np { navPlan = newPlan, navHistory = plan:history }
      | otherwise = np

    (newPlan, result)
      | opresult == Nothing  = (plan, False)
      | otherwise = (fromJust $ opresult, True)
    opresult = insertCheckpoint plan chkpt num

interface np@(Navigation plan active history redo) (DeleteCheckpoint num) = (newNav, BoolResponse result)
  where 
    newNav
      | result = np { navPlan = newPlan, navHistory = plan:history }
      | otherwise = np
    (newPlan, result) 
      | opresult == Nothing = (plan, False)
      | otherwise = (fromJust opresult, True)
    opresult = deleteCheckpoint plan num

interface np@(Navigation plan active history redo) (SetPlan newPlan) = (np {navPlan = newPlan, navHistory = plan:history}, BoolResponse True)

interface np@(Navigation plan active history redo) (SetPlanCheckpoints chkpts) = undefined
interface np@(Navigation plan active history redo) (SetPlanIdents idents) = undefined

interface np@(Navigation plan active history redo) Clear = (np { navPlan = Plan Nothing, navHistory = plan:history}, BoolResponse True )
interface np@(Navigation plan active [] redo) Undo = (np, BoolResponse False)
interface np@(Navigation plan active (p:ps) redo) Undo = (np { navPlan = p, navHistory = ps}, BoolResponse False)

--end of editing operations
--------------------------------------------------------------------------------

interface np@(Navigation {navPlan = Plan Nothing}) (SetActiveCheckpoint _) = (np, BoolResponse False)

interface np@(Navigation {navPlan = (Plan (Just (first,legs)))} ) (SetActiveCheckpoint newCp) 
  | newCp <= length legs = (np {navActive=Just newCp}, BoolResponse True)
  | otherwise = (np, BoolResponse False)

interface np@(Navigation plan active history redo) GetActiveCheckpoint = (np, CheckpointNumberResponse result)
  where result = case active of
                    (Just n) -> n
                    _ -> 0
    
interface np@(Navigation plan active history redo) DeactivateCheckpoint = (np { navActive = Nothing }, BoolResponse True)

interface np@(Navigation plan active history redo) IsActive = (np, BoolResponse res)
  where
    res = case active of
            Nothing -> False
            _ -> True

interface nav (SearchHistory chkpt) = (nav, PlanListResponse $ searchDestinations (navHistory nav) chkpt)
 where
    -- results will include those plans with more than 1 checkpoint, where the final checkpoint
    -- is equal to the one given
    searchDestinations plans chkpt = filter ((==chkpt).lastCheckpoint) (filter ((>1).numCheckpoints) plans )
    lastCheckpoint p = (last.listCheckpoints) p

interface nav DumpState = (nav, DumpResponse $ show nav)

interface _ (RestoreState state) = (read state, BoolResponse True)

