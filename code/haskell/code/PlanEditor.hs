{-# LANGUAGE NamedFieldPuns #-}

-- |Module information

module PlanEditor (

    EditMP
  , Message(..)
  , Response(..)

  , interface
  , initial

) where

import qualified MessageProcessor as MP
import Navigation(Checkpoint, Plan, CheckpointNumber, insertPermutations, deletePermutations, deleteCheckpoint)
import Data.Maybe

type EditMP = MP.MessageProcessor Message Response

--------------------------------------------------------------------------------
-- | Messages that the editor will respond to
data Message = 

    -- | Set the toPlans, base plan and the editing checkpoint, used to
    -- generate planning permutations
    SetEditCheckpoint [Plan] Plan Checkpoint

    -- |Set the editor to get ready to delete the specified leg
  | SetEditLeg Plan CheckpointNumber

  | ClearCheckpoint

    -- | Returns the next planning option from the possible
    -- plan permutations (using the set checkpoint)
  | NextAddOption

    -- | Returns the next planning option where the set checkpoint
    -- has been deleted.
  | NextDeleteOption

    -- |Returns the next planning option from the toPlans
  | NextToOption

    -- |Return count of requests made to Next*
  | GetRequestCount

 deriving (Eq, Read, Show)

--------------------------------------------------------------------------------
-- | Responses from the editor
data Response = 

    -- | Boolean
    BoolResponse Bool

    -- | Planning response to Next* messages; a 'Plan' and the number
    -- of plans in the list for that edit operation
  | PlanResponse Plan Int

    -- |Supported edit operations (response to SetEditCheckpoint)
    -- Response means (Plan?,Delete?,To?)
  | SupportedOps (Bool,Bool,Bool)

    -- |Count response
  | RequestCount Int

  deriving (Eq, Read, Show)

--------------------------------------------------------------------------------
-- |Editor state representation

data Editor = Editor {

    -- |List of plans with a new checkpoint inserted in various positions
    insertPlans :: [Plan]

    -- |List of plans with the checkpoint deleted
  , deletePlans :: [Plan]

    -- |List of plans with the checkpoint as their destination, from the
    -- history in the NavigationMP 
  , toPlans :: [Plan]

    -- |Count of the number of Next* requests that have been made, to assist
    -- the UI with knowning when an Undo is required prior to replacing the
    -- current plan
  , count :: Int

} deriving (Eq, Read, Show)

initial = Editor [] [] [] 0

--------------------------------------------------------------------------------
-- |Interface function for handling messages

interface :: MP.MessageHandler Editor Message Response
interface editor (SetEditCheckpoint toPlans basePlan chk) = (Editor insPlans delPlans toPlans 0, 
    SupportedOps (not $ null insPlans, not $ null delPlans, not $ null toPlans))
 where
    insPlans = insertPermutations basePlan chk
    delPlans = deletePermutations basePlan chk

interface editor (SetEditLeg basePlan legNumber) = (Editor [] delPlans [] 0, SupportedOps (False, True, False))
 where
    delPlans
      | tryDelete /= Nothing = [fromJust tryDelete]
      | otherwise = []
    tryDelete = deleteCheckpoint basePlan legNumber

interface editor ClearCheckpoint = (initial, BoolResponse True)

interface editor@(Editor {insertPlans = ip, count=c}) NextAddOption = (editor { insertPlans = rotate ip, count = c+1 }, PlanResponse (head ip) $ length ip)

interface editor@(Editor {deletePlans = dp, count=c}) NextDeleteOption = (editor { deletePlans = rotate dp, count = c+1 }, PlanResponse (head dp) $ length dp)

interface editor@(Editor {toPlans = tp, count=c}) NextToOption = (editor { toPlans = rotate tp, count = c+1 }, PlanResponse (head tp) $ length tp)

interface editor@(Editor { count }) GetRequestCount = (editor, RequestCount count)

--------------------------------------------------------------------------------
-- |Rotate a list to the left, by putting the head of the list to the end

rotate :: [a] -> [a]
rotate [] = []
rotate (a:as) = as ++ [a]
