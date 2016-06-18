-- |Selection

module Selection (


    -- * Functions
  
    interface

    -- * Types

  , Message(..)
  , SelectionMP
  , Response(..)
  , Selection(..)
  , initial

) where

import qualified Navigation as Nav
import Coord
import Data.Maybe
import qualified ObjectDB as DB

import MessageProcessor

--------------------------------------------------------------------------------
-- |Short-hand for Selection message processor

type SelectionMP = MessageProcessor Message Response

--------------------------------------------------------------------------------
-- |Messages that the selection processor will handle

data Message =

    -- |Set the selection as a database object, but we also record the position
    -- that the user selected in a 'Coord'
    SetByObjectReference DB.OBID Nav.Checkpoint

    -- |Set by coordinate
  | SetByCoord Coord

    -- |Set Leg, and checkpoint at end of the leg
  | SetLeg Nav.CheckpointNumber Nav.Checkpoint

    -- |Get the location the user selected (always available if selected)
  | GetAsCheckpoint 

    -- |Get the present selection's Object Identifier (if possible)
  | GetOBID

    -- |Get the selection
  | Get

    -- |Clear the current selection
  | Clear

    deriving (Eq, Read, Show)

--------------------------------------------------------------------------------
-- |Responses sent by the interface function

data Response =
    
    BooleanResponse Bool
  | CheckpointResponse (Maybe Nav.Checkpoint)
  | OBIDResponse (Maybe DB.OBID)
  | SelectionResponse Selection
    deriving (Eq, Read, Show)

--------------------------------------------------------------------------------
-- |The representation of a selection 

data Selection = 
    
    NoSelection

    -- |Object from the database, and the location the user hit
  | DatabaseObject DB.OBID Nav.Checkpoint

    -- |An arbitrary selection point, at the location the user hit
  | Freepoint Nav.Checkpoint

    -- |A leg from the flight plan, and the checkpoint at the end of
    -- that leg. The checkpoint lets us uniquely match against a specific
    -- leg in the plan, as two identical checkpoints cannot be next to
    -- each other in the plan given a specific CheckpointNumber

  | Leg Nav.CheckpointNumber Nav.Checkpoint

    -- ARCH: Synchronise with NavMP when Leg is deleted from flight plan

  deriving (Eq, Read, Show)

initial = NoSelection

--------------------------------------------------------------------------------
-- |Selection interface function, for use by a MessageProcessor

interface :: Selection -> Message -> (Selection, Response)

-- Setting selection
interface _ (SetByObjectReference id chk) = (DatabaseObject id chk, BooleanResponse True)
interface _ (SetByCoord coord) = (Freepoint $ Nav.toCheckpoint coord, BooleanResponse True)
interface _ (SetLeg legNum chkpt) = (Leg legNum chkpt, BooleanResponse True)

-- GetAsCheckpoint
interface sel@(DatabaseObject _ chk) GetAsCheckpoint = (sel, CheckpointResponse $ Just chk)
interface sel@(Freepoint chk) GetAsCheckpoint = (sel,CheckpointResponse $ Just chk)
interface sel@(Leg _ chk) GetAsCheckpoint = (sel,CheckpointResponse $ Just chk)
interface sel GetAsCheckpoint = (sel, CheckpointResponse Nothing)

interface sel@(DatabaseObject id _) GetOBID = (sel, OBIDResponse (Just id))

interface sel GetOBID = (sel, OBIDResponse Nothing)

interface sel Get = (sel, SelectionResponse sel)
interface sel Clear = (NoSelection, BooleanResponse True)
