-- |Object Database

module ObjectDB (

    -- * Functions
  
    interface
    -- * Types
  , ObjectDB(..)
  , OBID(..)
  , Object(..)
  , ObjectDBMP
  
  , Message(..)
  , Response(..)

) where

import Waypoint
import Airspace
import Navigation(Checkpoint)
import Coord
import MessageProcessor

--------------------------------------------------------------------------------
-- |Short-hand for Object message processor

type ObjectDBMP = MessageProcessor Message Response

--------------------------------------------------------------------------------
-- | Object database comprises other databases

data ObjectDB = ObjectDB {

    waypointDB :: WaypointDB 
  , airspaceDB :: AirspaceDB

} deriving (Eq, Read, Show)


--------------------------------------------------------------------------------
-- |Object Identifiers for database objects

data OBID = 

    WaypointOBID WaypointIDType
  | AirspaceOBID AirspaceIDType

  deriving (Eq, Show, Read)


--------------------------------------------------------------------------------
-- |Objects that the database holds

data Object =

    WaypointObject (WaypointIDType, Waypoint)
  | AirspaceObject (AirspaceIDType, Airspace)
  
  deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- |Messages that the database will respond to

data Message =

    -- |Returns response 'ObjectList'
    FindByIdent Ident

    -- |Returns response 'Object'
  | FindByCheckpoint Checkpoint

    -- |Returns response 'Object'
  | GetObject OBID

    -- |Returns 'ObjectListResponse'
  | ObjectsInBounds BoundCoords

  | GetDB

  deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- |Responses to the messages
data Response =

    NotFound
  | ObjectIdResponse (Maybe OBID)
  | ObjectResponse (Maybe Object)
  | ObjectIDListResponse [OBID]
  | ObjectListResponse [Object]
  | DB ObjectDB

  deriving (Eq, Show, Read)



--------------------------------------------------------------------------------
-- |Interface function, allowing ObjectDB to be a MessageProcessor

interface :: ObjectDB -> Message -> (ObjectDB, Response)

interface objdb (FindByIdent ident) = (objdb, ObjectListResponse $ map wpidToObject $ findAllByIdent (waypointDB objdb) ident)

interface objdb (FindByCheckpoint chk) = error "blau"

interface objdb (GetObject (WaypointOBID id)) = (objdb, 
    ObjectResponse (Just $ WaypointObject (id, findByID (waypointDB objdb) id)))

interface objdb (GetObject (AirspaceOBID id)) = error "Airspace get not implemented"

interface objdb (ObjectsInBounds bounds) = (objdb, ObjectListResponse $ waypoints ++ airspaces)
  where
    waypoints = map wpidToObject $ inBounds (waypointDB objdb) bounds
    airspaces = map airspaceToObject $ inBoundsAndFilter (airspaceDB objdb) bounds (\a -> True)

interface objdb GetDB = (objdb, DB objdb)

--------------------------------------------------------------------------------
-- |Convert waypointID to Object

wpidToObject :: WaypointID -> Object
wpidToObject (WaypointID (id, wp)) = WaypointObject (id, wp)

--------------------------------------------------------------------------------
-- |Convert airspace to Object

airspaceToObject :: AirspaceID -> Object
airspaceToObject as = AirspaceObject (0, airspaceOf as)

