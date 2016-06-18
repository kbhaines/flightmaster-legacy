module Waypoint(

    WaypointDB(..),
    Waypoint(..),
    WaypointType(..),
    NavType(..),

    WaypointID(..),
    WaypointIDType,
    waypointData,
    waypointID,
    mkDatabase,
    loadDatabase,
    addWaypoint,
    toList,
    mkWaypoint,
    unWaypoint,
    distance,
    inCircle,
    inBounds,

    waypointGraphics,

    findByIdent,
    findAllByIdent,
    findByID,
    identIsValid

)


where

import Data.List
import List
import Coord
import Utils
import Runway
import Maybe

import Data.Binary

import Data.Char
import Data.IORef
import Text.Printf

--import Graphics.Rendering.OpenGL hiding (get)
--import Graphics.UI.GLUT hiding (get)
import Graphics

import MapNames

import Navigation(magVarOf, Checkpoint(..), CheckpointAble, toCheckpoint)

data WaypointDB = WaypointDB {

   wpDB :: [Waypoint]

} deriving (Eq, Show, Read)

type WaypointIDType = Int

-- |WaypointID is a union of the waypoint ID and the actual Waypoint data itself
newtype WaypointID = WaypointID (WaypointIDType, Waypoint) deriving (Show, Read, Eq, Ord)

-- |Instance declaration of 'GraphicsNameable' for a 'WaypointID', where we use
-- the WaypointIDType field to uniquely identify the waypoint
instance GraphicsNameable WaypointID where
    toGraphicsName (WaypointID(wpid, _)) = GraphicsName $ fromIntegral wpid

-- |Instance declaration of 'CheckpointAble' for a 'WaypointID', to convert
-- to a 'Checkpoint' type
instance CheckpointAble WaypointID where
    toCheckpoint (WaypointID (_, wp)) = toCheckpoint wp

-- |Instance declaration of 'CheckpointAble' for a 'Waypoint'
instance CheckpointAble Waypoint where
    toCheckpoint wp = Checkpoint (coord wp) (magVarOf $ coord wp) (AGL 0) (Just $ unSString $ identifier wp)

-- |Get the waypoint information
waypointData :: WaypointID -> Waypoint
waypointData (WaypointID (_,x)) = x

-- |Get the waypoint identifier
waypointID :: WaypointID -> WaypointIDType
waypointID (WaypointID (x,_)) = x

dbWithIdents db = (map WaypointID $ zip [0..] $ wpDB db)

-- |Waypoints, airfields navaids etc
data Waypoint = Waypoint {

  coord :: !Coord,
  identifier :: !SString,
  name  :: !SString,

  info  :: !WaypointType
    
} deriving (Show, Read, Ord)

-- |Instance 'Eq' for 'Waypoint'
instance Eq Waypoint where
    wp1 == wp2 = (identifier wp1 == identifier wp2) && (coord wp1 == coord wp2)

-- |Extra type-specific information for waypoints
data WaypointType = 
    Airfield { runways :: ![Runway], frequencies :: ![Int] } 
 |  NavAid { navaidType :: !NavType, frequency :: !Int } 
 |  NoType
  deriving (Show, Read, Ord, Eq)

-- |Navaid-specific type information
data NavType = VOR | VORDME | VORTAC | NDB | DME
  deriving (Show, Read, Eq, Ord)


-- |Convert list of waypoints to database
mkDatabase :: [Waypoint] -> WaypointDB
mkDatabase wps = WaypointDB wps

-- TODO: loadDatabase can be parameterised type operation, including checks for file
loadDatabase :: String -> IO WaypointDB
loadDatabase fname = decodeFile fname :: IO WaypointDB

toList :: WaypointDB -> [Waypoint]
toList = wpDB 

addWaypoint :: WaypointDB -> Waypoint -> WaypointDB
addWaypoint (WaypointDB wpdb) wp = WaypointDB (wp:wpdb)

-- Generate a waypoint from string input: "EGBP N51401700W002010100 Kemble"
mkWaypoint :: String -> Waypoint
mkWaypoint str 
 | (length.words) str == 3 = Waypoint c ident name NoType
 | otherwise = error "Invalid waypoint in mkWaypoint"
    where c = mkCoord (str' !! 1)
          ident = str' !! 0
          name = str' !! 2
          str' = map mkSString (words str)

unWaypoint :: Waypoint -> String
unWaypoint wp = printf "%s %s %s" (unSString $ identifier wp) ((unCoord.coord) wp) (unSString $ name wp)

-- distance between two waypoints
distance:: Waypoint -> Waypoint -> Double
distance wp1 wp2 = coordDistance (coord wp1) (coord wp2)

-- creates list of waypoints in rng of c1
inCircle :: WaypointDB -> Coord -> Double -> [WaypointID]
inCircle wpdb c1 rng =
   filter (\(WaypointID (_,x)) -> (abs(coordLatDelta c1 (coord x)) < rng && coordDistance c1 (coord x) < rng)) $ dbWithIdents wpdb

inBounds :: WaypointDB -> BoundCoords -> [WaypointID]
inBounds wpdb bnds = filter (\(WaypointID (_,w)) -> coordInBounds bnds (coord w)) $ dbWithIdents wpdb

-- creates sorted list of waypoints within rng of c1
--wpAndRanges :: WaypointDB -> Coord -> Double -> [(Double, Waypoint)]
--wpAndRanges wpdb c1 rng =
  --zip (map (Waypoint c1 emptySString emptySString NoType) wps1) wps1
  --where
    --wps1 = inCircle wpdb c1 rng

-- returns nearest n waypoints to position c1, sorted
--wpNearestN :: WaypointDB -> Int -> Coord -> [(Double, Waypoint)]
--wpNearestN wpdb n c1 =
  --take n $ sort $ wpAndRanges wpdb c1 (deg2rad 100/60)

findByID :: WaypointDB -> WaypointIDType -> Waypoint
findByID wpdb wpid = (wpDB wpdb) !! wpid

findByIdent :: WaypointDB -> String -> Maybe WaypointID
findByIdent wpdb id = find (\(WaypointID (_,x)) -> identifier x == mkSString id) $ dbWithIdents wpdb

findAllByIdent :: WaypointDB -> String -> [WaypointID]
findAllByIdent wpdb id = filter (\(WaypointID (_,x)) -> identifier x == mkSString id) $ dbWithIdents wpdb

identIsValid :: WaypointDB -> String -> Bool
identIsValid wpdb id = (findByIdent wpdb id /= Nothing)

isAirfield :: Waypoint -> Bool
--isAirfield (Waypoint _ _ (Airfield _ _ ))  = True
--isAirfield (Waypoint _ _ _) = False

isAirfield wp =
  case info wp of
    Airfield _ _ -> True
    _ -> False

wpToPair :: Waypoint -> (SString, Waypoint)
wpToPair w = (identifier w, w)

waypointGraphics :: XFormFunction -> Double -> WaypointID -> [GraphicsCmd]
waypointGraphics xform latScale mywp@(WaypointID (wpid,wp)) = [namedGraphics mywp (label:icon)]
 where
    icon =
        case wtype of
            Airfield rwys _ ->
                if iconRadius > 25 then 
                    runwayActions rwys xform ++ [Circle (x,y,z) circleSize (GraphicsColour3 0.6 0 0.6)]
                  else 
                    [Circle (x,y,z) (min iconRadius circleSize) (GraphicsColour3 0.6 0 0.6)]

            NavAid _ _ -> [Square (x,y,z) (min iconRadius circleSize) (GraphicsColour3 0 0 0.6), 
                            Circle (x,y,z) ((min iconRadius circleSize)/2) (GraphicsColour3 0 0 0.6)]

            _ -> [Translate (x,y,-z) [Polygon (GraphicsColour3 0.2 0.2 0.2) 1 Nothing (UntexturedPolygon [(0,limited,0),(limited,0,0),(0,0-limited/2,0),(-limited,0,0)])]]

    label = if (iconRadius > 5) then 
                RasterText 2 (x-14, y+12,z+12) (GraphicsColour3 0 0 0) tag
              else
                NullOp

    -- 1nm radius
    iconRadius = nm2rad 1 * latScale
    circleSize = 12.0
    limited = min iconRadius circleSize

    ((x,y),z) = (xform $ coord wp,(20.0::Graphics1d))
    tag = unSString $ identifier wp
    wtype = info wp

mkWaypointGLName :: Waypoint -> GraphicsName
mkWaypointGLName wp = GraphicsName (fromIntegral $ sum $ map ord (unSString $ identifier wp)) 

-------------------------------------------------------------------------------
-- Binary import and export
--
-- We may want to consider automatic derivation using the Scrap Your
-- Boilerplate generics module.
--

instance Binary Waypoint where
    put (Waypoint c ident name info) = do 
        put c
        put ident
        put name
        put info

    get = do
        c <- get
        ident <- get
        name <- get
        info <- get
        return (Waypoint c ident name info)

instance Binary WaypointDB where
    put (WaypointDB w) = do
        put w

    get = do
        db <- get
        return (WaypointDB db)

instance Binary WaypointType where
    put (Airfield r f) = do
        put (0 :: Word8)
        put r
        put f

    put (NavAid ty freq) = do
        put (1 :: Word8)
        put ty
        put freq

    put (NoType) = do
        put (9 :: Word8)

    get = do 
            tag <- get :: Get Word8
            case tag of
                0 -> do r <- get
                        f <- get
                        return (Airfield r f)
                1 -> do ty <- get
                        freq <- get
                        return (NavAid ty freq)

                otherwise -> return (NoType)

instance Binary NavType where
    put VOR = put (0 :: Word8)
    put VORDME = put (1 :: Word8)
    put VORTAC = put (2 :: Word8)
    put NDB = put (3 :: Word8)
    put DME = put (4 :: Word8)

    get = do
        ty <- get :: Get Word8
        case ty of
            0 -> return (VOR)
            1 -> return (VORDME)
            2 -> return (VORTAC)
            3 -> return (NDB)
            4 -> return (DME)
            otherwise -> return (VOR)
