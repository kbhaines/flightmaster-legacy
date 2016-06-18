-------------------------------------------------------------------------------
-- Airspace
-------------------------------------------------------------------------------

module Airspace where

import Coord
import qualified Data.ByteString.Lazy.Char8 as LC

import Data.Binary
import List(tails)

import Graphics hiding (Circle)
import Utils

-------------------------------------------------------------------------------
-- exported functions & data
--

newtype AirspaceDB = AirspaceDB {
    asDB :: [Airspace]
} deriving (Eq, Show, Read)

type AirspaceIDType = Int

data Class = ClassA | ClassB | ClassC | ClassD | ClassE | ClassF | ClassG | SUAS | ClassU
	deriving (Eq, Ord, Show, Read)

data BoundarySegment = 
    Line { lineTo :: !Coord }
  | LeftArc { arcStart :: !Coord, arcCentre :: !Coord, arcRadius :: !Double }
  | RightArc { arcStart :: !Coord, arcCentre :: !Coord, arcRadius :: !Double }
  | Circle { circleCentre :: !Coord, circleRadius :: !Double }
	deriving (Eq, Read, Show)


data Airspace = Airspace {

    lowAlt :: !Altitude,
    upperAlt :: !Altitude,
    asClass :: !Class,
    notes :: !SString,

    -- bounding box of the entire airspace (topLeft, bottomRight)
    bounds :: !(Coord, Coord),

    -- boundary starts at first record and is closed by joining last
    -- record back to the first one.
    boundary :: ![BoundarySegment]

} deriving (Eq, Read, Show)

-- |Represents a record from an airspace database; an identifier
-- and the record itself
newtype AirspaceID = AirspaceID { airspace :: (AirspaceIDType, Airspace) }

airspaceOf :: AirspaceID -> Airspace
airspaceOf = snd.airspace

idOf :: AirspaceID -> AirspaceIDType
idOf = fst.airspace

instance GraphicsNameable AirspaceID where
    toGraphicsName as = GraphicsName (fromIntegral.idOf $ as)

type AsFilter = (Airspace -> Bool)


mkDatabase :: [Airspace] -> AirspaceDB
mkDatabase asl = AirspaceDB asl

dbWithIdents :: AirspaceDB -> [AirspaceID]
dbWithIdents db = (map AirspaceID $ zip [0..] $ asDB db)

-- TODO: loadDatabase can be parameterised type operation, including checks for file
loadDatabase :: String -> IO AirspaceDB
loadDatabase fname = decodeFile fname :: IO AirspaceDB

toList :: AirspaceDB -> [Airspace]
toList = asDB

inBoundsAndFilter :: AirspaceDB -> BoundCoords -> AsFilter -> [AirspaceID]
inBoundsAndFilter asdb bds userFilter = 
    filter (\as -> let as' = airspaceOf as in 
        (userFilter as') && ((boundsOverlap bds).bounds $ as')) (dbWithIdents asdb)


-------------------------------------------------------------------------------
-- draw an airspace boundary
--

airspaceGraphics :: XFormFunction -> Double -> AirspaceID -> [GraphicsCmd]
airspaceGraphics xform radScale as
  | areaOfAirspace as' < 100 = []
  | otherwise = [namedGraphics as $ [Polyline col width stipple bs]]
    where 
        as' = airspaceOf as
        (col,width,stipple) = airspaceStyle as'
        bs = map (pointToPlane 2.0) (boundaryPoints xform radScale (boundary as'))
        areaOfAirspace :: Airspace -> Graphics1d
        areaOfAirspace a = (y1-y2)*(x2-x1)
            where (x1,y1) = xform c1
                  (x2,y2) = xform c2
                  (c1,c2) = bounds a

airspaceStyle :: (Num t, Num t1, Num t2) =>Airspace -> (GraphicsColour, t, Maybe (t1, t2))
airspaceStyle as =
    case asClass as of
        ClassA -> (GraphicsColour3 0.7 0.0 0.0, 2, Nothing)
        ClassD -> (GraphicsColour3 0 0 0.5, 2, Just (1,0xFFF0))
        SUAS -> (GraphicsColour3 0.6 0.0 0.0, 4, Nothing)
        otherwise -> (GraphicsColour3 0.5 0.0 0.5, 2, Nothing)

------------------------------------------------------------------------------
-- compute the points of a list of Airspace.BoundarySegment records
--
-- 'tails' is used because arc segments need to 'read ahead' to get
-- their end point, which is the start of the next segment

boundaryPoints :: XFormFunction -> Double -> [BoundarySegment] -> [Graphics2d]
boundaryPoints xform radScale segs = pts ++ [firstPt]
  where
    firstPt 
     | not $ null pts = head pts 
     | otherwise = error "No points in BP"
    pts = concatMap (bp2 firstPt xform radScale) (tails $ segs)

bp2 :: Graphics2d -> XFormFunction -> Double -> [BoundarySegment] -> [Graphics2d]

-- circle and line transforms
bp2 _ xform radScale [(Circle c r)] = circlePoints (xform c) (dbl2glf $ r*radScale)
bp2 _ xform _ (Airspace.Line l:_) = [xform l]

-- left & right arcs with segments following
bp2 _ xform radScale (RightArc s c r:nextSeg:_) = arcPoints (xform s) (xform $ startOfSeg nextSeg) (xform c) (r*radScale) 
bp2 _ xform radScale (LeftArc s c r:nextSeg:_) = reverse $ arcPoints (xform $ startOfSeg nextSeg) (xform s) (xform c) (r*radScale) 

-- left & right arcs with no segments following: connects arc back to start of boundary
bp2 firstPt xform radScale [RightArc s c r] = arcPoints (xform s) firstPt (xform c) (r*radScale)
bp2 firstPt xform radScale [LeftArc s c r] = reverse $ arcPoints firstPt (xform s) (xform c) (r*radScale)

bp2 _ _ _ [] = []

startOfSeg :: BoundarySegment -> Coord
startOfSeg (LeftArc s _ _) = s
startOfSeg (RightArc s _ _) = s
startOfSeg (Airspace.Line s) = s


-------------------------------------------------------------------------------
-- private functions & data
--

boundaryBox :: [BoundarySegment] -> (Coord, Coord) 
boundaryBox segs = (Coord maxLat minLon, Coord minLat maxLon)
    where maxLat = maximum latitudes
          minLat = minimum latitudes
          maxLon = maximum longitudes
          minLon = minimum longitudes
          latitudes = map coordLat (concatMap segBounds segs)
          longitudes = map coordLon (concatMap segBounds segs)

segBounds :: BoundarySegment -> [Coord]
segBounds (Line l1) = [l1]
segBounds (Circle c r) = 
    [
        Coord (coordLat c+r) (coordLon c - (r/cos(coordLat c))), 
        Coord (coordLat c-r) (coordLon c + (r/cos(coordLat c))) 
    ]

segBounds (LeftArc s c r) = segBounds $ Circle c r
segBounds (RightArc s c r) = segBounds $ Circle c r


-------------------------------------------------------------------------------
-- Instances of Binary for data types
-- (another possible target of generic boilerplate avoidance!)
--

instance Binary AirspaceDB where
    put (AirspaceDB db) = do put db

    get = do 
        db <- get
        return (AirspaceDB db)

instance Binary Airspace where
    put (Airspace low high aClass ns bs b) = do
        put low
        put high
        put aClass
        put ns
        put bs
        put b

    get = do
        low <- get
        high <- get
        aClass <- get
        ns <- get
        bs <- get :: Get (Coord, Coord)
        b <- get
        return (Airspace low high aClass ns (boundaryBox b) b)


instance Binary Altitude where
    put (AMSL alt) = do  
        put (0::Word8)
        put alt

    put (AGL alt) = do
        put (1::Word8)
        put alt

    put (FL alt) = do
        put (2::Word8)
        put alt

    put (NOTAM) = do
        put (3::Word8)

    get = do
            tag <- get :: Get Word8
            case tag of
                0 -> do alt <- get
                        return (AMSL alt)
                1 -> do alt <- get
                        return (AGL alt)
                2 -> do alt <- get
                        return (FL alt)
                3 -> do return (NOTAM)
                _ -> error "Bad altitude type in airspace data"

instance Binary Class where
    put ClassA = do put (0::Word8)
    put ClassB = do  put (1::Word8)
    put ClassC = do  put (2::Word8)
    put ClassD = do  put (3::Word8)
    put ClassE = do  put (4::Word8)
    put ClassF = do  put (5::Word8)
    put ClassG = do  put (6::Word8)
    put SUAS   = do  put (7::Word8)
    put ClassU = do  put (8::Word8)

    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> do return (ClassA)
            1 -> do return (ClassB)
            2 -> do return (ClassC)
            3 -> do return (ClassD)
            4 -> do return (ClassE)
            5 -> do return (ClassF)
            6 -> do return (ClassG)
            7 -> do return (SUAS)
            8 -> do return (ClassU)
            _ -> error "Bad airspace class in airspace data"

instance Binary BoundarySegment where
    put (Line l) = do 
        put (0::Word8) 
        put l

    put (LeftArc s c r) = do 
        put (1::Word8)
        put s
        put c
        put r

    put (RightArc s c r) = do 
        put (2::Word8)
        put s
        put c
        put r

    put (Circle c r) = do
        put (3::Word8)
        put c
        put r

    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> do 
                l <- get
                return (Line l)

            1 -> do
                s <- get
                c <- get
                r <- get
                return (LeftArc s c r)
            2 -> do
                s <- get
                c <- get
                r <- get
                return (RightArc s c r)
            3 -> do
                c <- get
                r <- get
                return (Circle c r)

            _ -> error "Bad segment description in airspace data"
