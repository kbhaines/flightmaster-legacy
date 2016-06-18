module Coord where


import Utils
import Data.Binary
import Graphics
import Text.Printf

-------------------------------------------------------------------------------

type XFormFunction = (Coord -> Graphics2d)
type ReverseXFormFunction = (Graphics2d -> Coord)

-------------------------------------------------------------------------------

rad2deg :: Double -> Double
rad2deg n = (n/pi)*180

rad2nm :: Double -> Double
rad2nm n = (rad2deg n)*60

deg2rad :: Double -> Double
deg2rad d = (d/180)*pi

nm2rad :: Double -> Double
nm2rad n = deg2rad (n/60)

-------------------------------------------------------------------------------
-- Coordinate type, representing latitude/longitude pair
--

--
-- Some type definitions in order to aid readability
--

type Radians = Double

type Range = Radians
type Track = Radians
type Time = Int          -- seconds
type Latitude = Radians  -- +ve = north
type Longitude = Radians -- +ve = east
type MagneticVariation = Radians -- +ve = east
type Speed = Radians     -- Speed in radians/second
type NauticalMiles  = Double
type Degrees = Double
type Ident = String

-- |Altitudes can be specified in different ways
data Altitude = 
    AMSL { altitude :: !Int } 
  | AGL { altitude :: !Int }
  | FL { fl :: !Int }
  | NOTAM 
	deriving (Eq, Show, Read)


data Coord = Coord !Latitude !Longitude
  deriving (Show, Read, Ord, Eq)

type BoundCoords = (Coord, Coord)

coordLat :: Coord -> Latitude
coordLat (Coord lat _) = lat

coordLon :: Coord -> Longitude
coordLon (Coord _ lon) = lon

coordDeg lat lon = Coord (deg2rad lat) (deg2rad lon)
coordDegMin (dlat, mlat) (dlon, mlon) = Coord (deg2rad (dlat + mlat/60)) (deg2rad (dlon + mlon/60))

boundTopLeft :: BoundCoords -> Coord
boundTopLeft (tl,_) = tl

boundBottomRight :: BoundCoords -> Coord
boundBottomRight (_,br) = br

boundsOverlap :: BoundCoords -> BoundCoords -> Bool
boundsOverlap (Coord top1 left1, Coord bot1 right1) (Coord top2 left2, Coord bot2 right2) =
    not ((bot1 > top2) || (top1 < bot2) || (right1 < left2) || (left1 > right2))

-- returns true if the coordinates are within 1 foot (at the equator) of each other
closeTo :: Coord -> Coord -> Bool
closeTo (Coord lat1 lon1) (Coord lat2 lon2) =
    withinFoot lat1 lat2 && withinFoot lon1 lon2

withinFoot :: Double -> Double -> Bool
withinFoot n1 n2 = (abs (n1-n2) < deg2rad (1/(60*6074)))

coordInBounds :: BoundCoords -> Coord -> Bool
coordInBounds (Coord top left, Coord bot right) (Coord lat lon) = 
    (lat > bot && lat < top && lon > left && lon < right)

-- difference in latitude between two Coords
coordLatDelta:: Coord -> Coord -> Double
coordLatDelta c1 c2 = coordLat c2 - coordLat c1
 
-- great circle distance between Coords, radians
coordDistance :: Coord -> Coord -> Double
coordDistance c1 c2 = 
  acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon1-lon2))
  where (lat1,lon1)=(coordLat c1, coordLon c1)
        (lat2,lon2)=(coordLat c2, coordLon c2)

-- parse a coordinate in format [NS]DDMMSSss[WE]DDDMMSSss where DD=Degrees,
-- MM=Minutes SS=Seconds and ss=100ths of second
mkCoord :: SString -> Coord
mkCoord s 
  | slength s == 19 = Coord lat lon 
  | otherwise = error "Bad coordinate passed to mkCoord"
    where
        lat = decodeLatLonString $ subSStr s 1 9
        lon = decodeLatLonString $ subSStr s 10 10

        decodeLatLonString lls = 
            case lls `sindex` 0 of
                'N' -> deg2rad(d)
                'E' -> deg2rad(d)
                'S' -> deg2rad(0-d)
                'W' -> deg2rad(0-d)
                otherwise -> error "Bad hemispehere passed to mkCoord"
            where
                d=deg + mins/60 + secs/3600 + secs100/360000
                deg = fromIntegral (n `div` 1000000)
                mins = fromIntegral((n `div` 10000) `mod` 100)
                secs = fromIntegral((n `div` 100) `mod` 100)
                secs100 = fromIntegral(n `mod` 100)
                n = case (sreadInt $ stail lls) of
                        Just (x,_) -> x
                        Nothing -> error "Bad magnitude passed to mkCoord"

unCoord :: Coord -> String
unCoord c = printf "%c%02d%02d%04d%c%03d%02d%04d" ns latd latm lats we lond lonm lons
    where
        ns = if coordLat c < 0 then 'S' else 'N'
        we = if coordLon c < 0 then 'W' else 'E'
        latsecs = round $ 360000*(abs.rad2deg.coordLat) c :: Int
        lonsecs = round $ 360000*(abs.rad2deg.coordLon) c :: Int
        (latd,latm,lats) = (latsecs `div` 360000, latsecs `div` 6000 `mod` 60, latsecs `mod` 6000)
        (lond,lonm,lons) = (lonsecs `div` 360000, lonsecs `div` 6000 `mod` 60, lonsecs `mod` 6000)
        

-------------------------------------------------------------------------------
-- Binary import and export
--
-- We may want to consider automatic derivation using the Scrap Your
-- Boilerplate generics module. Mind you, this one isn't difficult ;-)
--

instance Binary Coord where
    put (Coord lat lon) = do
        put lat
        put lon

    get = do 
        lat <- get
        lon <- get
        return (Coord lat lon)

