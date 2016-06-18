-------------------------------------------------------------------------------
-- ARINC 424 parsing
-------------------------------------------------------------------------------

module Arinc424 (
    arincConvertWaypoints,
    arincConvertAirspace,
    getCoord,
    a424Coord
)

where

--import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy as Lazy
import List
import Data.Binary

import Coord
import Utils
import Runway
import qualified Waypoint as WP
import Airspace
import Maybe

-------------------------------------------------------------------------------
--
-- Terminology:
--
-- A424 record:
-- Each line in an ARINC424 file is a record
--
-- A424 Super-record:
-- Group of continuous A424 records that refer to the same real-life object,
-- e.g. a runway usually comprises 3x A424 records
--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- exported functions
--

arincConvertWaypoints :: FilePath -> FilePath -> [String] -> IO ()
arincConvertWaypoints input output areas = do
    runways <- runwayPass areasPacked input
    waypoints <- waypointPass areasPacked runways input
    encodeFile output (WP.mkDatabase waypoints)
    return ()
    where areasPacked = map LC.pack areas


arincConvertAirspace :: FilePath -> FilePath -> [String] -> IO ()
arincConvertAirspace input output areas = do
    airspace <- airspacePass areasPacked input
    encodeFile output airspace
    return ()

    where areasPacked = map LC.pack areas

-------------------------------------------------------------------------------
-- private functions & data
--

type LinkedRunway = (LC.ByteString, Runway)
type ArincLine = LC.ByteString

data ARINCRecord = ATNavAidVHF | ATNavAid | ATAirfield | ATCommFreq | ATRunway | ATAirspace | ATNothing 
    deriving (Show, Read, Eq)


-------------------------------------------------------------------------------
--
-- this block of code (and arincConvertWaypoints2, above) uses a consumer model
-- of passing around the *rest* of the lines that haven't been used, in a
-- tail-recursive manner.  The result list is built using an accumulator, that
-- is also passed into each invocation of the function.
--
-- It's quite an elegant concept, which reduces memory usage by about 25%, at
-- the cost of a slight increase in runtime. 
--

-------------------------------------------------------------------------------
-- extracts runway records for the area, from the specified input file
--

runwayPass :: [ArincLine] -> FilePath -> IO [LinkedRunway]
runwayPass areasPacked input = do
    inputRecords <- Lazy.readFile input
    let lns = filter (\x -> (filterByArea areasPacked x) && fst (arincRecordType x) == ATRunway) (LC.lines inputRecords)
    return (processRunways [] lns)

--
-- uses a tail-recursive implementation of file processing, with an accumulator
-- variable for the result
--

processRunways :: [LinkedRunway] -> [ArincLine] -> [LinkedRunway]

processRunways accRwys [] = accRwys 

processRunways accRwys (l1:lns)
  | recType == ATRunway = (processRunways (rw:accRwys) rest)
  | otherwise = processRunways accRwys lns
  where
    recType = fst (arincRecordType l1)
    rw = fromJust $ readRunway superRecord
    (superRecord,rest) = getSuperRecord (l1:lns)



-------------------------------------------------------------------------------
-- extracts waypoint records for the area, from the specified input file
-- and uses the runway records to create a complete waypoint record
--

waypointPass :: [LC.ByteString] -> [LinkedRunway] -> FilePath -> IO [WP.Waypoint]
waypointPass areasPacked rwys input = do
    inputRecords <- Lazy.readFile input
    let lns = filter (filterByArea areasPacked) (LC.lines inputRecords)
    return (processWaypoints rwys [] lns)

--
-- uses a tail-recursive implementation of file processing, with an accumulator
-- variable for the result
--

processWaypoints :: [LinkedRunway] -> [WP.Waypoint] -> [ArincLine] -> [WP.Waypoint]

processWaypoints rwys accWps [] = accWps

processWaypoints rwys accWps (l1:lns) 
  | (recType == ATNavAidVHF || recType == ATAirfield) = processWaypoints rwys (wp:accWps) rest
  | otherwise = processWaypoints rwys accWps lns
  where
    recType = fst (arincRecordType l1)
    wp = case arincRecordType l1 of
            (ATAirfield,1) -> fromJust $ readAirfield rwys superRecord
            (ATNavAidVHF,1) -> fromJust $ readVHFNavaid superRecord
            _ -> error "ERROR: processWaypoints"
    (superRecord, rest) = getSuperRecord (l1:lns)


-------------------------------------------------------------------------------
-- extracts airspace records for the area from the specified input file
--

airspacePass :: [LC.ByteString] -> FilePath -> IO [Airspace]
airspacePass areasPacked input = do
    inputRecords <- Lazy.readFile input
    let lns = filter (filterByArea areasPacked) (LC.lines inputRecords)
    return (processAirspace [] lns)
    
processAirspace :: [Airspace] -> [ArincLine] -> [Airspace]

processAirspace accAirspace [] = accAirspace

processAirspace accAirspace allLines@(l1:lns)
  | (recType == ATAirspace) = processAirspace (airspace:accAirspace) rest
  | otherwise = processAirspace accAirspace lns
  where
    recType = fst (arincRecordType l1)
    airspace = fromJust $ a424airspace superRecord
    (superRecord,rest) = getSuperRecord allLines

a424airspace :: [ArincLine] -> Maybe Airspace
a424airspace lns = Just $ Airspace low high cls n b segs
  where
    low = a424airspaceAltitude (mySubStr (head lns) 82 6)
    high = a424airspaceAltitude (mySubStr (head lns) 88 6)
    cls = a424airspaceClass $ head lns
    n = emptySString
    b = boundaryBox segs
    segs = a424airspaceSegments lns

a424airspaceAltitude :: LString -> Altitude
a424airspaceAltitude str = 
    if (str `LC.index` 0 == 'F') then 
        FL $ value 3 3
    else if (char 1 str == 'U') then
        AMSL 99999
    else
        case (str `LC.index` 5) of
            'M' -> AMSL $ value 1 5
            'A' -> AGL $ value 1 5
            _ -> AGL 0

    where 
        value n l = case (LC.readInt $ mySubStr str n l) of
                        Just (x,_) -> x
                        Nothing -> 0

a424airspaceClass :: ArincLine -> Class
a424airspaceClass ln = 
    if (ln `LC.index` 5) == 'C' then
        case (ln `LC.index` 16) of
        'A' -> ClassA
        'B' -> ClassB
        'C' -> ClassC
        'D' -> ClassD
        'E' -> ClassE
        'F' -> ClassF
        'G' -> ClassG
        _ -> ClassU
    else
        SUAS


a424airspaceSegments :: [ArincLine] -> [BoundarySegment]
a424airspaceSegments (ln:lns)
  | elemIndex (char 26 ln) "LHB " /= Nothing = 
    case char 31 ln of 
        'G' -> Line (getCoord ln 33):rest
        'H' -> Line (getCoord ln 33):rest
        'R' -> RightArc (getCoord ln 33) (getCoord ln 52) radius:rest
        'L' -> LeftArc (getCoord ln 33) (getCoord ln 52) radius:rest
        'C' -> Circle (getCoord ln 52) radius:rest
        _ -> a424airspaceSegments lns
        -- _ -> error ("Unrecognised segment:\n"++(LC.unpack ln))

  | otherwise = a424airspaceSegments lns

    where radius = nm2rad ((fromIntegral $ getIntFromStr ln 71 4)/10)
          rest = if char 32 ln == 'E' then [] else a424airspaceSegments lns

a424airspaceSegments [] = []

-------------------------------------------------------------------------------
-- 
-- returns a tuple (r,rs) where r is the super-record set of ArincLines forming
-- a complete record, and rs is the rest of the list
--

getSuperRecord :: [ArincLine] -> ([ArincLine],[ArincLine])
getSuperRecord (r:rs) = (superRecord, rest)
    where
        superRecord = (r:r2on)
        (r2on,rest) = break (isNewRecord srType) rs
        srType = fst (arincRecordType r)

isNewRecord :: ARINCRecord -> ArincLine -> Bool
isNewRecord t str = (fst at /= t) || (snd at == 1)
    where at = arincRecordType str

-------------------------------------------------------------------------------
-- takes a list of A424 records and returns the ones that are in 
-- the area(s) specified

filterByArea :: [LC.ByteString] -> ArincLine -> Bool
filterByArea [] ln = False
filterByArea (area:areas) ln = if (mySubStr ln 2 3) == area then
                                    True
                                  else
                                    filterByArea areas ln

-------------------------------------------------------------------------------
-- takes an A424 record and returns the (type,ind) tuple for it, where ind
-- is the continuation record indicator, which is 1 if the record is the
-- first record of an A424 super-record

arincRecordType :: ArincLine -> (ARINCRecord, Int)
arincRecordType s = (t, cont)  -- cont is the continuation record indicator
    where
    t = case s `LC.index` 4 of
      'D' -> if (s `LC.index` 5 ) == ' ' then ATNavAidVHF else ATNothing -- TODO: non-VHF
      'P' -> case s `LC.index` 12 of
               'G' -> ATRunway
               'V' -> ATNothing -- TODO: ATCommFreq
               'A' -> ATAirfield
               _ -> ATNothing
      --'U' -> if char 6 s == 'C' then ATAirspace else ATNothing
      'U' -> if (char 6 s == 'C') || (char 6 s == 'R') then ATAirspace else ATNothing
      otherwise -> ATNothing

    cont = case t of
        ATNavAidVHF -> read [(s `LC.index` 21)]
        ATNavAid -> read [(s `LC.index` 21)]
        ATAirfield -> read [(s `LC.index` 21)]
        ATCommFreq -> read [(s `LC.index` 25)] -- TODO: analyse frequencies
        ATRunway -> read [(s `LC.index` 21)]
        ATAirspace -> case (LC.readInt $ mySubStr s 23 3) of
                        Just (x,_) -> if x == 101 then 1 else x
                        Nothing -> 0
        _ -> 0

-------------------------------------------------------------------------------
-- parse a coordinate in format [NS]DDMMSSss[WE]DDDMMSSss where DD=Degrees,
-- MM=Minutes SS=Seconds and ss=100ths of second

a424Coord :: ArincLine -> Coord
a424Coord s = 
    Coord lat lon 
    where
        lat = decodeLatLonString $ mySubStr s 1 9
        lon = decodeLatLonString $ mySubStr s 10 10

        decodeLatLonString lls = 
            case lls `LC.index` 0 of
                'N' -> deg2rad(d)
                'E' -> deg2rad(d)
                'S' -> deg2rad(0-d)
                'W' -> deg2rad(0-d)
                otherwise -> 0  -- TODO: throw error
            where
                d=deg + mins/60 + secs/3600 + secs100/360000
                deg = fromIntegral (n `div` 1000000)
                mins = fromIntegral((n `div` 10000) `mod` 100)
                secs = fromIntegral((n `div` 100) `mod` 100)
                secs100 = fromIntegral(n `mod` 100)
                n = case (LC.readInt $ LC.tail lls) of
                        Just (x,_) -> x
                        Nothing -> 0 -- TODO: throw error

getCoord :: ArincLine -> Int -> Coord
getCoord s i = a424Coord (mySubStr s i 19)

-------------------------------------------------------------------------------
-- returns a NavAid record from A424 Super-record
--

readVHFNavaid :: [ArincLine] -> Maybe WP.Waypoint
readVHFNavaid (rec1:rec2:rs) = 
    case arincRecordType rec1 of
        (ATNavAidVHF,1) -> Just (WP.Waypoint c1 ident name (WP.NavAid ntype freq))
        _ -> Nothing
    
    where c1 = getCoord rec1 33
          ident = (mkSString.unLString) $ LC.takeWhile (/=' ') $ mySubStr rec1 14 6
          ntype = case (LC.unpack $ mySubStr rec1 28 2) of
                    "VD" -> WP.VORDME
                    "V " -> WP.VOR
                    " D" -> WP.DME
                    otherwise -> WP.VOR  -- TODO: change to Nothing??

          freq = case (LC.readInt $ mySubStr rec1 23 5) of
                    Just (x,_) -> x
                    Nothing -> 0

          name = (mkSString.unLString) $ LC.takeWhile (/=' ') $ mySubStr rec2 24 12

readVHFNavaid _ = Nothing


-------------------------------------------------------------------------------
-- returns runway record from A424 Super-record
--

readRunway :: [ArincLine] -> Maybe LinkedRunway
readRunway (rec1:rec2:rs)
    | arincRecordType rec1 == (ATRunway,1) = Just (airportIdent, (Runway ident coord length width elev))
    | otherwise = Nothing
    where
        airportIdent = mySubStr rec1 7 4
        ident = (mkSString.unLString) $ LC.takeWhile(/=' ') $ mySubStr rec1 16 3
        length = case (LC.readInt $ mySubStr rec1 23 5) of
            Just (x,_) -> x
            Nothing -> 0
        width = case (LC.readInt $ mySubStr rec1 78 3) of
            Just (x,_) -> x
            Nothing -> 0
        coord = getCoord rec1 33
        elev  = case (LC.readInt $ mySubStr rec1 67 5) of
            Just (x,_) -> x
            Nothing -> 0

readAirfield :: [LinkedRunway] -> [ArincLine] -> Maybe WP.Waypoint
readAirfield rwys (rec1:rec2:recs) =
    case arincRecordType rec1 of
        (ATAirfield,1) -> Just (WP.Waypoint c1 ident name (WP.Airfield rw fr))
        _ -> Nothing

    where
        c1 = getCoord rec1 33
        ident = (mkSString.unLString) $ LC.takeWhile(/=' ') $ mySubStr rec1 7 4
        name = (mkSString.unLString) $ LC.takeWhile(/=' ') $ mySubStr rec1 94 25
        rw = map snd (filter (\(x,_) -> ident == (mkSString.unLString)x) rwys)
        fr = []


-------------------------------------------------------------------------------
--
-- deprecated from here onwards
--
-- (left for sentimental reasons, *sniff*)
--
-------------------------------------------------------------------------------
-- take a list of A424 records and return a set of (Ident,runway) tuples
-- where Ident is the identifier of the airfield

getRunways :: [[ArincLine]] -> [LinkedRunway]
getRunways lns = getAllRunways lns
    --ad <- fmap GZip.decompress (Lazy.readFile fn)
    --ad <- Lazy.readFile fn
    --return (getAllRunways $ groupRecordLines ad)

    where
        getAllRunways :: [[ArincLine]] -> [LinkedRunway]
        getAllRunways s =
            map (fromJust.readRunway) (filter (\x -> (arincRecordType.head) x == (ATRunway,1)) s)

-------------------------------------------------------------------------------
-- takes a list of (Ident,runway) tuples and A424 records and returns
-- a set of waypoints

getAirfields :: [LinkedRunway] -> [[ArincLine]] -> [WP.Waypoint]
getAirfields runways lns = getAllAirfields runways lns
    --ad <- fmap GZip.decompress (Lazy.readFile fn)
    --ad <- Lazy.readFile fn
    --return (getAllAirfields runways $ groupRecordLines ad)

    where
        getAllAirfields :: [LinkedRunway] -> [[ArincLine]] -> [WP.Waypoint]
        getAllAirfields runways s=
            map (fromJust.readAirfield runways) (filter (\y -> (arincRecordType.head) y == (ATAirfield,1)) s)

getNavAids :: [[ArincLine]] -> [WP.Waypoint]
getNavAids lns = map (fromJust.readVHFNavaid) (filter (\y -> (arincRecordType.head) y == (ATNavAidVHF,1)) lns)

-------------------------------------------------------------------------------
-- takes a list of A424 records and splits them into groups according
-- to the type and A424 continuation record indicators, such
-- that each element in the result is an A424 super-record

groupRecordLines :: [ArincLine] -> [[ArincLine]]
groupRecordLines lns = groupBy firstRecordFlags lns
    where
        firstRecordFlags :: ArincLine -> ArincLine -> Bool
        firstRecordFlags x y =
            (fst $ arincRecordType x) == (fst $ arincRecordType y) 
              && (snd $ arincRecordType y) /= 1

-- deprecated, along with getRunways, getNavAids and getAirfields
arincConvertWaypointsOld :: FilePath -> FilePath -> [String] -> IO ()
arincConvertWaypointsOld input output areas = do
    inputRecords <- Lazy.readFile input
    let glines = groupRecordLines $ filter (filterByArea areasPacked) (LC.lines inputRecords)
    let rwys = getRunways glines
    let waypoints = (getAirfields rwys glines) ++ (getNavAids glines)
    encodeFile output (WP.mkDatabase waypoints)
    return ()
    where areasPacked = map LC.pack areas


