-------------------------------------------------------------------------------
-- FlightPlan
-------------------------------------------------------------------------------

module FlightPlan where

import Data.Maybe
import Data.List
import qualified Waypoint as WP
import Waypoint(Waypoint,WaypointDB)
import Coord
import Utils

import Data.Binary

data FlightPlan = FlightPlan {

    legWaypoints :: [LegWaypoint]

} deriving (Show,Read,Eq)

-- TODO: Decide the semantics of ordering FlightPlans
instance Ord FlightPlan where
    (<) f1 f2 = True

data LegWaypoint = LegWaypoint {

    legWaypoint :: Waypoint,
    legAltitude :: Double

} deriving (Show,Read)


instance Eq LegWaypoint where
    (==) l1 l2 = legWaypoint l1 == legWaypoint l2

data Leg = Leg {

    from :: Waypoint,
    to :: Waypoint,
    track :: Double,
    distance :: Double,
    altitude :: Double

} deriving (Show,Read)

-------------------------------------------------------------------------------
-- exported functions
--

blankPlan :: FlightPlan
blankPlan = FlightPlan []

-------------------------------------------------------------------------------
--
identsToPlan :: WaypointDB -> [String] -> FlightPlan
identsToPlan wpdb idents =
    FlightPlan $ mapMaybe (\i -> WP.findByIdent wpdb i >>= \w -> Just $ LegWaypoint (WP.waypointData w) (0::Double)) idents

-------------------------------------------------------------------------------
--
numLegs :: FlightPlan -> Int
numLegs (FlightPlan fp) = length fp - 1 

-------------------------------------------------------------------------------
--
toList :: FlightPlan -> [Waypoint]
toList fp = map legWaypoint (legWaypoints fp)

-------------------------------------------------------------------------------
--
planIdents :: FlightPlan -> [String]
planIdents fp = map (unSString.WP.identifier) (toList fp)

-------------------------------------------------------------------------------
--
waypointIsInPlan :: FlightPlan -> Waypoint -> Bool
waypointIsInPlan (FlightPlan wps) wp = wp `elem` (map legWaypoint wps)

-------------------------------------------------------------------------------
--
deleteWaypoint :: FlightPlan -> Waypoint -> FlightPlan
deleteWaypoint (FlightPlan []) _ = FlightPlan []
deleteWaypoint f@(FlightPlan fpwps) wp
 | waypointIsInPlan f wp = FlightPlan $ delete (LegWaypoint wp 0) fpwps
 | otherwise = f

-------------------------------------------------------------------------------
--
insertWaypoint :: FlightPlan -> Int -> LegWaypoint -> FlightPlan
insertWaypoint f@(FlightPlan wps) i lwp@(LegWaypoint wp alt)
  | dupeCheck = FlightPlan $ before ++ [lwp] ++ after
  | otherwise = f
  where
    before = take i wps
    after = drop i wps
    dupeCheck = (noDupe before (last before) && noDupe after (head after))
    noDupe lst lg = if length lst == 0 || legWaypoint lg /= wp then True else False

-------------------------------------------------------------------------------
--
appendWaypoint :: FlightPlan -> LegWaypoint -> FlightPlan
appendWaypoint (FlightPlan wps) lwp = FlightPlan $ wps ++ [lwp]

-------------------------------------------------------------------------------
--
planLegs :: FlightPlan -> [Leg]
planLegs (FlightPlan ws) = planLegs' ws
  where
    planLegs' [] = []
    planLegs' [x] = []
    -- TODO: Implement track/heading function (in Coord)
    planLegs' (lw1:lw2:lws) = Leg wp1 wp2 (0.0::Double) (coordDistance (WP.coord wp1) (WP.coord wp2)) (legAltitude lw2):planLegs' (lw2:lws)
      where
        wp1 = legWaypoint lw1
        wp2 = legWaypoint lw2

-------------------------------------------------------------------------------
--
planTotalDistance :: FlightPlan -> Double
planTotalDistance (FlightPlan []) = 0.0
planTotalDistance (FlightPlan [x]) = 0.0
planTotalDistance (FlightPlan (w1:w2:ws)) = 
 coordDistance (WP.coord w1') (WP.coord w2') + planTotalDistance (FlightPlan (w2:ws))
 where
    (LegWaypoint w1' _) = w1
    (LegWaypoint w2' _) = w2

-- return a list of possible new plans based on inserting or appending the
-- specified waypoint into the plan. This list is sorted by smallest extra
-- distance

-------------------------------------------------------------------------------
--
planPermutations :: FlightPlan -> Waypoint -> [FlightPlan]
planPermutations fp@(FlightPlan []) wp = [appendWaypoint fp (LegWaypoint wp 0.0)]
planPermutations fp@(FlightPlan [x]) wp = [appendWaypoint fp (LegWaypoint wp 0.0)]
--planPermutations (FlightPlan [x]) wp = planPermutations (FlightPlan []) wp
planPermutations fp wp = map snd (sort $ (filter (\(deltaDist, _) -> deltaDist /=0) permutations)) 
  where
    lwp = LegWaypoint wp 0.0
    permutations = zip (map (\p -> planTotalDistance p - planTotalDistance fp) permutations') permutations'
    permutations' = map (\i -> insertWaypoint fp i lwp) [1..numLegs fp + 1]

-------------------------------------------------------------------------------
--
deletePermutations :: FlightPlan -> Waypoint -> [FlightPlan]
deletePermutations (FlightPlan [LegWaypoint w _]) wp | w == wp = [FlightPlan []]
deletePermutations fp@(FlightPlan legs) wp
  | waypointIsInPlan fp wp =
    map (FlightPlan.(deleteElement legs)) (elemIndices wp (map legWaypoint legs))

deletePermutations fp _ = [fp]

--legDistances :: FlightPlan -> [Double]
--legDistances fp = map (\(_,_,_,d,_) -> d) (planLegs fp)

--cumulativeDistances :: FlightPlan -> [Double]
--cumulativeDistances fp = drop 1 $ reverse $ map sum (tails $ reverse $ legDistances fp)

--legAltitude :: PlanLeg -> Double
--legAltitude (_,w2) = altitude w2 

-------------------------------------------------------------------------------
--

legSegmentate :: Leg -> Double -> [Coord]
legSegmentate (Leg fromWp toWp _ dist _) limit = p1 : segments ++ [p2]
  where
    (p1,p2) = (WP.coord fromWp, WP.coord toWp)
    segments 
      | dist < limit = []
      | otherwise = take n $ map (intermediatePointsGC p1 p2 dist) [interval,(2*interval)..1.0]
    interval = 1 / (fromIntegral n + 1)
    n = truncate (dist/limit)

intermediatePointsGC :: Coord -> Coord -> Double -> Double -> Coord
intermediatePointsGC (Coord lat1 lon1) (Coord lat2 lon2) dist f = Coord lat lon
  where
    d = dist
    a = sin ((1-f)*d) / sin d
    b = sin (f*d) / sin d
    x = a*cos lat1*cos lon1 + b*cos lat2 * cos lon2
    y = a*cos lat1*sin lon1 + b*cos lat2 * sin lon2
    z = a*sin lat1 + b * sin lat2
    lat = atan2 z (sqrt (x^2+y^2))
    lon = atan2 y x

-------------------------------------------------------------------------------
-- private functions & data
--

harness = do
    --wpdb <- decodeFile "r:/jep/eu-wps.dat" :: IO WaypointDB
    wpdb <- decodeFile "/media/truecrypt1/fmdata/eu-wps.dat" :: IO WaypointDB
    return wpdb
