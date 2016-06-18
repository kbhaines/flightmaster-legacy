module Test.Waypoint

-- TODO: Increase waypoint test coverage, for subtypes (airfield, navaid)
-- TODO: Add boundsOverlap testing

where

import Waypoint
import Coord
import Test.HUnit
import Data.Maybe
import Data.Binary

import Test.WaypointData
import Test.Utils
import Utils

egbp = Waypoint (mkc "N51400500W002032500") (mkSString "EGBP") (mkSString "KEMBLE") NoType

[dty,egbe,egbj,egll] = map mkWaypoint [ dtyUnpack, egbeUnpack, egbjUnpack, egllUnpack ]

-------------------------------------------------------------------------------

loaddb :: IO WaypointDB
loaddb = do
    decodeFile "r:/jep/eu-wps.dat" :: IO WaypointDB
    
unWaypointList :: String -> [String] -> IO [String]
unWaypointList fname ids = do
    wpdb <- decodeFile fname :: IO WaypointDB
    return $ map (unWaypoint.waypointData.(fromJust.findByIdent wpdb)) ids

doubleInt :: Double -> Int
doubleInt d = round (d*1000000)

-------------------------------------------------------------------------------


testMkCoord = mapAssertEqual [

    ("Should create NE coordinate", (coordDeg 51.5 2), (mkc "N51300000E002000000"))
   ,("Should create SW coordinate", (coordDeg (-51.5) (-2)), (mkc "S51300000W002000000"))

   ]

--testMkCoord3 = TestCase $ assertFailure
    --"Should fail because of bad string length"
    --(mkc "S51300000W00200000")

testUnCoord = mapAssertEqual [
    ("Should unpack NE coordinate", ("N51305432E002123456"), (unCoord (mkc "N51305432E002123456")))
   ,("Should unpack SW coordinate", ("S51301234W002445432"), (unCoord (mkc "S51301234W002445432")))
   ]


distChk c1 c2 = (withinFoot (deg2rad 1) $ coordDistance (mkc c1) (mkc c2))

testDistance = mapAssertBool [
    ("Should be 1deg away (North/South)", (distChk "N51000000W000000000" "N52000000W000000000"))
   ,("Should be 1deg away (across equator)", (distChk "N00300000W000000000" "S00300000W000000000"))
   ,("Should be 1deg away (across prime meridian)", (distChk "N00000000W000300000" "N00000000E000300000"))
   ,("Should be 1deg away (across prime meridian at 60deg N)", (distChk "N60000000W001000000" "N60000000E001000000"))
    ]

testSaveLoad1 = TestCase $ assertEqual 
    "Should store and re-load the coordinate"  (mkc "N12345678W111223344")  (decode $ encode $ mkc "N12345678W111223344")

testSaveLoad2 = TestCase $ assertEqual 
   "Should store and re-load the coordinates" 
        [mkc "N12345678W111223344", mkc "N51400000W002000000", mkc "S51400000E002000000"]
        (decode $ encode [mkc "N12345678W111223344", mkc "N51400000W002000000", mkc "S51400000E002000000"])

coordTests = TestLabel "Coordinate tests" (TestList [

    testMkCoord, testUnCoord, testDistance, testSaveLoad1, testSaveLoad2

    ])

-------------------------------------------------------------------------------

testMkWaypoint1 = TestCase $ assertEqual
    "Should create Kemble waypoint"
    egbp 
    (mkWaypoint egbpUnpack)

testUnWaypoint1 = TestCase $ assertEqual
    "Should unpack Kemble waypoint"
    egbpUnpack
    (unWaypoint egbp)

testMkDatabase = TestCase $ assertEqual
    "Should create waypoint database"
    wplist
    (toList $ wpdatabase)

testFind1 = mapAssertEqual [

    ("Should find EGBP", egbp, (waypointData.fromJust $ findByIdent wpdatabase "EGBP"))
   --,("Should not find EGBX", Nothing, (findByIdent wpdatabase "EGBX"))

   ]

testFind2 = mapAssertEqual [

    ("Should find empty waypoint list", [], (map waypointData $ findAllByIdent wpdatabase "EGBX"))
   ,("Should find [EGLL]", [mkWaypoint egllUnpack], (map waypointData $ findAllByIdent wpdatabase "EGLL"))
   ,("Should find [BCN,BCN]", (map mkWaypoint (searchUnWaypoints "BCN")), (map waypointData $ findAllByIdent wpdatabase "BCN"))

   ]


testBounds = mapAssertEqual [
    ("Should find EGBP", [egbp], (map waypointData $ inCircle wpdatabase (coord egbp) (nm2rad 2.0)))
   ,("Should find EGBP/EGBJ", [egbp,egbj], (map waypointData $ inCircle wpdatabase (coord egbp) (nm2rad 15.0)))
   ,("Should find egbp/egbj", [egbp,egbj], (map waypointData $ inBounds wpdatabase (mkc "N52000000W002300000", mkc "N51300000W002000000")))
   ,("Should find egbp/egbj/bcn", [egbp,egbj,mkwn "BRECON"], (map waypointData $ inBounds wpdatabase (mkc "N52000000W003300000", mkc "N51300000W002000000")))
    ]

testIdentValid = mapAssertEqual [

    ("Should invalidate EGBX", False, (identIsValid wpdatabase "EGBX"))
  , ("Should validate EGBP", True, (identIsValid wpdatabase "EGBP"))
  , ("Should invalidate DTYX, i.e. prefix of DTY", False, (identIsValid wpdatabase "DTYX"))

  ]


testSaveLoadWaypoint1 = TestCase $ assertEqual
    "Should save and reload waypoint"
    (egbp)
    (decode $ encode egbp)

testSaveLoadWaypoint2 = TestLabel "Lo" $ TestCase $ assertEqual
    "Should save and reload waypoints"
    [egbp,dty,egll,egbj]
    (decode $ encode [egbp,dty,egll,egbj])

waypointTests = TestLabel "Waypoint tests" (TestList [
    testMkWaypoint1, testUnWaypoint1,
    testMkDatabase,
    testFind1, testFind2, 
    testBounds, testIdentValid,
    testSaveLoadWaypoint1, testSaveLoadWaypoint2
    
    ])

runAll = runTestTT $ TestLabel "Waypoint.hs" $ TestList [coordTests, waypointTests]

