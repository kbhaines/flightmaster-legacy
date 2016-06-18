module Test.GPS

where

import Maybe
import Test.HUnit
import Test.Utils
import Test.QuickCheck
import GPS
import Text.Printf
import Coord
import qualified Data.ByteString.Lazy.Char8 as LZ

-------------------------------------------------------------------------------
--
-- QuickCheck tests
--
newtype ValidNMC = ValidNMC (String,Char)
    deriving (Eq,Show)

instance Arbitrary ValidNMC where
    arbitrary = genValidLatNMC
    coarbitrary = undefined

genValidLatNMC = genNMC 90 ['N','S']

genNMC :: Int -> [Char] -> Gen ValidNMC
genNMC maxd elems = do
    d <- choose (0,maxd) :: Gen Int
    nsew <- elements elems

    if d == maxd then
        return $ ValidNMC (show ( fromIntegral $ d*100 ),nsew)
      else do
        m <- choose (0,59) :: Gen Int
        dm <- choose (0,9999) :: Gen Int
        return $ ValidNMC (printf "%09.4f" ( (fromIntegral $ d*100+m) + (fromIntegral dm / 10000) :: Double), nsew)

newtype TestCoord = TestCoord (Int,Int,Int) deriving (Eq,Show)

instance Arbitrary TestCoord where
    arbitrary = do
        d <- choose (0,200)
        m <- choose (0,69)
        s <- choose (0,69)
        return $ TestCoord (d,m,s)

    coarbitrary = undefined
        
validLat :: TestCoord -> Bool
validLat (TestCoord (d,m,s)) = (d < 90 && m < 60 && s < 60)

validLon :: TestCoord -> Bool
validLon (TestCoord (d,m,s)) = (d < 180 && m < 60 && s < 60)

testCoord2NMEAString :: TestCoord -> String
testCoord2NMEAString (TestCoord (d,m,s)) = 
    printf "%09.4f" ( (fromIntegral $ d*100+m) + (fromIntegral s / 60) :: Double)

prop_validNM :: TestCoord -> TestCoord -> Property
prop_validNM lat@(TestCoord (dlat,mlat,slat)) lon@(TestCoord (dlon,mlon,slon))=
     (validLat lat && validLon lon) ==> nmeaCoord (testCoord2NMEAString lat, "N") (testCoord2NMEAString lon, "W")  /= Nothing

-------------------------------------------------------------------------------

testInvalidNMEACoord = mapAssertEqual [

    ("Should return nothing for invalid coordinate (blank strings)", Nothing, (nmeaCoord ("","") ("","")))

  , ("NW blank degrees", Nothing, (nmeaCoord ("","N") ("","W")))
  , ("WW", Nothing, (nmeaCoord ("5140.1234","W") ("5140.1234","W")))
  , ("NN", Nothing, (nmeaCoord ("5140.1234","N") ("5140.1234","N")))

  , ("Lat out of range", Nothing, (nmeaCoord ("9940.1234","N") ("15140.1234","W")))
  , ("Lon out of range", Nothing, (nmeaCoord ("5140.1234","N") ("19140.1234","W")))

  , ("Malformed latitude string", Nothing, (nmeaCoord ("120.34","N") ("00234.1234","W")))
  , ("Malformed longitude string", Nothing, (nmeaCoord ("5012.3445","N") ("034.1234","W")))

   ]

testValidNMEACoord = mapAssertBool [

    ("Should parse valid coordinates NE", (closeTo (coordDegMin (51,40.1234) (2,45.6789)) (fromJust $ nmeaCoord ("5140.1234","N") ("00245.6789","E"))))
  , ("Should parse valid coordinates SW", (closeTo (coordDegMin (-51,-40.1234) (-2,-45.6789)) (fromJust $ nmeaCoord ("5140.1234","S") ("00245.6789","W"))))

    ]

testNMEACoord = TestLabel "group tests" (TestList [ testInvalidNMEACoord, testValidNMEACoord ])

-------------------------------------------------------------------------------
-- GPRMC tests
-- 

rmcp1 = (GPS.parse).(LZ.pack)

testInvalidRMC = mapAssertEqual [

    ("Invalid fix", Nothing, rmcp1 "$GPRMC,080152.542,V,5135.7927,N,00147.5688,W,0.000000,,190609,,*0D"  )
  , ("Invalid N/S", Nothing, rmcp1 "$GPRMC,080152.542,A,5135.7927,,00147.5688,W,0.000000,,190609,,*0D"   )
  , ("Invalid W/E", Nothing, rmcp1 "$GPRMC,080152.542,A,5135.7927,N,00147.5688,,0.000000,,190609,,*0D"   )
  , ("Invalid Lat", Nothing, rmcp1 "$GPRMC,080152.542,A,9135.7927,N,00147.5688,W,0.000000,,190609,,*0D"  )
  , ("Invalid Lon", Nothing, rmcp1 "$GPRMC,080152.542,A,5135.7927,N,20147.5688,W,0.000000,,190609,,*0D"  )
  , ("Lat out of range", Nothing, rmcp1 "$GPRMC,080152.542,A,9000.0001,N,00147.5688,W,0.000000,,190609,,*0D"  )
  , ("Lat out of range", Nothing, rmcp1 "$GPRMC,080152.542,A,9000.0001,S,00147.5688,W,0.000000,,190609,,*0D"  )
  , ("Lon out of range", Nothing, rmcp1 "$GPRMC,080152.542,A,5123.1234,N,18000.0001,W,0.000000,,190609,,*0D"  )
  , ("Lon out of range", Nothing, rmcp1 "$GPRMC,080152.542,A,5123.1234,N,18000.0001,E,0.000000,,190609,,*0D"  )
  , ("truncated message", Nothing, rmcp1 "$GPRMC,080152.542,A,5123.1234,N,18000.0001,E"                        )

    ]

rmcp2 = (GPS.posn).fromJust.(GPS.parse).(LZ.pack)

testValidRMC = mapAssertEqual [

    ("Somewhere north west", coordDegMin (51,35.7927) (-1,-47.5688), rmcp2 "$GPRMC,080152.542,A,5135.7927,N,00147.5688,W,0.000000,,190609,,*0D" )
  , ("Somewhere south east", coordDegMin (-51,-35.7927) (1,47.5688), rmcp2 "$GPRMC,080152.542,A,5135.7927,S,00147.5688,E,0.000000,,190609,,*0D" )
  , ("North West edge", coordDegMin (90,0) (-180,0), rmcp2 "$GPRMC,080152.542,A,9000.0000,N,18000.0000,W,0.000000,,190609,,*0D" )
  , ("South east edge", coordDegMin (-90,0) (180,0), rmcp2 "$GPRMC,080152.542,A,9000.0000,S,18000.0000,E,0.000000,,190609,,*0D" )

    ]

testGPRMC = TestList [ testInvalidRMC, testValidRMC ]

runAll = runTestTT $ TestLabel "GPS.hs" $ TestList [ testNMEACoord, testGPRMC ]

