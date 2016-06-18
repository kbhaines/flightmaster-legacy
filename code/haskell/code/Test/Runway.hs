module Test.Runway

where

import Test.HUnit
import Test.Utils
import Data.Binary

import Coord
import Runway
import Utils

crd = Coord 0 0
rwy16=Runway (mkSString "16") crd 1000 20 0
rwy34=Runway (mkSString "34") crd 1000 20 0

rwy24L=Runway (mkSString "24L") crd 2000 30 0
rwy06R=Runway (mkSString "06R") crd 2000 30 0

rwy24R=Runway (mkSString "24R") crd 3000 30 0
rwy06L=Runway (mkSString "06L") crd 3000 30 0

allrwys = [rwy16,rwy34,rwy24L,rwy06R,rwy24R,rwy06L]

testLandable = mapAssertEqual [

    ("All runways", allrwys, runwayFilter 900 allrwys)
  , ("4 runways",[rwy24L,rwy06R,rwy24R,rwy06L], runwayFilter 1500 allrwys)
  , ("2 runways",[rwy24R,rwy06L], runwayFilter 2500 allrwys)

  ]


flipTest r1 r2 = (rwIdent r1 == runwayFlipIdent (rwIdent r2))
ftt s1 s2 = (mkSString s1) == (runwayFlipIdent $ mkSString s2)

testFlips = mapAssertBool [
    
    ("Flip 16", flipTest rwy34 rwy16)
  , ("Flip 34", flipTest rwy16 rwy34)
  , ("Flip 24L", flipTest rwy06R rwy24L)
  , ("Flip 06R", flipTest rwy24L rwy06R)

  , ("Flip 01R", ftt "01R" "19L" )
  , ("Flip 36", ftt "36" "18" )

    ]

mks = mkSString

testOtherEnds = mapAssertEqual [

    ("Other end of 16", rwy34, runwayOtherEnd allrwys $ mks "16")
  , ("Other end of 34", rwy16, runwayOtherEnd allrwys $ mks "34")
  , ("Other end of 24L", rwy06R, runwayOtherEnd allrwys $ mks "24L")
  , ("Other end of 06L", rwy24R, runwayOtherEnd allrwys $ mks "06L")

    ]

testSaveLoad = TestCase $ assertEqual
    "Should save and load runways"
    allrwys
    (decode $ encode allrwys)

runAll = runTestTT $ TestLabel "Runway.hs" $ TestList [ testLandable, testFlips, testOtherEnds, testSaveLoad ]
    
