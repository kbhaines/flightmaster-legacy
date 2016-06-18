module Test.FlightPlan

where

-- TODO: Add planLegs test coverage

import Test.HUnit
import Test.Utils

import FlightPlan
import Waypoint hiding (toList)
import Coord

import Test.WaypointData

waypoints@[egbp,dty,egbe,egbj,egll] = map mkWaypoint [ egbpUnpack, dtyUnpack, egbeUnpack, egbjUnpack, egllUnpack ]

plan1 = [egbp, dty,egbe]
plan2 = [egbp,egbj,dty,egbe]

appendPlan1 = foldl appendWaypoint blankPlan [LegWaypoint egbp 2000, LegWaypoint dty 2000, LegWaypoint egbe 2000]

legEGBE = LegWaypoint egbe 3000
legEGBP = LegWaypoint egbp 3000

testPlans = mapAssertEqual [

    -- Append
    ("Should append to blank plan", [egbp], (toList $ appendWaypoint blankPlan (LegWaypoint egbp 2000)))
  , ("Should create flight plan egbp-dty-egbe", plan1, (toList appendPlan1))

    -- Insert
  , ("Should attempt insert into blank plan", [egbp], (toList $ insertWaypoint blankPlan 10 (LegWaypoint egbp 2000)))
  , ("Should insert EGBJ in egbp-dty-egbe = egbp-EGBJ-dty-egbe", plan2, (toList $ insertWaypoint appendPlan1 1 (LegWaypoint egbj 3000)))
  , ("Should insert EGBJ in egbp-dty-egbe = EGBJ-egbp-dty-egbe", [egbj,egbp,dty,egbe], (toList $ insertWaypoint appendPlan1 0 (LegWaypoint egbj 3000)))

    -- Failed insert
  , ("Should not insert DTY again in egbp-dty-egbe", [egbp,dty,egbe], (toList $ insertWaypoint appendPlan1 1 (LegWaypoint dty 3000)))
  , ("Should not insert EGBP again in egbp-dty-egbe, index 0", [egbp,dty,egbe], (toList $ insertWaypoint appendPlan1 0 legEGBP))
  , ("Should not insert EGBP again in egbp-dty-egbe, index 1", [egbp,dty,egbe], (toList $ insertWaypoint appendPlan1 0 legEGBP))
  , ("Should not insert EGBE again in egbp-dty-egbe, index 2", [egbp,dty,egbe], (toList $ insertWaypoint appendPlan1 2 legEGBE))
  , ("Should not insert EGBE again in egbp-dty-egbe, index 3", [egbp,dty,egbe], (toList $ insertWaypoint appendPlan1 3 legEGBE))
  , ("Should not insert EGBE again in egbp-dty-egbe, index 5", [egbp,dty,egbe], (toList $ insertWaypoint appendPlan1 5 legEGBE))

    -- Delete
  , ("Should delete a waypoint from a blank plan", [], (toList $ deleteWaypoint blankPlan egbp))
  , ("Should delete first waypoint from plan", [dty,egbe], (toList $ deleteWaypoint appendPlan1 egbp))
  , ("Should delete last waypoint from plan", [egbp,dty], (toList $ deleteWaypoint appendPlan1 egbe))
  , ("Should delete second waypoint from plan", [egbp,egbe], (toList $ deleteWaypoint appendPlan1 dty))
  , ("Should not delete bad waypoint from plan", [egbp,dty,egbe], (toList $ deleteWaypoint appendPlan1 egll))
    
    -- Append/Delete
  , ("Append followed by delete should not alter plan", [egbp,dty,egbe], (toList $ deleteWaypoint (appendWaypoint appendPlan1 (LegWaypoint egll 2000)) egll))
  , ("Insert followed by delete should not alter plan", [egbp,dty,egbe], (toList $ deleteWaypoint (insertWaypoint appendPlan1 0 (LegWaypoint egll 2000)) egll))

    -- fromList
  , ("Should create egbp-dty-egbe", [egbp,dty,egbe], (toList $ identsToPlan wpdatabase ["EGBP","DTY","EGBE"]))
  , ("Should create egbp-egbe, given wrong ident in list", [egbp,egbe], (toList $ identsToPlan wpdatabase ["EGBP","DDTY","EGBE"]))

  ]

planTests = TestLabel "Waypoint tests" (TestList [ testPlans ])

runAll = runTestTT $ TestLabel "FlightPlan.hs" $ TestList [planTests]
    
