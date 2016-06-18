-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------

module Main where

import qualified Test.Waypoint
import qualified Test.FlightPlan
import qualified Test.Runway
import qualified Test.GPS

main = do
    Test.Waypoint.runAll
    Test.FlightPlan.runAll
    Test.Runway.runAll
    Test.GPS.runAll
