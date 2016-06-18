module Test.$name

where

import Test.HUnit
import Test.Utils

runAll = runTestTT $ TestLabel "name.hs" $ TestList [ ]
    
