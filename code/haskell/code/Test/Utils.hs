module Test.Utils where

import Test.HUnit
mapAssertBool :: [(String, Bool)] -> Test
mapAssertBool cases = TestList $ map (\(n,t) -> TestLabel n $ TestCase $ assertBool "" t) cases
 
mapAssertEqual :: (Eq a,Show a) => [(String, a,a)] -> Test
mapAssertEqual cases = TestList $ map (\(n,t1,t2) -> TestLabel n $ TestCase $ assertEqual "" t1 t2) cases
 
mapTest :: [(String,Assertion)] -> Test
mapTest cases = TestList $ map (\(n,a) -> TestLabel n $ TestCase $ a) cases

