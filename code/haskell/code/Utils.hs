{-# LANGUAGE TypeSynonymInstances #-}
-- extensions necessary to disable warnings about non-standard pattern guards
-- in readM
{-# OPTIONS -fglasgow-exts #-}

-- Enables flexible instances --

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

module Utils where

import Data.Maybe
import GHC.Int
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as S

import System.IO

import Char

-------------------------------------------------------------------------------
-- exported functions
--

metresToFeet n = n*3.2808399 
feetToMetres n = n/3.2808399

nmToKm n = n * 1.85200
kmToNm n = n / 1.85200

class Digitiser a where
    digitise :: a -> Int

instance Digitiser Char where
    digitise x 
     | ordx >= 65 && ordx <= 90 = ((ordx - 65) `div` 3 + 1)
     | ordx >= 48 && ordx <= 58 = (ordx - 48)
     | otherwise  = 0
     where ordx = fromEnum x

instance Digitiser String where
    digitise x = foldl (\x y -> 10*x + y) 0 (map digitise x)

type SString = S.ByteString
type LString = LC.ByteString

mkSString :: String -> SString
mkSString = S.pack
slength = S.length
sindex = S.index
sreadInt = S.readInt
stail = S.tail

mkLString :: String -> LString
mkLString = LC.pack

unSString :: SString -> String
unSString = S.unpack

unLString :: LString -> String
unLString = LC.unpack

emptyLString = LC.empty
emptySString = S.empty

lzword2Double :: LString -> Int -> Double
lzword2Double s n = read (LC.unpack $ LC.words s !! n)

word2Double :: String -> Int -> Double
word2Double s n = read (words s !! n)

subStr :: String -> Int -> Int -> String
subStr s start len = take len $ drop (start-1) s

subSStr :: SString -> Int -> Int -> SString
subSStr s start len = S.take len $ S.drop (start-1) s

mySubStr :: LString -> Int -> Int -> LString
mySubStr s start len = LC.take len' $ LC.drop (start'-1) s
    where start' = fromIntegral start :: GHC.Int.Int64
          len' = fromIntegral len :: GHC.Int.Int64

deleteElement :: [a] -> Int -> [a]
deleteElement lst i | i > length lst = error "deleteElement, index too big"
deleteElement lst i = (take i lst ++ drop (i+1) lst)

char :: Int -> LString -> Char
char n s = s `LC.index` (fromIntegral (n-1)::GHC.Int.Int64)

getIntFromStr :: LString -> Int -> Int -> Int
getIntFromStr str from len = 
    case (LC.readInt $ mySubStr str from len) of
        Just (x,_) -> x
        Nothing -> error "Unable to get int from string"

-- TODO: Re-implement using Parsec (=low)
splitString :: Char -> String -> [String]
splitString c [] = []
splitString c str = (fstb:splitString c (drop 1 $ sndb))
    where (fstb,sndb) = break (==c) str

-- see top of file, regarding disabling warnings for this function
readM :: (Monad m, Read a) => String -> m a
readM s | [x] <- parse = return x
        | otherwise    = fail $ "Failed to parse \"" ++ s ++ "\" as a number."
  where
    parse = [x | (x,y) <- reads s, y == ""]
    --parse = [x | (x,_) <- reads s]

-- my modification to parse, above, enforces the *entire* string (s) to be
-- parsed in order to match. 
--
-- NB:
-- "reads" returns the remainder of the string i.e.  reads "123x55" =
-- (123.0,"x55)

-- special truncation handles cases <0, where we want to truncate *away* 
-- from zero
truncAwayFromZero d 
    | d < 0.0 = truncd - 1
    | otherwise = truncd
    where truncd = (fromIntegral $ truncate d)

debug :: String -> IO ()
debug s = putStrLn s >> hFlush stdout
--debug _ = do return ()

debug1 :: (Show a) => String -> a -> IO ()
--debug1 s v = (putStrLn $ s ++ show v) >> hFlush stdout
debug1 _ _ = do return ()

--------------------------------------------------------------------------------
-- |Parser function for reading input that conforms to haskell data structures,
-- use this in preference to 'read' commands when dealing with possible user
-- input

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
        [(x, str)] | all isSpace str -> Just x
        _                            -> Nothing

