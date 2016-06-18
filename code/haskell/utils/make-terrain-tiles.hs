module Main where

import Terrain
import IO

import Data.List
import Data.Word
import Foreign

loop :: [Word8] -> Int -> Int -> IO [Word8]
loop acc _ 0 = return acc
loop acc lat lon = 
    analyseTile "r:/world-terrain" lat lon hspan >>= \r ->
    loop (acc++[r]) lat (lon-1)
    
main = do
    mapM_ (\(lat,lon) -> 
        readFromMasterTerrain "r:/world-terrain" lat lon >>= \tile ->
        writeCompressedTile tile "r:/wtdata/t" lat lon >>
        free tile
      ) [(lat,lon) | lat <- [-32..31], lon <- [-64..63]]
      --) [(lat,lon) | lat <- [-32..31], lon <- [-64..63]]


hspan lst = (last n - head n)
 where n = (sort.nub)lst
