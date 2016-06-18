module Main where

import Arinc424
import System

wrongArgs = do
    error "Arguments: <infile> <outfile-waypoints> <out-airspace> <area> [<area>..]"

main = do
    args <- getArgs
    case length args of
        0 -> do wrongArgs
        1 -> do wrongArgs
        2 -> do wrongArgs
        3 -> do wrongArgs
        _ -> return ()

    let infile=args !! 0
    let outfile=args !! 1
    let outfile2=args !! 2
    let areas=drop 2 args
    arincConvertWaypoints infile outfile areas
    arincConvertAirspace infile outfile2 areas
