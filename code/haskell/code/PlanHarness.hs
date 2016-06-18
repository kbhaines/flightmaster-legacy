module Main where

import IO
import Navigation
import MessageProcessor

import Control.Concurrent

doLoop :: MessageProcessor Message Response -> IO ()
doLoop nav = do
    putStrLn "---------------"
    hFlush stdout
    cmd <- getLine 

    resp <- nav (read cmd :: Message)
    putStrLn $ show resp
    doLoop nav

doLoop2 :: MessageProcessor Message Response -> IO ()
doLoop2 nav = do
    resp <- nav ReportCheckpoints
    putStrLn $ show resp
    hFlush stdout
    threadDelay 1000000
    doLoop2 nav

main = do

    navProcessor <- newMessageProcessor (Navigation (Plan Nothing) Nothing) navInterface 
    forkIO $ doLoop2 navProcessor

    doLoop navProcessor
    main
