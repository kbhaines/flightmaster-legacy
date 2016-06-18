{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SerialController where

import IO
#ifdef mingw32_HOST_OS
import System.Win32
#endif
import Foreign
import Data.Word
import Control.Concurrent
import Control.Concurrent.Chan

import System.IO
import System.Process

--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Char8 as BSC
--import qualified Data.ByteString.Lazy.Char8 as BSL
--import qualified Data.ByteString.Internal as BSI

import Maybe

-------------------------------------------------------------------------------
-- exported functions
--

type CloseComms = (IO ())

openPort :: String -> IO (Handle,CloseComms)
openPort portId = do
#ifdef mingw32_HOST_OS
    openWinPort "wincomm" portId
#else
    openWinPort "/bin/cat" portId

    -- TODO: openFile for serial comms broken
    -- This is strange, sometimes the serial port opens
    -- OK, sometimes it doesn't. Even when it does work,
    -- the handle doesn't receive any data. Works in
    -- GHCI though?

    --handle <- openFile portId ReadMode
    --hSetBuffering handle LineBuffering
    --return (handle,hClose handle)
#endif

-------------------------------------------------------------------------------
-- private functions & data
--

openWinPort :: String -> String -> IO (Handle,CloseComms)
openWinPort command portId = do
    (extStdIn,inchan,_,ph) <- createProcess (proc command [portId]) { std_out = CreatePipe, std_in = CreatePipe }
    return (fromJust inchan,killExtComms ph)

killExtComms :: System.Process.ProcessHandle -> IO ()
killExtComms proch = 
    terminateProcess proch

-------------------------------------------------------------------------------
-- Windows Serial management code
--
-- TODO: Remove need for WinComm.exe
-- This code causes the receiving end of the data to run wild and allocate multi-
-- megabytes of memory as data is consumed. I have no idea why!!
--
-- Left here until I can figure out why?

#ifdef WINDOWS_NATIVE_COM
bufferSize = 16 :: Int
bufferSizeWin = 8 :: DWORD

openWindowsPortNative :: String -> Handle -> IO ()
openWindowsPortNative portId chan = do
    winh <- createFile portId gENERIC_READ 0 Nothing oPEN_EXISTING 0 Nothing
    forkOS (readWindowsPort winh chan)
    return ()

readWindowsPort :: HANDLE -> Handle -> IO ()
readWindowsPort winh chan =
    mallocBytes bufferSize >>= \buf ->
    (portToByteString winh buf) >>= \numRead ->
    hPutBuf chan buf numRead >>
    hFlush chan>>
    readWindowsPort winh chan

-- specifically, it seems to be this part of the code that causes the problems...

readWindowsPort2 :: BSC.ByteString -> HANDLE -> Handle -> IO ()
readWindowsPort2 acc winh chan = 
    (BSC.split '\n' `fmap` BSI.createAndTrim bufferSize (portToByteString winh)) >>= \strs ->
    if length strs > 1 then 
        hPutStrLn chan (show (BSC.append acc (head strs))) >>
        hFlush chan >>
        readWindowsPort2 (BSC.concat $ tail strs) winh chan
      else
        readWindowsPort2 (BSC.append acc (strs !! 0)) winh chan

portToByteString :: HANDLE -> Ptr Word8 -> IO Int
portToByteString winh ptr = do
    bytesRead <- win32_ReadFile winh ptr bufferSizeWin Nothing
    return (fromIntegral bytesRead)

#endif
