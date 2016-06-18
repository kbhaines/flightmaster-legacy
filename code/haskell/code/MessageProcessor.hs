-- |A MessageProcessor is an abstraction mechanism that sits in front of
-- an MVar, and allows an interface function to accept messages that act
-- on and modify the contents of the MVar in an atomic manner. The interface
-- function is supplied by the user


module MessageProcessor (

    -- * Functions
    newMessageProcessor

    -- * Types
  , MessageProcessor
  , MessageHandler

) where

import Control.Concurrent.MVar
import Control.Monad.State

-----
--Experiments
--

data MyType = GetInt | PutInt Int deriving (Show, Read)
data MyResp = MyInt Int deriving (Show, Read)

userFunc :: RunMessage Int MyType MyResp -> State Int MyResp
userFunc send = do
    res <- send GetInt
    case res of
        (MyInt 1) -> send (PutInt 0)
        (MyInt 0) -> send (PutInt 1)
        _ -> send (PutInt 10)

    return res

--sysFunc :: a -> (State a resp) -> resp
--sysFunc val sm = runState val sm

applyUserFunc :: State s a -> s -> a
applyUserFunc uf val = do
    evalState uf val

--------------------------------------------------------------------------------
-- |A function that accepts a message and updates the underlying MVar. This is
-- the abstraction that the client/user code deals with; simply send a message
-- to the MessageProcessor e.g. 
--
-- @
-- -- create a MessageProcessor for an integer, initialised to 1
-- mp <- newMessageProcessor (1::Int) (\val msg -> (val+1, True ::Bool))
-- -- send a '2' to the processor, get the result
-- result <- mp 2
--
-- result <- mp (userFunc firstMessage)
-- where
--  userFunc :: msg -> State Int Result
--  userFunc msg1 = do
--      
-- @

type MessageProcessor msg res = (msg -> IO res)

-- |A user-supplied function that takes a value of the underlying type and a
-- message, and returns the updated value and a result code, based on the
-- message.
type MessageHandler a msg res = ( a -> msg -> (a,res))

-- |Create a new 'MessageProcessor', returning a handle to it that can be used
-- by the client. 
newMessageProcessor :: a -> MessageHandler a msg res -> IO (MessageProcessor msg res)
newMessageProcessor initVal handlerFunc = do
    mv <- newMVar initVal
    return $ messageHandler mv handlerFunc

-- |This function handles the atomic updating of the attached MVar
messageHandler :: (MVar a) -> MessageHandler a m r -> m -> IO r
messageHandler mv handlerFunc msg = do
    chk <- isEmptyMVar mv
    if (chk) then putStrLn "Blocked" else return ()

    -- once we've acquired the MVar, there are no other updates that can occur.
    -- So we're safe to apply our updates and put the value back afterwards
    val <- takeMVar mv
    let (newVal, result) = handlerFunc val msg
    putMVar mv newVal
    return $ result


--------------------------------------------------------------------------------
-- | Type of function that runs a message inside a state monad

type RunMessage a msg res = (msg -> State a res)

-- |Start some comments:
type MessageProcessorState a msg res = (UserFuncState a msg res -> IO res)

type UserFuncState a msg res = (RunMessage a msg res -> State a res)

--------------------------------------------------------------------------------
-- |Update the state in accordance with the supplied handler and message

sendMsg :: MessageHandler s msg resp -> msg -> State s resp
sendMsg handler msg = do
    v <- get
    let (new, resp) = handler v msg
    put new
    return resp

newMessageProcessorState :: a -> MessageHandler a msg res -> IO (MessageProcessorState a msg res)
newMessageProcessorState myval handler = do
    mv <- newMVar myval
    return $ messageHandlerState mv (sendMsg handler) 

messageHandlerState :: MVar a -> RunMessage a msg resp -> UserFuncState a msg resp -> IO resp
messageHandlerState mv runMessage userFunc = do
    val <- takeMVar mv
    let (result, newVal) = runState (userFunc runMessage) val
    putMVar mv newVal
    return $ result


singleMessage :: msg -> UserFuncState a msg res
singleMessage msg = (
    \send -> do 
        res <- send msg
        return res
        )

