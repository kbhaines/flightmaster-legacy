module Main where

import Monad

newtype MyMonad a = MyMonad { runMyMonad :: a } deriving (Show, Eq, Ord)

instance Monad MyMonad where
    return x =  MyMonad x
    m >>= f = let v = runMyMonad m
                  v' = f v
               in
                  MyMonad (runMyMonad v')

initVal :: Int -> MyMonad Int
initVal i = return i

addTo :: Int -> Int -> MyMonad Int
addTo x y = return (x+y)

addX :: MyMonad Int
addX = initVal 0 >>= addTo 2 >>= addTo 7


record :: String -> MyMonad String
record s = MyMonad s

act1 :: Int -> MyMonad Int
act1 x = record "act1" >> return x

main=do
    let x@(MyMonad x1)= addX
    putStrLn $ show x1
