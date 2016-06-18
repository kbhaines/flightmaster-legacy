module Main where

import Monad

type Log = [String]

newtype MyMonad a = MyMonad { runMyMonad :: (a, Log,Log) } deriving (Show, Eq, Ord)

instance Monad MyMonad where
    return a =  MyMonad (a, [],[])
    m >>= f = let (s,log1,log2) = runMyMonad m
                  n = f s
                  (s',log1',log2') = runMyMonad n
               in
                  MyMonad (s', log1 ++ log1', log2 ++ log2')

record :: (String,String) -> MyMonad Int
record (s1,s2) = MyMonad (0,[s1],[s2])

act1 :: Int -> MyMonad Int
act1 x = record ("act1",show x) >> return x

act2 :: Int -> MyMonad Int
act2 x = record ("act2", show x) >> return x

act3 :: Int -> Int -> MyMonad Int
act3 x y = record ("act3",show x) >> return (x+y)

main=do
    --let x@(MyMonad x1)= addX
    putStrLn $ show 1
