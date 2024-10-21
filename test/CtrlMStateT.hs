{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module CtrlMStateT where

import Control.Monad.State

-- data ShowBox = forall s. Show s => SB s

myVal :: StateT Int Maybe Int
myVal  = state (1,)

-- State s a
-- runState :: s -> (s, a)

myMain :: StateT Int Maybe Int
myMain  = do
    x <- get
    let res = x + 1
    put 4
    modify (^ (3 :: Int))
    return $ 10000 + res

main :: IO ()
main  = do
    let
        allV = runStateT  myMain 5
        valV = evalStateT myMain 5
        sttV = execStateT myMain 5
    putStrLn "Test State:"
    putStrLn $ "->  All " ++ show allV
    putStrLn $ "->  Val " ++ show valV
    putStrLn $ "->  St  " ++ show sttV
