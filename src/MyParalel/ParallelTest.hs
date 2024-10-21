module MyParalel.ParallelTest (
  evalTest, evalTest2, defaultTest
) where

import qualified Code.HomaCode as HC
import Code.HomaCodeData

import Control.Parallel.Strategies

import Data.Text


defaultTest :: Text -> ([HNumsL], Int)
defaultTest x = (HC.getTapeId y, HC.getTapeLength y)
  where y = HC.getArr x :: [HNumsL]

-- stack repl --ghci-options='+RTS -N -RTS'
-- defaultTest "HELLO"
-- evalTest    "HELLO"
-- evalTest2   "HELLO"

-- Eval
evalTest :: Text -> ([HNumsL], Int)
evalTest t = runEval $ do
  a <- rpar (f1 x)
  b <- rpar (f2 x)
  return (a, b)
  where
    x = HC.getArr t :: [HNumsL]
    f1 = HC.getTapeId
    f2 = HC.getTapeLength

evalTest2 :: Text -> ([HNumsL], Int)
evalTest2 t = runEval $ do
  a <- rpar (f1 x)
  b <- rpar (f2 x)
  rseq a
  rseq b
  return (a, b)
  where
    x = HC.getArr t :: [HNumsL]
    f1 = HC.getTapeId
    f2 = HC.getTapeLength



