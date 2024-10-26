module MyParallel.ParallelTest (
    defaultTest
  , evalTest
  , evalTest2
  , evalTest3
) where

import qualified Code.HomaCode as HC
import Code.HomaCodeData

import Control.Parallel.Strategies
import Control.DeepSeq

import Data.Text

-- stack run -- -O2 +RTS -N8 -A1G
-- stack run -- -O2 +RTS -N8 -A1G -s
-- stack run -- -O2 +RTS -N8 -A1G -s -l

-- stack repl --ghci-options='+RTS -N2 -s -RTS'
-- defaultTest "HELLO"
-- evalTest    "HELLO"
-- evalTest2   "HELLO"


defaultTest :: Text -> ([HNumsL], Int)
defaultTest x = (HC.getTapeId y, HC.getTapeLength y)
  where y = HC.getArr x :: [HNumsL]


-- Strategy
firstStrat :: (NFData a, NFData b) => Strategy (a, b)
firstStrat (a, b) = do
  a' <- rpar $ force a
  b' <- rpar $ force b
  return (a', b')

-- >>> (2 + 33, 4 + 5) `using` firstStrat
-- (35,9)

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

evalTest3 :: Text -> ([HNumsL], Int)
evalTest3 t = (f1 x, f2 x) `using` firstStrat
  where
    x = HC.getArr t :: [HNumsL]
    f1 = HC.getTapeId
    f2 = HC.getTapeLength



