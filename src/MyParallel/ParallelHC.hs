module MyParallel.ParallelHC where

import qualified Code.HomaCode as HC
import Code.HomaCodeData

import Control.Parallel.Strategies
import Control.DeepSeq

-- firstStrat :: Strategy (a, b)
-- firstStrat (a, b) = do
--   a' <- rpar $ force a
--   b' <- rpar $ force b
--   return (a', b')

gogo = minimum $ runEval $ parMap' (ttt (HC.getArr "34209472" :: [Int])) [1 .. 100]

parMap' :: (NFData a, NFData b) => (a -> b) -> [a] -> Eval [b]
parMap' f [] = return []
parMap' f (a:as) = do
   b <- rpar $ force (f a)
   bs <- parMap' f as
   return (b:bs)

ttt a n = minimum $ take 1000 $ drop (1000 * n) $ iterate HC.code (HC.code a)

-- >>> (2 + 3, 4 + 5) `using` firstStrat
-- (5,9)
