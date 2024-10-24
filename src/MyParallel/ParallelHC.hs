{-# LANGUAGE TypeApplications #-}
module MyParallel.ParallelHC (
   getTapeIdParallel
 , getOffsetParallel
) where

import qualified Code.HomaCode as HC
import Code.HomaCodeData

import Control.Parallel.Strategies
import Control.DeepSeq
import Data.List (elemIndex)
import Data.Maybe (isJust)


-- stack run -- -O2 +RTS -N8 -A512M -s -l

-- Strats
myStataDeep :: NFData a => Strategy a
myStataDeep a = rseq $ force a

instance NFData HNumsL where rnf a = seq a ()


getTapeIdParallel :: [HNumsL] -> [Int] -> [HNumsL]
getTapeIdParallel dat c = minimum res
  where
    res = map (getTapeIdFromChank dat) c `using` parList rpar

getTapeIdFromChank :: [HNumsL] -> Int -> [HNumsL]
getTapeIdFromChank a n = 
  minimum $
  take 1000 $
  drop (1000 * n) $
  iterate HC.code (HC.code a)


getOffsetParallel :: [HNumsL] -> [HNumsL] -> [Int] -> [(Int, Maybe Int)]
getOffsetParallel a b c = filter (\(_,b) -> isJust b) res
  where
    res = map (getOffsetChank a b) c `using` parList myStataDeep

getOffsetChank :: (Eq a, HC.Code a) => a -> a -> Int -> (Int, Maybe Int)
getOffsetChank a b n = 
  (,) (10000 * n) $ elemIndex b $
  take 10000 $
  drop (10000 * n) $
  iterate HC.code (HC.code a)


--
-- parMap :: (a -> b) -> [a] -> [b]
-- parMap f xs = map f xs `using` evalList rseq

-- evalList :: Strategy a -> Strategy [a]
-- evalList f [] = return []
-- evalList f (x:xs) = do
--   x' <- f x
--   xs' <- evalListm f xs
--   return (x':xs')
