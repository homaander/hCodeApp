{-# LANGUAGE TypeApplications #-}
module MyParallel.ParallelHC (
   getTapeIdParallel
 , getOffsetParallel
 , getStarter
) where

import qualified Code.HomaCode as HC
import Code.HomaCodeData

import Control.Parallel.Strategies
import Control.DeepSeq
import Data.List (elemIndex)
import Data.Maybe (isJust)
import Code.HomaCode (Code(codeN))


-- stack run -- -O2 +RTS -N8 -A1G -s
-- stack run -- -O2 +RTS -N8 -A1G -s -l

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
  take 10000 $
  drop (10000 * n) $
  iterate HC.code (HC.code a)


getStarter :: [HNumsL] -> [[HNumsL]]
getStarter a = [a, a1, a2, a3, a4, a5]
  where
    a1 = HC.codeN 2500000 a
    a2 = HC.codeN 2500000 a1
    a3 = HC.codeN 2500000 a2
    a4 = HC.codeN 2500000 a3
    a5 = HC.codeN 2500000 a4


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
