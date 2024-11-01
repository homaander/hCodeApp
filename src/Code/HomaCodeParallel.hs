module Code.HomaCodeParallel (
   getTapeIdParallel
 , getOffsetParallel
 , getStarter

 , preset250000
) where

import qualified Code.HomaCode as HC
import Code.HomaCodeData

import Control.Parallel.Strategies
-- import Control.DeepSeq


import Data.List (elemIndex)
import Data.Maybe (isJust)

-- Strats

preset250000 :: [[HNumsL]]
preset250000 = map HC.getArr ["_DOD_5","DNQNI4","OQMVSH","DNVRU4","_ISU3H","54H4HG"]

getStarter :: [HNumsL] -> [[HNumsL]]
getStarter a = [a, a1, a2, a3, a4, a5]
  where
    a1 = HC.codePreset preset250000 a
    a2 = HC.codePreset preset250000 a1
    a3 = HC.codePreset preset250000 a2
    a4 = HC.codePreset preset250000 a3
    a5 = HC.codePreset preset250000 a4


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


getOffsetParallel :: [HNumsL] -> [HNumsL] -> [Int] -> [(Int, Maybe Int)]
getOffsetParallel a b c = filter (\(_,g) -> isJust g) res
  where
    res = map (getOffsetChank a b) c `using` parList rdeepseq

getOffsetChank :: HC.Code a => a -> a -> Int -> (Int, Maybe Int)
getOffsetChank a b n =
  (,) (10000 * n) $ elemIndex b $
  take 10000 $
  drop (10000 * n) $
  iterate HC.code (HC.code a)


