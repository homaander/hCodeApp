{-# LANGUAGE TypeApplications #-}
module ParallelTest (aaa)  where

import Code.HomaCode
import Code.HomaCodeData

import Control.Parallel


getTapeId' :: [HNumsL] -> [HNumsL]
getTapeId' = minimum . getTapeAll'

getTapeAll' :: [HNumsL] -> [[HNumsL]]
getTapeAll' hdata = getTapeList' hdata hdata

getTapeList' :: [HNumsL] -> [HNumsL] -> [[HNumsL]]
getTapeList' ihd hdata = ihd : foldr
        (\he n -> if he == hdata then [he] else [he] <> n) [] (iterate code (code ihd))

myValHNL :: [HNumsL]
myValHNL = [L00, L02, L23, L21, L04]
-- getTapeId -> (19.91 secs, 10,539,363,600 bytes)


myValI :: [Int]
myValI = [4, 5, 6, 7, 8, 3, 3, 5]
-- getTapeId -> (1.73 secs, 2,192,908,088 bytes)

j0 :: [Int]
j0 = [1,0]

aaa = [itr1, itr2]
  where
    -- g = getMod @HNumsL
    g = getMod @Int
    len  = (g ^ 2) `div` 2
    mlen = (g ^ 2) `mod` 2
    p1 = len + mlen
    p2 = len
    itr = iterate code (code j0)
    itr1 = take p1 itr
    itr2 = take p2 (drop p1 itr)

