{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Code.HomaCode.HData (HData(..)) where

import Code.HomaCode.Data
import Code.HomaCode.Math

class Math a => HData a where
  (<^<) :: [a] -> Int -> [a]
  (>^>) :: [a] -> Int -> [a]

  fromHData   :: [a] -> Int
  toHData     :: Int -> [a]


instance HData HNum where
  dat <^< n = drop n dat <> replicate n (HN (hBase $ head dat) 0)

  dat >^> n =  replicate n (HN (hBase $ head dat) 0) <> take (length dat - n) dat

  fromHData [] = 0
  fromHData hdata@(hf:_) = sum $ zipWith (*) (map hVal hdata) powArr
    where
      powArr = map (hBase hf ^) $ reverse [0 .. length hdata - 1]

  toHData num = map (HN 10 . (`mod` 10) . div num) powArr
    where
      powArr = map (10 ^) $ reverse [0 .. len - 1]
      len    = ceiling @Double @Int $ logBase 10 (fromIntegral num)

