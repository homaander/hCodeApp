{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Code.HomaCode.HData (HData(..)) where

import Code.HomaCode.Data
import Code.HomaCode.Math

class Math a => HData a where
  (<^<) :: [a] -> Int -> [a]
  (>^>) :: [a] -> Int -> [a]

  fromHData   :: [a] -> Int
  toHData     :: HBase -> Int -> [a]
  toHDataN    :: Int -> HBase -> Int -> [a]

  setLength :: HBase -> Int -> [a] -> [a]

  setBase :: Int -> HBase -> [a] -> [a]

  -- default
  toHDataN l b dat = setLength b l $ toHData b dat

  setBase l b dat = toHDataN l b $ fromHData dat


instance HData HNum where
  dat <^< n = drop n dat <> replicate n (HN (hBase $ head dat) 0)

  dat >^> n =  replicate n (HN (hBase $ head dat) 0) <> take (length dat - n) dat

  fromHData [] = 0
  fromHData hdata@(hf:_) = sum $ zipWith (*) (map hVal hdata) powArr
    where
      powArr = map (hBase hf ^) $ reverse [0 .. length hdata - 1]

  toHData b num = map (HN b . (`mod` b) . div num) powArr
    where
      powArr = map (10 ^) $ reverse [0 .. len - 1]
      len    = ceiling @Double @Int $ logBase (fromIntegral b) (fromIntegral num)

  setLength b nN dat = replicate (nN - length dat) (HN b 0) <> dat

