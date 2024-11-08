{-# LANGUAGE ScopedTypeVariables #-}

module Code.HomaCode.HData (HData(..)) where

import Code.HomaCode.Math

class Math a => HData a where
  (<^<) :: a -> Int -> a
  (>^>) :: a -> Int -> a 

  fromHData   ::  a  -> Int
  toHData     :: Int ->  a
  toHDataN    :: Int -> Int -> a

  setLength   :: Int ->  a  -> a


instance (Enum a, Math a) => HData [a] where
  fromHData hdata = sum $ zipWith (*) (map fromEnum hdata) powArr
    where
      powArr = map (notation @a ^) powLen
      powLen = reverse [0 .. length hdata - 1]
  toHData num = map (toEnum . (`mod` notation @a) . div num) powArr
    where
      powArr = map (notation @a ^) powLen
      powLen = reverse [0 .. len - 1]
      len    = ceiling @Double @Int $ logBase (fromIntegral $ notation @a) (fromIntegral num)
  toHDataN count num = setLength count $ toHData num

  setLength n dat = replicate pre zero <> dat
    where
      pre = n - length dat

  dat <^< n = drop n dat <> replicate n zero
  dat >^> n =  replicate n zero <> take (length dat - n) dat