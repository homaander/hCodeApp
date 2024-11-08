{-# LANGUAGE ScopedTypeVariables #-}

module Code.HomaCode.Code (Code(..)) where

import Code.HomaCode.Math
import Code.HomaCode.HData

class HData a => Code a where
  (-^>) :: a -> Int -> a
  (<^-) :: a -> Int -> a

  getPreset  :: Int -> Int -> [a]
  runPreset :: [a] -> a ->  a

  code       ::  a  -> a
  codeN      :: Int -> a ->  a
  codeNList  :: Int -> a -> [a]
  codeR      ::  a  -> a

  decode  ::  a  -> a
  decodeN :: Int -> a -> a

  findOffset :: a -> a -> Maybe Int
  findList   :: a -> a -> Maybe [a]


  -- default
  (-^>)  = flip codeN
  (<^-)  = flip decodeN

  codeN     n hdata = iterate code hdata !! n
  codeNList n ihd   = take n $ iterate code (code ihd)

  codeR hdata = codeN (fromHData hdata) hdata

  decodeN n hdata = iterate decode hdata !! n



instance (Enum a, Math a) => Code [a] where
  code d = reverse $ zipWith (^-) d (d >^> 1)

  decode d = reverse [foldl (^+) zero (d <^< a) | a <- [0 .. length d - 1]]

  findOffset ihd hdata = if res == maxlen then Nothing else Just res
    where
      res         = foldr finder 0 (codeNList maxlen ihd)
      finder he n = if he == hdata then 1 else n + 1
      maxlen      = notation @a ^ length ihd

  findList ihd hdata = do
    off <- findOffset ihd hdata
    pure $ ihd : codeNList off ihd

  getPreset n a | a <= 0    = map (decodeN (1 - a)) preset
                | otherwise = map (codeN   (a - 1)) preset
    where
      st = setLength n [neg $ toEnum 1, toEnum 1]
      preset = map (st <^<) [0 .. n - 1]

  runPreset preset dat = map (foldl (^+) zero . (^* dat)) preset
