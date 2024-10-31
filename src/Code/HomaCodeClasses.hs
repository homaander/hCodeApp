{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Code.HomaCodeClasses (
    Math(..)
  , Code(..)
  , Tape(..)
  , HData(..)
  , HDataInfo(..)
) where

import Code.HomaCodeData
import Data.List  ( nub )
import Data.Maybe ( mapMaybe, fromJust )


class (Ord a, Show a) => Math a where
  (^+) :: a -> a -> a
  (^-) :: a -> a -> a
  (^*) :: a -> a -> a

  neg  :: a -> a
  zero :: a

  notation :: Int

  -- default
  (^-) a b = a ^+ neg b


class Math a => HData a where
  (^<<) :: a -> Int -> a
  (^>>) :: a -> Int -> a

  fromHData   ::  a  -> Int
  toHData     :: Int ->  a
  toHDataN    :: Int -> Int -> a

  setLength   :: Int ->  a  -> a


class HData a => Code a where
  (^->) :: a -> Int -> a
  (^<-) :: a -> Int -> a

  code       ::  a  -> a
  codeN      :: Int -> a ->  a
  codeNList  :: Int -> a -> [a]
  codeR      ::  a  -> a
  codePreset :: [a] -> a ->  a

  decode  ::  a  -> a
  decodeN :: Int -> a -> a

  findOffsetMaybe :: a -> a -> Maybe Int
  findListMaybe   :: a -> a -> Maybe [a]

  getPreset :: Int -> Int -> [a]

  -- default
  (^->)  = flip codeN
  (^<-)  = flip decodeN

  codeN     n hdata = iterate code hdata !! n
  codeNList n ihd   = take n $ iterate code (code ihd)

  codeR hdata = codeN (fromHData hdata) hdata

  decodeN n hdata = iterate decode hdata !! n


class Code a => Tape a where
  toTape :: a -> HTape a
  fromTape :: HTape a -> a

  getTapeId     :: a -> a
  getTapeLength :: a -> Int
  getTapeList   :: a -> [a]

  -- default
  fromTape (HTape h n _ _) = codeN n h

  getTapeId  = minimum . getTapeList

  getTapeLength hdata = fromJust $ findOffsetMaybe hdata hdata

  getTapeList hdata = fromJust $ findListMaybe hdata hdata


class Tape a => HDataInfo a where
  showDisperseList :: a -> a -> [a]
  findDisperseData :: a -> a -> a ->  [(Int,Int)]



-- Math
instance Math Int where
  (^*) a b = (a * b) `mod` notation @Int
  (^+) a b = (a + b) `mod` notation @Int
  neg a    = toEnum $ (n - fromEnum a) `mod` n
    where n = notation @Int
  zero = toEnum 0
  notation = 10

instance Math HNums16 where
  (^*) a b = toEnum $ (fromEnum a * fromEnum b) `mod` notation @HNums16
  (^+) a b = toEnum $ (fromEnum a + fromEnum b) `mod` notation @HNums16
  neg a    = toEnum $ (n - fromEnum a) `mod` n
    where n = notation @HNums16
  zero = toEnum 0
  notation = 16

instance Math HNumsL where
  (^*) a b = toEnum $ (fromEnum a * fromEnum b) `mod` notation @HNumsL
  (^+) a b = toEnum $ (fromEnum a + fromEnum b) `mod` notation @HNumsL
  neg a    = toEnum $ (n - fromEnum a) `mod` n
    where n = notation @HNumsL
  zero = toEnum 0
  notation = 37


instance Math a => Math [a] where
  (^+)= zipWith (^+)
  neg = map neg
  (^*) = zipWith (^*)
  zero = [zero]
  notation = 1


-- HData
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

  dat ^<< n = drop n dat <> replicate n zero
  dat ^>> n =  replicate n zero <> take (length dat - n) dat



-- Code 
instance (Enum a, Math a) => Code [a] where
  code d = reverse $ zipWith (^-) d (d ^>> 1)

  decode d = reverse [foldl (^+) zero (d ^<< a) | a <- [0 .. length d - 1]]

  findOffsetMaybe ihd hdata = if res == maxlen then Nothing else Just res
    where
      res         = foldr finder 0 (codeNList maxlen ihd)
      finder he n = if he == hdata then 1 else n + 1
      maxlen      = notation @a ^ length ihd

  findListMaybe ihd hdata = do
    off <- findOffsetMaybe ihd hdata
    pure $ ihd : codeNList off ihd

  getPreset n a | a <= 0    = map (decodeN (1 - a)) preset
                | otherwise = map (codeN   (a - 1)) preset
    where
      st = setLength n [neg $ toEnum 1, toEnum 1]
      preset = map (st ^<<) [0 .. n - 1]

  codePreset preset dat = map (foldl (^+) zero . (^* dat)) preset


-- Tape
instance (Enum a, Math a) => Tape [a] where
  toTape hdata = HTape hid offset (len - offset) len
    where
      offset  = if offset' /= len then offset' else 0
      offset' = fromJust $ findOffsetMaybe hid hdata
      len     = getTapeLength hdata
      hid     = getTapeId     hdata

-- HDataInfo
instance (Enum a, Math a) => HDataInfo [a] where
  showDisperseList aTape bTape = nub $ map (\n -> getTapeId (aTape ^+ codeN n bTape)) [0 .. 500]

  findDisperseData aTape bTape resTape = mapMaybe check [0 .. 500]
    where
      lid = getTapeId resTape
      check n = if getTapeId nsum == lid then Just (n, tapeAntiOffset $ toTape nsum) else Nothing
        where
          nsum = aTape ^+ codeN n bTape

