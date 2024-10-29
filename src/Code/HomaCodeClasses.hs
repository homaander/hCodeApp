{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Code.HomaCodeClasses (
    Math(..)
  , Code(..)
  , CodeRecurse(..)
  , Tape(..)
  , HData(..)
  , HDataInfo(..)
) where

import Code.HomaCodeData
import Data.List  ( nub )
import Data.Maybe ( mapMaybe )


class (Eq a, Show a) => Math a where
  (^+) :: a -> a -> a
  neg  :: a -> a
  zero :: a

  (^*) :: a -> a -> a

  getMod :: Int

  -- default
  (^-) :: a -> a -> a
  (^-) a b = a ^+ neg b


class Math a => HData a where
  fromHData   ::  a  -> Int
  toHData     :: Int ->  a
  toHDataN    :: Int -> Int -> a


class Math a => Code a where
  code   :: a -> a
  decode :: a -> a

  findOffsetMaybe :: a -> a -> Maybe Int
  findListMaybe   :: a -> a -> Maybe [a]

  -- default
  codeN :: Int -> a -> a
  codeN n hdata = iterate code hdata !! n

  nextNCode :: Int -> a -> [a]
  nextNCode n ihd = take n $ iterate code (code ihd)

  decodeN :: Int -> a -> a
  decodeN n hdata = iterate decode hdata !! n

  (^->) :: a -> Int -> a
  (^->)  = flip codeN


class (Code a, HData a) => CodeRecurse a where
  -- default
  codeRecurse :: a -> a
  codeRecurse hdata = codeN (fromHData hdata) hdata


class (Ord a, Code a) => Tape a where
  toTape :: a -> HTape a

  getTapeOffset :: a -> a -> Int
  getTapeList   :: a -> a -> [a]

  -- default
  fromTape :: HTape a -> a
  fromTape (HTape h n _ _) = codeN n h

  getTapeId :: a -> a
  getTapeId  = minimum . getTapeAll

  getTapeLength :: a -> Int
  getTapeLength hdata = getTapeOffset hdata hdata

  getTapeAll :: a -> [a]
  getTapeAll hdata = getTapeList hdata hdata


class (CodeRecurse a, Tape a) => HDataInfo a where
  -- Get tape_id list from sums with offset second 0-500
  showDisperseList :: a -> a -> [a]
  -- Get (offset b, anti-offset c) -> (a_0 + b_offset)_anti-offset = c_0
  findDisperseData :: a -> a -> a ->  [(Int,Int)]



-- Math
instance Math Int where
  (^+) a b = (a + b) `mod` getMod @Int
  neg n   = (getMod @Int - n) `mod` getMod @Int
  (^*) a b = (a * b) `mod` getMod @Int
  zero = toEnum 0
  getMod = 10

instance Math HNums16 where
  (^+) a b = toEnum $ (fromEnum a + fromEnum b) `mod` getMod @HNums16
  neg n   = toEnum $ (getMod @HNums16 - fromEnum n) `mod` getMod @HNums16
  (^*) a b = toEnum $ (fromEnum a * fromEnum b) `mod` getMod @HNums16
  zero = toEnum 0
  getMod = 16

instance Math HNumsL where
  (^+) a b = toEnum $ (fromEnum a + fromEnum b) `mod` getMod @HNumsL
  neg n   = toEnum $ (getMod @HNumsL - fromEnum n) `mod` getMod @HNumsL
  (^*) a b = toEnum $ (fromEnum a * fromEnum b) `mod` getMod @HNumsL
  zero = toEnum 0
  getMod = 37


instance Math a => Math [a] where
  (^+) a b = zipWith (^+) (zerosA <> a) (zerosB <> b)
    where
      lRes = max lA lB
      lA = length a
      lB = length b
      zerosA = replicate (lRes - lA) zero
      zerosB = replicate (lRes - lB) zero
  neg = map neg
  (^*) = zipWith (^*)
  zero = [zero]
  getMod = 1


-- HData
instance (Math a, Enum a) => HData [a] where
  fromHData [] = 0
  fromHData hdata = sum $ zipWith (*) (map fromEnum hdata) powArr
    where
      powArr = map (getMod @a ^) powLen
      powLen = reverse [0 .. length hdata - 1]
  toHData num = map (toEnum . (`mod` getMod @a) . div num) powArr
    where
      powArr = map (getMod @a ^) powLen
      powLen = reverse [0 .. len - 1]
      len    = length $ show num
  toHDataN count num = replicate (count - length dt) zero <> dt
    where
      dt = toHData num


-- Code 
instance Math a => Code [a] where
  code hdata = map (uncurry (^-)) pairs
    where
      pairs = reverse $ zip hdata (zero : hdata)

  decode hdata = fst $
    foldr (\e (r, a) -> (r <> [e ^+ a], e ^+ a)) ([], zero) hdata

  findOffsetMaybe ihd hdata = if res == maxlen then Nothing else Just res
    where
      res = foldr
        (\he n -> if he == hdata then 1 else n + 1) 0
        (nextNCode maxlen ihd)
      maxlen = getMod @a ^ length ihd

  findListMaybe ihd hdata = if length res == maxlen then Nothing else Just res
    where
      res = ihd : foldr
        (\he n -> if he == hdata then [he] else [he] <> n) []
        (nextNCode maxlen ihd)
      maxlen = getMod @a ^ length ihd


-- RecurseCode
instance (Enum a, Math a) => CodeRecurse [a]


-- Tape
instance (Ord a, Enum a, Math a) => Tape [a] where
  toTape hdata = HTape hid offset (len - offset) len
    where
      offset  = if   offset' == len
                then 0
                else offset'

      offset' = getTapeOffset hid hdata
      hid     = getTapeId     hdata
      len     = getTapeLength hdata

  getTapeOffset ihd hdata =  foldr
        (\he n -> if he == hdata then 1 else n + 1) 0 (iterate code (code ihd))

  getTapeList ihd hdata = ihd : foldr
        (\he n -> if he == hdata then [he] else [he] <> n) [] (iterate code (code ihd))


-- HDataInfo
instance (Eq a, Ord a, Enum a, Math a) => HDataInfo [a] where
  showDisperseList aTape bTape = nub $ map (\n -> getTapeId (aTape ^+ codeN n bTape)) [0 .. 500]

  findDisperseData aTape bTape resTape = mapMaybe check [0 .. 500]
    where
      lid = getTapeId resTape
      check n = if getTapeId nsum == lid then Just (n, tapeAntiOffset $ toTape nsum) else Nothing
        where
          nsum = aTape ^+ codeN n bTape

