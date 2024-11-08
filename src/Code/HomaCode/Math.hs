{-# LANGUAGE AllowAmbiguousTypes #-}

module Code.HomaCode.Math (Math(..)) where

import Code.HomaCode.Data

class (Ord a, Show a) => Math a where
  (^+) :: a -> a -> a
  (^-) :: a -> a -> a
  (^*) :: a -> a -> a

  neg  :: a -> a
  zero :: a

  notation :: Int

  -- default
  (^-) a b = a ^+ neg b


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