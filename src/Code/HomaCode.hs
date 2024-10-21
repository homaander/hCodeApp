{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}

module Code.HomaCode (
    Math(..)
  , Code(..)
  , CodeRecurse(..)
  , Tape(..)
  , HData(..)
  , HDataInfo(..)
  , Arr(..)

  -- , HNums16(..)
  -- , HNumsL(..)
  -- , HTape (..)
) where

import Code.HomaCodeData
import Code.HomaCodeClasses
import Data.List ( elemIndex )
import Data.Maybe ( fromMaybe )


import Data.Text (Text)
import qualified Data.Text as T

-- HC.toTape (HC.getArr "HELLO" :: [HNumsL])
-- (44.57 secs, 25,886,063,400 bytes)

-- HC.getTapeId (HC.getArr "HELLO" :: [HNumsL])
-- (21.00 secs, 10,539,351,968 bytes)

class (Show a, Code [a]) => Arr a where
  showArr :: [a] -> Text
  showArr [] = ""
  showArr arr = foldl (\r a -> r <> T.pack (show a)) "" arr

  fakeRead :: Char -> a

  -- default
  getArr :: Text -> [a]
  getArr arr = map fakeRead $ T.unpack arr

  showDecN :: Int -> [a] -> Text
  showDecN  n = showArr . decodeN n

  showCodeN :: Int -> [a] -> Text
  showCodeN  n = showArr . codeN n

  dataText :: Int -> [a] -> (Text, Text)
  dataText n t = (showDecN n t, showCodeN n t)

  tapeText :: HTape [a] -> HTape Text
  tapeText t = HTape (showArr $ tapeId t) (tapeOffset t) (tapeAntiOffset t) (tapeLength t)


alfabet :: [Char]
alfabet  = [ '0', '1', '2', '3', '4', '5'
           , '6', '7', '8', '9', 'A', 'B'
           , 'C', 'D', 'E', 'F', 'G', 'H'
           , 'I', 'J', 'K', 'L', 'M', 'N'
           , 'O', 'P', 'Q', 'R', 'S', 'T'
           , 'U', 'V', 'W', 'X', 'Y', 'Z'
           , '_'
           ]

instance Arr Int where
  fakeRead a = fromMaybe 0 $ elemIndex a $ take (getMod @Int) alfabet

instance Arr HNums16 where
  fakeRead a = toEnum $ fromMaybe 0 $ elemIndex a $ take (getMod @HNums16) alfabet


instance Arr HNumsL where
  fakeRead a = toEnum $ fromMaybe 0 $ elemIndex a $ take (getMod @HNumsL) alfabet


-- >>>  Code.HomaCodeClasses.toHDataN 6 1234 :: [Int]
-- [0,0,1,2,3,4]

-- >>> [H12,H02,H13,H04,H06]
-- >>> Code.HomaCodeClasses.code [H12,H02,H13,H04,H06]
-- [C,2,D,4,6]
-- [2,7,B,6,C]

-- >>> [H12,H02,H13,H03,H14,H04]
-- >>> Code.HomaCodeClasses.codeN 66 [H12,H02,H13,H03,H14,H04]
-- [C,2,D,3,E,4]
-- [5,8,4,C,3,2]

-- >>> Code.HomaCodeClasses.decode ([1,2,5] :: [Int])
-- [5,7,8]

-- >>> Code.HomaCodeClasses.decodeN 2 ([1,2,3] :: [Int])
-- [6,1,4]

-- >>> Code.HomaCodeClasses.toTape ([0,1,2,3,4] :: [Int])
-- HTape {tapeId = [0,0,0,0,1], tapeOffset = 17811, tapeAntiOffset = 6400, tapeLength = 24211}
