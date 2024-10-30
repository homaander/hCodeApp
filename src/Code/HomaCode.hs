{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Code.HomaCode (
    Math(..)
  , Code(..)
  , CodeRecurse(..)
  , Tape(..)
  , HData(..)
  , HDataInfo(..)
  , Arr(..)

  , getPreset
  , codePreset
  , codeNPreset
  , codeNPresetInt

  , (^<<)
  , toLength

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



codeNPreset :: Int -> [HNumsL] -> [HNumsL]
codeNPreset = codePreset . getPreset 6

codeNPresetInt :: Int -> [Int] -> [Int]
codeNPresetInt n dat = map (foldl (^+) 0 . (^* dat)) preset
  where
    fl = [ [0,0,0,0,9,1]
         , [0,0,0,9,1,0]
         , [0,0,9,1,0,0]
         , [0,9,1,0,0,0]
         , [9,1,0,0,0,0]
         , [1,0,0,0,0,0]
         ] :: [[Int]]
    preset = map (codeN (n - 1)) fl




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

  dataText :: Int -> [a] -> (Text, Text)
  dataText n t = ((showArr . decodeN n) t, (showArr . codeN n) t)

  tapeText :: HTape [a] -> HTape Text
  tapeText t = HTape (showArr $ tapeId t) (tapeOffset t) (tapeAntiOffset t) (tapeLength t)


instance Arr Int where
  fakeRead a = fromMaybe 0 $
               elemIndex a $
               take (getMod @Int) hCAlfabet

instance Arr HNums16 where
  fakeRead a = toEnum $
               fromMaybe 0 $
               elemIndex a $
               take (getMod @HNums16) hCAlfabet

instance Arr HNumsL where
  fakeRead a = toEnum $
               fromMaybe 0 $
               elemIndex a $
               take (getMod @HNumsL) hCAlfabet


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
