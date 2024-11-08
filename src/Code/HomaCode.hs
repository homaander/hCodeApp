{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Code.HomaCode (
    Math(..)
  , HData(..)
  , Code(..)
  , Tape(..)
  , TapeInfo(..)

  , Arr(..)
) where

import Code.HomaCode.Data
import Code.HomaCode.Math
import Code.HomaCode.HData
import Code.HomaCode.Code
import Code.HomaCode.Tape
import Code.HomaCode.TapeInfo


import Data.List ( elemIndex )
import Data.Maybe ( fromMaybe )
import Data.Text (Text)
import qualified Data.Text as T



class (Show a, Enum a, Code [a]) => Arr a where
  showHCodeT :: [a] -> Text
  getHCT  :: Text -> [a]

  fakeRead :: Char -> a

  dataText :: Int -> [a] -> (Text, Text)

  tapeText t = HTape (showHCodeT $ tapeId t) (tapeOffset t) (tapeAntiOffset t) (tapeLength t)

  -- default
  showHCodeT arr = T.pack $ showHCode arr
  getHCT  arr = map fakeRead $ T.unpack arr

  dataText n t = ((showHCodeT . decodeN n) t, (showHCodeT . codeN n) t)

  tapeText :: HTape [a] -> HTape Text


instance Arr Int where
  fakeRead a = fromMaybe 0 $
               elemIndex a $
               take (notation @Int) hcodeAlfebet

instance Arr HNums16 where
  fakeRead a = toEnum $
               fromMaybe 0 $
               elemIndex a $
               take (notation @HNums16) hcodeAlfebet

instance Arr HNumsL where
  fakeRead a = toEnum $
               fromMaybe 0 $
               elemIndex a $
               take (notation @HNumsL) hcodeAlfebet
