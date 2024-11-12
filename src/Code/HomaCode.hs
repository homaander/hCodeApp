{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Code.HomaCode (
    Math(..)
  , HData(..)
  , Code(..)
  , Tape(..)
  , TapeInfo(..)

  , tapeText
  , getHCT
  , dataText
  , showHCodeT
  , fakeRead
  , setBaseForce

  -- , Arr(..)
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

tapeText :: HTape [HNum] -> HTape Text
tapeText t = HTape (showHCodeT $ tapeId t) (tapeOffset t) (tapeAntiOffset t) (tapeLength t)

getHCT :: Int -> Text -> [HNum]
getHCT n arr = map (fakeRead n) $ T.unpack arr

dataText :: Int -> [HNum] -> (Text, Text)
dataText n t = ((showHCodeT . decodeN n) t, (showHCodeT . codeN n) t)

showHCodeT :: [HNum] -> Text
showHCodeT arr = T.pack $ showHCode arr

setBaseForce :: HBase -> [HNum] -> [HNum]
setBaseForce b = map (\(HN _ v) -> HN b v)

fakeRead :: Int -> Char -> HNum
fakeRead n a = HN n $ fromMaybe 0 $
               elemIndex a $
               take n hcodeAlfebet
