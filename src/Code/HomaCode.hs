{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Code.HomaCode (
    Math(..)
  , HData(..)
  , Code(..)
  , Tape(..)
  , TapeInfo(..)

  , tapeText
  , dataText
  , getHCodeText
  , showHCodeText

  -- , Arr(..)
) where

import Code.HomaCode.Data
import Code.HomaCode.Math
import Code.HomaCode.HData
import Code.HomaCode.Code
import Code.HomaCode.Tape
import Code.HomaCode.TapeInfo


import Data.Text (Text)
import qualified Data.Text as T


-- forText
tapeText :: HTape [HNum] -> HTape Text
tapeText t = HTape (showHCodeText $ tapeId t) (tapeOffset t) (tapeAntiOffset t) (tapeLength t)

dataText :: Int -> [HNum] -> (Text, Text)
dataText n t = ((showHCodeText . decodeN n) t, (showHCodeText . codeN n) t)

getHCodeText :: HBase -> Text -> [HNum]
getHCodeText base arr = map (fromLetter base) $ T.unpack arr

showHCodeText :: [HNum] -> Text
showHCodeText arr = T.pack $ showHCode arr
