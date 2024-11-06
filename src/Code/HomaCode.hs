{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Code.HomaCode (
    Math(..)
  , Code(..)
  , Tape(..)
  , HData(..)
  , TapeInfo(..)
  , Arr(..)
) where

import Code.HomaCodeData
import Code.HomaCodeClasses

import Data.List ( elemIndex )
import Data.Maybe ( fromMaybe )
import Data.Text (Text)
import qualified Data.Text as T



class (Show a, Enum a, Code [a]) => Arr a where
  showHCT :: [a] -> Text
  getHCT  :: Text -> [a]

  fakeRead :: Char -> a

  dataText :: Int -> [a] -> (Text, Text)

  tapeText t = HTape (showHCT $ tapeId t) (tapeOffset t) (tapeAntiOffset t) (tapeLength t)

  -- default
  showHCT arr = T.pack $ showH arr
  getHCT  arr = map fakeRead $ T.unpack arr

  dataText n t = ((showHCT . decodeN n) t, (showHCT . codeN n) t)

  tapeText :: HTape [a] -> HTape Text


instance Arr Int where
  fakeRead a = fromMaybe 0 $
               elemIndex a $
               take (notation @Int) hCAlfabet

instance Arr HNums16 where
  fakeRead a = toEnum $
               fromMaybe 0 $
               elemIndex a $
               take (notation @HNums16) hCAlfabet

instance Arr HNumsL where
  fakeRead a = toEnum $
               fromMaybe 0 $
               elemIndex a $
               take (notation @HNumsL) hCAlfabet


-- HData
--   Получение данных из числа
-- >>> showH $ toHDataN @[Int] 6 1234
-- >>> showH $ toHDataN @[HNumsL] 6 1234
-- "001234"
-- "0000XD"

-- >>> showH [H12,H02,H13,H03,H14,H04]
-- "C2D3E4"

-- Code
--   Кодирование 1 раз
-- >>> showH $ code [H12,H02,H13,H03,H14,H04]
-- "6B6B6C"

--   Кодирование N раз
-- >>> showH $ codeN 66 [H12,H02,H13,H03,H14,H04]
-- "584C32"
-- >>> showH $ [H12,H02,H13,H03,H14,H04] -^> 66
-- "584C32"

--   Кодирование через пресет
-- >>> map showH $ getPreset @[HNums16] 6 66
-- >>> showH $ codePreset (getPreset @[HNums16] 6 66) [H12,H02,H13,H03,H14,H04]
-- ["02819A","2831B3","831DB4","11DB6C","9BB6CE","A34CEE"]
-- "584C32"

--   Декодировать 1 раз
-- >>> showH $ decode [H12,H02,H13,H03,H14,H04]
-- "425240"

--   Декодировать N раз
-- >>> showH $ decodeN 66 [H12,H02,H13,H03,H14,H04]
-- "323BB5"
-- >>> showH $ [H12,H02,H13,H03,H14,H04] <^- 66
-- "323BB5"

--   Декодирование через пресет
-- >>> map showH $  getPreset @[HNums16] 6 (-66)
-- >>> showH  $ codePreset (getPreset 6 (-66)) [H12,H02,H13,H03,H14,H04]
-- ["DC9CD0","C686CD","983869","C68352","DC65FE","0D92EB"]
-- "323BB5"


-- Tape
--   Основные данные заданного кода в ленте
-- >>> toTape [H12,H02,H13,H03,H14,H04]
-- HTape {tapeId = [H00,H00,H07,H04,H05,H04], tapeOffset = 275, tapeAntiOffset = 229, tapeLength = 504}


-- TapeInfo
--   Узнать какие ленты можно получить из суммы двух, смещая 2 ленту до 500
-- >>> getSumsList @[Int] [0,0,1] [0,0,3]
-- [[0,0,4],[0,0,1],[0,0,3],[0,0,2]]

--   Узнать как из ленты a и b получить ленту c
--     Результат: (требуемое смещение b, полученное смещение c)
-- >>> take 10 $ getOfsetsSums @[Int] [0,0,1] [0,0,7] [0,0,3]
-- [(1,198),(2,305),(3,110),(5,370),(8,233),(10,285),(11,253),(13,332),(15,240),(19,20)]
