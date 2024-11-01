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
-- >>>  toHDataN @[Int] 6 1234
-- >>>  toHDataN @[HNumsL] 6 1234
-- [0,0,1,2,3,4]
-- [0,0,0,0,X,D]

-- Code
--   Кодирование 1 раз
-- >>> [H12,H02,H13,H04,H06]
-- >>> code [H12,H02,H13,H04,H06]
-- [C,2,D,4,6]
-- [2,7,B,6,C]

--   Кодирование N раз
-- >>> [H12,H02,H13,H03,H14,H04]
-- >>> codeN 66 [H12,H02,H13,H03,H14,H04]
-- [C,2,D,3,E,4]
-- [5,8,4,C,3,2]

-- >>> [H12,H02,H13,H03,H14,H04]
-- >>> [H12,H02,H13,H03,H14,H04] ^-> 66
-- [C,2,D,3,E,4]
-- [5,8,4,C,3,2]

--   Кодирование через пресет
-- >>> [H12,H02,H13,H03,H14,H04]
-- >>> getPreset @[HNums16] 6 66
-- >>> codePreset (getPreset @[HNums16] 6 66) [H12,H02,H13,H03,H14,H04]
-- [C,2,D,3,E,4]
-- [[0,2,8,1,9,A],[2,8,3,1,B,3],[8,3,1,D,B,4],[1,1,D,B,6,C],[9,B,B,6,C,E],[A,3,4,C,E,E]]
-- [5,8,4,C,3,2]

--   Декодировать 1 раз
-- >>> [H12,H02,H13,H03,H14,H04]
-- >>> decode [H12,H02,H13,H03,H14,H04]
-- [C,2,D,3,E,4]
-- [4,2,5,2,4,0]

--   Декодировать N раз
-- >>> [H12,H02,H13,H03,H14,H04]
-- >>> decodeN 66 [H12,H02,H13,H03,H14,H04]
-- [C,2,D,3,E,4]
-- [3,2,3,B,B,5]

--   Декодирование через пресет
-- >>> [H12,H02,H13,H03,H14,H04]
-- >>> getPreset @[HNums16] 6 (-66)
-- >>> codePreset (getPreset 6 (-66)) [H12,H02,H13,H03,H14,H04]
-- [C,2,D,3,E,4]
-- [[D,C,9,C,D,0],[C,6,8,6,C,D],[9,8,3,8,6,9],[C,6,8,3,5,2],[D,C,6,5,F,E],[0,D,9,2,E,B]]
-- [3,2,3,B,B,5]


-- Tape
--   Основные данные заданного кода в ленте
-- >>> toTape [H12,H02,H13,H03,H14,H04]
-- HTape {tapeId = [0,0,7,4,5,4], tapeOffset = 275, tapeAntiOffset = 229, tapeLength = 504}


-- TapeInfo
--   Узнать какие ленты можно получить из суммы двух, смещая 2 ленту до 500
-- >>> getSumsList @[Int] [0,0,1] [0,0,3]
-- [[0,0,4],[0,0,1],[0,0,3],[0,0,2]]

--   Узнать как из ленты a и b получить ленту c
--     Результат: (требуемое смещение b, полученное смещение c)
-- >>> getOfsetsSums @[Int] [0,0,1] [0,0,3] [0,0,2]
-- [(42,36),(56,21),(77,45),(98,29),(112,51),(126,7),(133,18),(147,49),(154,4),(168,53),(182,32),(189,9),(196,35),(203,47),(210,56),(217,31),(231,28),(259,12),(280,43),(287,16),(294,23),(301,59),(329,30),(336,20),(343,26),(378,38),(385,39),(399,52),(406,17),(413,6),(427,60),(476,36),(490,21)]
