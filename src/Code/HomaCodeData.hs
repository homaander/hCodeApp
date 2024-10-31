module Code.HomaCodeData (
    HNums16(..)
  , HNumsL (..)
  , HTape (..)
  , hCAlfabet
) where

import Control.Parallel.Strategies
import Control.DeepSeq

data HNums16 = H00 | H01 | H02 | H03
             | H04 | H05 | H06 | H07
             | H08 | H09 | H10 | H11
             | H12 | H13 | H14 | H15
  deriving (Enum, Eq, Ord)

data HNumsL  = L00 | L01 | L02 | L03 | L04 | L05
             | L06 | L07 | L08 | L09 | L10 | L11
             | L12 | L13 | L14 | L15 | L16 | L17
             | L18 | L19 | L20 | L21 | L22 | L23
             | L24 | L25 | L26 | L27 | L28 | L29
             | L30 | L31 | L32 | L33 | L34 | L35
             | L36
  deriving (Enum, Eq, Ord)

data HTape hdata = HTape {
    tapeId         :: hdata
  , tapeOffset     :: Int
  , tapeAntiOffset :: Int
  , tapeLength     :: Int
}
  deriving Show



hCAlfabet :: [Char]
hCAlfabet  = [ '0', '1', '2', '3', '4', '5'
           , '6', '7', '8', '9', 'A', 'B'
           , 'C', 'D', 'E', 'F', 'G', 'H'
           , 'I', 'J', 'K', 'L', 'M', 'N'
           , 'O', 'P', 'Q', 'R', 'S', 'T'
           , 'U', 'V', 'W', 'X', 'Y', 'Z'
           , '_'
           ]

instance Show HNums16 where
  show a = [hCAlfabet !! fromEnum a]

instance Show HNumsL where
  show a = [hCAlfabet !! fromEnum a]



instance NFData HNumsL where 
  rnf a = seq a ()

