{-# LANGUAGE FlexibleInstances #-}

module Code.HomaCode.Data (
    HNums16(..)
  , HNumsL (..)
  , toLetter
  , showHCode

  , HTape (..)

  , HNum (..)
  , toLetterHN
  , showHCodeHN

  , hcodeAlfebet
) where

import Control.Parallel.Strategies
import Control.DeepSeq

hcodeAlfebet :: String
hcodeAlfebet  =  "0123456789AB"
             <> "CDEFGHIJKLMN"
             <> "OPQRSTUVWXYZ_"

data HNums16 = H00 | H01 | H02 | H03
             | H04 | H05 | H06 | H07
             | H08 | H09 | H10 | H11
             | H12 | H13 | H14 | H15
  deriving (Enum, Eq, Ord, Show)

data HNumsL  = L00 | L01 | L02 | L03 | L04 | L05
             | L06 | L07 | L08 | L09 | L10 | L11
             | L12 | L13 | L14 | L15 | L16 | L17
             | L18 | L19 | L20 | L21 | L22 | L23
             | L24 | L25 | L26 | L27 | L28 | L29
             | L30 | L31 | L32 | L33 | L34 | L35 | L36
  deriving (Enum, Eq, Ord, Show)

toLetter :: Enum a => a -> Char
toLetter a = hcodeAlfebet !! fromEnum a

showHCode :: Enum a => [a] -> String
showHCode  = mconcat . map (pure . toLetter)



data HNum = HN Int Int
  deriving (Show, Eq)

toLetterHN :: HNum -> Char
toLetterHN (HN _ a) = hcodeAlfebet !! a

showHCodeHN :: [HNum] -> String
showHCodeHN a = mconcat $ map (pure . toLetterHN) a


data HTape hdata = HTape { tapeId         :: hdata
                         , tapeOffset     :: Int
                         , tapeAntiOffset :: Int
                         , tapeLength     :: Int }
  deriving Show


instance NFData HNumsL where 
  rnf a = seq a ()