{-# LANGUAGE FlexibleInstances #-}

module Code.HomaCode.Data (
    HTape (..)

  , HNum (..)
  , toLetter
  , showHCode

  , hcodeAlfebet
) where

hcodeAlfebet :: String
hcodeAlfebet  =  "0123456789AB"
             <> "CDEFGHIJKLMN"
             <> "OPQRSTUVWXYZ_"

type HBase = Int
type HVal  = Int

data HNum = HN { hBase :: HBase
               , hVal  :: HVal }
  deriving (Show, Eq)

instance Ord HNum where
  (HN _ a) <= (HN _ b) = a <= b

toLetter :: HNum -> Char
toLetter (HN _ a) = hcodeAlfebet !! a

showHCode :: [HNum] -> String
showHCode a = mconcat $ map (pure . toLetter) a


data HTape hdata = HTape { tapeId         :: hdata
                         , tapeOffset     :: Int
                         , tapeAntiOffset :: Int
                         , tapeLength     :: Int }
  deriving Show