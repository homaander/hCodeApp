module Code.HomaCode.TapeInfo (TapeInfo(..)) where

import Code.HomaCode.Tape
import Code.HomaCode.Math
import Code.HomaCode.Code
import Code.HomaCode.Data

import Data.Maybe (mapMaybe)
import Data.List (nub)



class Tape a => TapeInfo a where
  getSumsList :: a -> a -> [a]
  getOfsetsSums :: a -> a -> a ->  [(Int,Int)]

instance (Enum a, Math a) => TapeInfo [a] where
  getSumsList aTape bTape = nub $ map (\n -> getTapeId (aTape ^+ codeN n bTape)) [0 .. 500]

  getOfsetsSums aTape bTape resTape = mapMaybe check [0 .. 500]
    where
      lid = getTapeId resTape
      check n = if tapeId nsumd == lid then Just (n, tapeAntiOffset nsumd) else Nothing
        where
          nsum  = aTape ^+ codeN n bTape
          nsumd = toTape nsum