module Code.HomaCode.TapeInfo (TapeInfo(..)) where

import Code.HomaCode.Tape
import Code.HomaCode.Math
import Code.HomaCode.Code
import Code.HomaCode.Data

import Data.Maybe (mapMaybe, fromJust)
import Data.List (nub, intersect)



class Tape a => TapeInfo a where
  getSumsList   :: Int -> [a] -> [a] -> [[a]]
  getOfsetsSums :: Int -> [a] -> [a] ->  [a]  ->  [(Int,Int)]

  trapFinderLengthHead :: HBase -> HRank -> [a] -> [a]
  trapFinderLength :: [a] -> [a] -> Int

instance TapeInfo HNum where
  -- Get Tape Id from (a + b_offset)
  getSumsList c aTape bTape = nub $ map (\n -> getTapeId (aTape ^+ codeN n bTape)) [0 .. c]

  -- Get (b_offset, c_anti_offset) from (a + b_offset = c_offset)
  getOfsetsSums c aTape bTape resTape = mapMaybe check [0 .. c]
    where
      lid = getTapeId resTape
      check b_offset = if tapeId nsumd == lid then Just (b_offset, tapeAntiOffset nsumd) else Nothing
        where
          nsum  = aTape ^+ codeN b_offset bTape
          nsumd = toTape nsum

  -- Get length tape for big tapes
  --   Example: (base: 37, rank: 6)
  trapFinderLengthHead base rank dat = head $ results `intersect` trap
    where
      trap    = codeNList 100000 dat
      preset  = getPreset @HNum base rank (-100000)
      results = take 10000 $ iterate (runPreset preset) dat

  trapFinderLength a b = (100000 * fst (head $ filter (\(_, r) -> r == b) dd)) + step
    where
      step = fromJust $ findOffset a b

      preset  = getPreset @HNum 37 6 (-100000)
      dd      = zip [0..] results
      results = take 10000 $ iterate (runPreset preset) a

-- "ANDREW" = 40356 + (100'000 x)
-- in 100k: "V2SZMU"
-- 142640356