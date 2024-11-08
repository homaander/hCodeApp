module Code.HomaCode.Tape (Tape(..)) where

import Code.HomaCode.Code
import Code.HomaCode.Math
import Code.HomaCode.Data


import Data.Maybe (fromJust)

class Code a => Tape a where
  toTape   :: a -> HTape a
  fromTape :: HTape a -> a

  getTapeId     :: a -> a
  getTapeLength :: a -> Int
  getTapeList   :: a -> [a]

  -- default
  fromTape (HTape h n _ _) = codeN n h

  getTapeId  = minimum . getTapeList

  getTapeLength hdata = fromJust $ findOffset hdata hdata
  getTapeList   hdata = fromJust $ findList   hdata hdata


instance (Enum a, Math a) => Tape [a] where
  toTape hdata = HTape hid offset (len - offset) len
    where
      offset  = if offset' /= len then offset' else 0
      offset' = fromJust $ findOffset hid hdata
      len     = getTapeLength hdata
      hid     = getTapeId     hdata