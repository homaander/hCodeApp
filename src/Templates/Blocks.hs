module Templates.Blocks() where

-- import Monomer

-- import TextShow ( TextShow(showt) )

-- import Data.List (transpose)
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import TextShow ( TextShow(showt) )

-- import Code.HomaCode.Data
-- import qualified Code.HomaCode as HC

-- getInfoblock :: WidgetEvent e -> WidgetNode s e
-- getInfoblock d = vstack [ 
--     button "Get Info" d,

--     spacer,

--     hstack [ 
--       vstack [
--         label "Rank:",
--         label "Base:",
--         label "Val:",
--         label "---",
--         label "Tape ID Val:",
--         label "---",
--         label "Tape ID:",
--         label "Offset:",
--         label "Anti Offset:   ",
--         label_ "Length:\n " [multiline]
--         ]
--       ]
--   ]
--        `styleBasic` [paddingH 40, width 200]