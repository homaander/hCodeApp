{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Main(main) where

-- import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (transpose)

import Monomer
-- import qualified Monomer.Lens as L
import Control.Lens
import TextShow ( TextShow(showt) )


import Cfg
import Templates.Blocks

import Code.HomaCode


-- main :: IO ()
-- main = do
--   let
--     base = 37
--     rank = 6 
--   print $ HC.trapFinderLength base rank (HC.getHCodeText base "ANDREW")

--   -- let
--   --   dat     = HC.getHCodeText 37 "ANDREW"
--   --   preset  = HC.getPreset @HNum 37 6 100000
--   --   results = iterate (HC.runPreset preset) dat !! 1425
--   -- print $ showHCode $ HC.codeN 40356 results

main :: IO ()
main = do
    startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "H Code App",
      appWindowIcon  "./assets/images/icon.png",
      appWindowState (MainWindowNormal (800, 800)),
      appTheme       darkTheme,
      appFontDef     "Regular" "./assets/fonts/FiraCode-Light.ttf",
      appInitEvent   AppInit
      ]

    model = AppModel {
      _codeText   = "12345",
      _tapeInfoId = "?????",
      _codeTable = [
        toHDataN 10 5 13243,
        toHDataN 10 5 67521,
        toHDataN 10 5 43212,
        toHDataN 10 5 98721,
        toHDataN 10 5 84328
        ],
      _codeNC             = 1,
      _selectDataBase     = 10,
      _selectRowNum       = 1,
      _tapeInfoLength     = 0,
      _tapeInfoOffset     = 0,
      _tapeInfoAntiOffset = 0
      }


buildUI :: WidgetEnv' -> AppModel -> WidgetNode'
buildUI _ model = widgetTree
  where
    widgetTree = vstack [

      label "H_Code" `styleBasic` [ textSize 32 ],
      spacer,

      label "Default code:",
      spacer,

      hgrid [
        vstack [
          numericField codeNC
            `styleBasic` [textCenter],

          dropdown selectDataBase [2, 10, 16, 37]
            (\sRow -> hstack [ label "Base: ", label $ showt sRow ]) (label . showt),

          spacer,

          textField codeText
            `styleBasic` [textCenter],

          hgrid [
            button "<----" AppDecode,
            button "---->"   AppCode
            ]
              `styleBasic` [width 300]
          ]
            `styleBasic` [paddingH 10, width 300],

        blockInfo model
        ],

      label "Table:",

      spacer,

      blockMatrix model
      ]
        `styleBasic` [ padding 10 ]



handleEvent :: WidgetEnv' -> WidgetNode' -> AppModel -> AppEvent -> [AppEventResp']
handleEvent _ _ model evt =
  case evt of
    AppInit      -> []
    AppDecode    -> [ Model $ model & codeText .~ fst dataV ]
    AppCode      -> [ Model $ model & codeText .~ snd dataV ]
    AppTapeInfo  -> [ Model $ model
                      & tapeInfoId         .~ tapeId         tapeV
                      & tapeInfoOffset     .~ tapeOffset     tapeV
                      & tapeInfoAntiOffset .~ tapeAntiOffset tapeV
                      & tapeInfoLength     .~ tapeLength     tapeV
                      ]
    AppTDown     -> [ Model $ model & codeTable .~ fst dataTR ]
    AppTUp       -> [ Model $ model & codeTable .~ snd dataTR ]
    AppTRight    -> [ Model $ model & codeTable .~ fst dataT  ]
    AppTLeft     -> [ Model $ model & codeTable .~ snd dataT  ]

    AppToTable   -> [ Model $ model & codeTable .~ incodeTUpdate ]
    AppFromTable -> [ Model $ model
                      & codeText  .~ showHCodeText (incodeT !! (rowNum - 1))
                      ]
  where
    -- Code / Decode
    baseVal  = model ^. selectDataBase
    codeVal  = model ^. codeText
    codeNVal = model ^. codeNC

    rowNum = model ^. selectRowNum

    codeHN = getHCodeText baseVal codeVal

    dataV = dataText codeNVal    codeHN
    tapeV = tapeText $ toTape codeHN

    -- Table Code / Decode
    incodeT  = map (resetBase baseVal) $ model ^. codeTable
    incodeTR = transpose incodeT

    incodeTUpdate = take (rowNum - 1) incodeT <> [codeHN] <> drop rowNum incodeT

    dataT  = (map code incodeT, map decode incodeT)
    dataTR = (transpose $ map code incodeTR, transpose $ map decode incodeTR)