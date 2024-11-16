{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Main(main) where

import Monomer

import Control.Lens

import Data.List (transpose)
import Data.Text (Text)
import qualified Data.Text as T
import TextShow ( TextShow(showt) )

import Code.HomaCode.Data
import qualified Code.HomaCode as HC
-- import Code.HomaCode.Parallel


-- import Data.Maybe
-- import qualified Monomer.Lens as L
-- import Monomer.Core.Lens (HasFontSize(fontSize))

data AppModel = AppModel {
      _codeText           :: Text
    , _codeTable          :: [[HNum]]

    , _codeNC             :: Int

    , _selectDataBase     :: HBase
    , _selectRowNum       :: Int

    , _tapeInfoId         :: Text
    , _tapeInfoOffset     :: Int
    , _tapeInfoAntiOffset :: Int
    , _tapeInfoLength     :: Int
  }
    deriving (Eq, Show)

makeLenses 'AppModel

data AppEvent = AppInit

              | AppDecode
              | AppCode
              | AppTapeInfo

              | AppToTable
              | AppFromTable

              | AppTUp
              | AppTDown
              | AppTLeft
              | AppTRight
  deriving (Eq, Show)


type WidgetEnv'    = WidgetEnv AppModel AppEvent
type WidgetNode'   = WidgetNode AppModel AppEvent
type AppEventResp' = AppEventResponse AppModel AppEvent


buildUI :: WidgetEnv' -> AppModel -> WidgetNode'
buildUI _ model = widgetTree where
  widgetTree = vstack
    [
      label "H_Code" `styleBasic` [ textSize 32 ]
    , spacer

    -- , label $ T.pack $ show incodeT

    , label "Default code:"
    , spacer

    , hgrid [
        vstack [
            numericField codeNC `styleBasic` [textCenter]
          , dropdown selectDataBase [2, 10, 16, 37] (\sRow -> hstack [ label "Base: ", label $ showt sRow ]) (label . showt)
          , spacer
          , textField codeText `styleBasic` [textCenter]
          , hgrid [
                button "<----" AppDecode
              , button "---->" AppCode
              ]
                `styleBasic` [width 300]
          ]
            `styleBasic` [paddingH 10, width 300]
      , vstack [
            button "Get Info" AppTapeInfo
          , spacer
          , hstack [
                vstack [
                    label "Rank:"
                  , label "Base:"
                  , label "Val:"
                  , label "---"
                  , label "Tape ID Val:"
                  , label "---"
                  , label "Tape ID:"
                  , label "Offset:"
                  , label "Anti Offset:   "
                  , label_ "Length:\n " [multiline]
                  ]
              , vstack [
                    label $ showt $ length $ T.unpack $ model ^. codeText
                  , label $ showt baseVal
                  , label $ showt $ HC.fromHData codeHN
                  , label "---"
                  , label $ showt $ HC.fromHData tapeInfoIdHN
                  , label "---"
                  , label tapeInfoIdT
                  , label $ showt $ model ^. tapeInfoOffset
                  , label $ showt $ model ^. tapeInfoAntiOffset
                  , label $ showt $ model ^. tapeInfoLength
                  ]
              ]
          ]
            `styleBasic` [paddingH 40, width 200]
    ]

    , label "Table:"
    , spacer

    , hgrid [

        vstack [
            hgrid [
              dropdown selectRowNum [1 .. 5] (\sRow -> hstack [ label "Row: ", label $ showt sRow ]) (label . showt)
            , button "Write row"   AppToTable
            , button "Read row" AppFromTable
            ]
          , spacer
          , box_ [alignCenter] (vstack [ hgrid [ label (T.pack $ mconcat [[toLetter a] <> " " | a <- b]) `styleBasic` [textSize 20] ] | b <- incodeT])
              `styleBasic` [paddingH 40, width 200]
          ]

        , vstack [
              button "Up"   AppTUp
            , hgrid
                [ button "<-"   AppTLeft
                , button "->"   AppTRight
                ]
            , button "Down" AppTDown
            ] `styleBasic` [paddingH 40, width 200]
        ]
    ]
      `styleBasic` [ padding 10 ]

  incodeT = model ^. codeTable
  codeVal  = model ^. codeText
  baseVal  = model ^. selectDataBase

  tapeInfoIdT = model ^. tapeInfoId
  tapeInfoIdHN = HC.getHCodeText baseVal tapeInfoIdT
  codeHN = HC.getHCodeText baseVal codeVal



handleEvent :: WidgetEnv' -> WidgetNode' -> AppModel -> AppEvent -> [AppEventResp']
handleEvent _ _ model evt =
  case evt of
    AppInit     -> []
    AppDecode   -> [ Model $ model & codeText .~ fst dataV ]
    AppCode     -> [ Model $ model & codeText .~ snd dataV ]
    AppTapeInfo -> [ Model $ model
                       & tapeInfoId         .~ tapeId         tapeV
                       & tapeInfoOffset     .~ tapeOffset     tapeV
                       & tapeInfoAntiOffset .~ tapeAntiOffset tapeV
                       & tapeInfoLength     .~ tapeLength     tapeV
                       ]
    AppTDown    -> [ Model $ model & codeTable .~ fst dataTR ]
    AppTUp      -> [ Model $ model & codeTable .~ snd dataTR ]
    AppTRight   -> [ Model $ model & codeTable .~ fst dataT  ]
    AppTLeft    -> [ Model $ model & codeTable .~ snd dataT  ]

    AppToTable   -> [ Model $ model & codeTable .~ incodeTUpdate ]
    AppFromTable -> [ Model $ model & codeText  .~ HC.showHCodeText (incodeT !! (rowNum - 1)) ]
  where
    -- Code / Decode
    baseVal  = model ^. selectDataBase
    codeVal  = model ^. codeText
    codeNVal = model ^. codeNC

    rowNum = model ^. selectRowNum

    codeHN = HC.getHCodeText baseVal codeVal

    dataV = HC.dataText codeNVal    codeHN
    tapeV = HC.tapeText $ HC.toTape codeHN

    -- Table Code / Decode
    incodeT  = map (HC.resetBase baseVal) $ model ^. codeTable
    incodeTR = transpose incodeT

    incodeTUpdate = take (rowNum - 1) incodeT <> [codeHN] <> drop rowNum incodeT

    dataT  = (map HC.code incodeT, map HC.decode incodeT)
    dataTR = (transpose $ map HC.code incodeTR, transpose $ map HC.decode incodeTR)

-- main :: IO ()
-- main = do
--   let
--     -- from 2'565'726'409 (2565726409)
--     -- res: 142'540'356 (142540356)
--     dat = HC.getHCodeText 37 "ANDREW"
--     preset  = HC.getPreset @HNum 37 6 100000
--     results = iterate (HC.runPreset preset) dat !! 1425
--   print $ showHCode $ HC.codeN 40356 results

main :: IO ()
main = do
    startApp model handleEvent buildUI config
  where
    config = [
        appWindowTitle "H Code App"
      , appWindowIcon  "./assets/images/icon.png"
      , appTheme       darkTheme
      , appFontDef     "Regular" "./assets/fonts/FiraCode-Light.ttf"
      , appInitEvent   AppInit
      ]

    model = AppModel {
        _codeText   = "12345"
      , _tapeInfoId = "?????"
      , _codeTable = [
                       HC.toHDataN 10 5 13243
                     , HC.toHDataN 10 5 67521
                     , HC.toHDataN 10 5 43212
                     , HC.toHDataN 10 5 98721
                     , HC.toHDataN 10 5 84328
                     ]
      , _codeNC             = 1
      , _selectDataBase     = 10
      , _selectRowNum       = 1
      , _tapeInfoLength     = 0
      , _tapeInfoOffset     = 0
      , _tapeInfoAntiOffset = 0
      }

