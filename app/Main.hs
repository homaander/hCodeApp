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

import qualified Code.HomaCode as HC
import Code.HomaCodeData

-- import MyParallel.ParallelTest
-- import MyParallel.ParallelHC

-- import Data.Maybe
-- import qualified Monomer.Lens as L
-- import Monomer.Core.Lens (HasFontSize(fontSize))

data AppModel = AppModel {
      _codeText           :: Text
    , _codeTable          :: [[Int]]

    , _codeNC             :: Int

    , _selectDataType     :: Text
    , _selectColumnNum    :: Int

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
    [ label "H_Code" `styleBasic` [ textSize 32 ]

    , label "Default code:"
    , spacer

    , hgrid [
        vstack [
            textField codeText `styleBasic` [textCenter]
          , spacer
          , numericField codeNC `styleBasic` [textCenter]
          , spacer
          , hgrid [
                dropdown selectDataType ["Int", "HNums16", "HNumsL"] label label
              , button "<----" AppDecode
              , button "---->" AppCode
              ]
                `styleBasic` [width 300]
          , spacer
          , hgrid [
                dropdown selectColumnNum [1 .. 5] (\sRow -> hstack [ label "Row: ", label $ showt sRow ]) (label . showt)
              , button "To   T"   AppToTable
              , button "From T" AppFromTable
              ]
          ]
            `styleBasic` [paddingH 10, width 300]
      , vstack [
            button "Get Info" AppTapeInfo
          , spacer
          , hstack [
                vstack [
                    label "Rank:"
                  , label "Tape:"
                  , label "Offset:"
                  , label "Anti Offset:   "
                  , label "Length:"
                  ]
              , vstack [
                    label $ showt $ length $ T.unpack $ model ^. codeText
                  , label $ model ^. tapeInfoId
                  , label $ showt $ model ^. tapeInfoOffset
                  , label $ showt $ model ^. tapeInfoAntiOffset
                  , label $ showt $ model ^. tapeInfoLength
                  ]
              ]
          ]
            `styleBasic` [paddingH 40, width 200]
    ]

    , spacer
    , label "Table:"
    , spacer

    , hgrid [
          vstack [ hgrid [ label (showt a) `styleBasic` [textSize 20, textCenter] | a <- b ] | b <- model ^. codeTable ]
            `styleBasic` [paddingH 40, width 200]

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
    AppTUp      -> [ Model $ model & codeTable .~ decodeTR ]
    AppTLeft    -> [ Model $ model & codeTable .~ decodeT  ]
    AppTDown    -> [ Model $ model & codeTable .~ codeTR   ]
    AppTRight   -> [ Model $ model & codeTable .~ codeT    ]

    AppToTable   -> [ Model model ]
    AppFromTable -> [ Model model]
  where
    -- Code / Decode
    typeVal  = model ^. selectDataType
    codeVal  = model ^. codeText
    codeNVal = model ^. codeNC

    dataV = case typeVal of
      "HNumsL"  -> HC.dataText codeNVal (HC.getArr @HNumsL  codeVal)
      "HNums16" -> HC.dataText codeNVal (HC.getArr @HNums16 codeVal)
      _         -> HC.dataText codeNVal (HC.getArr @Int     codeVal)

    tapeV = case typeVal of
      "HNumsL"  -> HC.tapeText $ HC.toTape (HC.getArr @HNumsL  codeVal)
      "HNums16" -> HC.tapeText $ HC.toTape (HC.getArr @HNums16 codeVal)
      _         -> HC.tapeText $ HC.toTape (HC.getArr @Int     codeVal)

    -- Table Code / Decode
    incodeT  = model ^. codeTable
    incodeTR = transpose incodeT

    codeT   = map HC.code   incodeT
    decodeT = map HC.decode incodeT

    codeTR   = transpose $ map HC.code   incodeTR
    decodeTR = transpose $ map HC.decode incodeTR


main :: IO ()
main = do
    startApp model handleEvent buildUI config
  where
    config = [
        appWindowTitle "H Code App"
      , appWindowIcon  "./assets/images/icon.png"
      , appTheme       darkTheme
      , appFontDef     "Regular" "./assets/fonts/Roboto-Regular.ttf"
      , appInitEvent   AppInit
      ]
    model = AppModel
              "12345"
              [ [6,4,2,8,3]
              , [2,4,5,7,6]
              , [3,5,3,1,2]
              , [9,9,2,4,6]
              , [1,2,3,5,7]
              ]
              1 "Int" 1
              "?????" 0 0 0

