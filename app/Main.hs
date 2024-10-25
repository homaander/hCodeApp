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
import MyParallel.ParallelHC

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


exampleData1 :: [Int]
exampleData1 = HC.getArr @Int "34209472"

exampleData2 :: [HNumsL]
exampleData2 = HC.getArr @HNumsL "ANDREW"

resultId2 :: [HNumsL]
resultId2 = HC.getArr @HNumsL "0000O9"

main :: IO ()
-- main = mainUI
-- main = print $ getStarter (HC.getArr @HNumsL "ANDREW") 
-- main = print $ getStarter (HC.getArr @HNumsL "_IPG0Q") 
-- main = print $ getStarter (HC.getArr @HNumsL "UR510Q") 
-- main = print $ getStarter (HC.getArr @HNumsL "79OBUE") 
-- main = print $ getStarter (HC.getArr @HNumsL "C67XQA") 
-- main = print $ getStarter (HC.getArr @HNumsL "6V0_AW") 
-- main = print $ getStarter (HC.getArr @HNumsL "YBXEZN") 
-- main = print $ getStarter (HC.getArr @HNumsL "PKOE_Q") 
main = print $ getStarter (HC.getArr @HNumsL "MML7WC") 

-- [[ANDREW],[P0B8I0],[T93GML],[7UN3A7],[0SMGAQ]]
-- [[_IPG0Q],[Q68ANA],[V_8MVU],[ZXVFWD],[YX33BI]]
-- [[UR510Q],[TA72WN],[8NH663],[91FYL3],[N08J9V]]
-- [[79OBUE],[IWI15_],[JU1_H5],[K8CSQ5],[W164K8]]
-- [[C67XQA],[L432DC],[SV9PV1],[2665EO],[L6X2N1]]
-- [[6V0_AW],[G348NI],[VF9APK],[U3IIM6],[HMS4FB]]
-- [[YBXEZN],[G77J6I],[JL7JGR],[B6PD45],[6RGUQ1]]
-- [[PKOE_Q],[E4HKGW],[_1LPOI],[L7YJX5],[F1ZEZM]]
-- [[MML7WC],[_EUDTO],[G8X3HA],[R0MBVO],[UHBPEL],[KH57V6]]


-- Find ID
-- main = print $ getTapeIdParallel exampleData2 [0..1000]
-- [0,0,0,0,O,9] for 10'000'000

-- Find len max = 2'565'726'409 -> 256'573 -> 1'100

-- main = print $ getOffsetParallel (HC.getArr @HNumsL "ANDREW") exampleData2 [0..250]   -- 0 - 2'500'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "P0B8I0") exampleData2 [0..250]   -- 2'500'000 - 5'000'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "T93GML") exampleData2 [0..250]   -- 5'000'000 - 7'500'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "7UN3A7") exampleData2 [0..250]   -- 7'500'000 - 10'000'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "0SMGAQ") exampleData2 [0..250]   -- 10'000'000 - 12'500'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "_IPG0Q") exampleData2 [0..250]   -- 12'500'000 - 15'000'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "Q68ANA") exampleData2 [0..250]   -- 15'000'000 - 17'500'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "V_8MVU") exampleData2 [0..250]   -- 17'500'000 - 20'000'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "ZXVFWD") exampleData2 [0..250]   -- 20'000'000 - 22'500'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "YX33BI") exampleData2 [0..250]   -- 22'500'000 - 25'000'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "UR510Q") exampleData2 [0..250]   -- 25'000'000 - 27'500'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "TA72WN") exampleData2 [0..250]   -- 27'500'000 - 30'000'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "8NH663") exampleData2 [0..250]   -- 30'000'000 - 32'500'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "91FYL3") exampleData2 [0..250]   -- 32'500'000 - 35'000'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "N08J9V") exampleData2 [0..250]   -- 35'000'000 - 37'500'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "79OBUE") exampleData2 [0..250]   -- 37'500'000 - 40'000'000
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "IWI15_") exampleData2 [0..250]   -- 42
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "JU1_H5") exampleData2 [0..250]   -- 45
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "K8CSQ5") exampleData2 [0..250]   -- 47
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "W164K8") exampleData2 [0..250]   -- 50
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "C67XQA") exampleData2 [0..250]   -- 52
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "L432DC") exampleData2 [0..250]   -- 55
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "SV9PV1") exampleData2 [0..250]   -- 57
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "2665EO") exampleData2 [0..250]   -- 60
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "L6X2N1") exampleData2 [0..250]   -- 62
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "6V0_AW") exampleData2 [0..250]   -- 65
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "G348NI") exampleData2 [0..250]   -- 67
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "VF9APK") exampleData2 [0..250]   -- 70
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "U3IIM6") exampleData2 [0..250]   -- 72
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "HMS4FB") exampleData2 [0..250]   -- 75
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "YBXEZN") exampleData2 [0..250]   -- 77
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "G77J6I") exampleData2 [0..250]   -- 80
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "JL7JGR") exampleData2 [0..250]   -- 82
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "B6PD45") exampleData2 [0..250]   -- 85
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "6RGUQ1") exampleData2 [0..250]   -- 87
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "PKOE_Q") exampleData2 [0..250]   -- 90
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "E4HKGW") exampleData2 [0..250]   -- 92
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "_1LPOI") exampleData2 [0..250]   -- 95
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "L7YJX5") exampleData2 [0..250]   -- 97
-- main = print $ getOffsetParallel (HC.getArr @HNumsL "F1ZEZM") exampleData2 [0..250]   -- 100


-- main = print $ getOffsetParallel (HC.getArr @HNumsL "8N666") (HC.getArr @HNumsL "8N6") [0..100]

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


mainUI :: IO ()
mainUI = do
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

