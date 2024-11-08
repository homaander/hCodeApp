import Code.HomaCode
import Code.HomaCode.Data


main :: IO ()
main = do
  putStrLn "Test start"

  test (showHCode [H12,H02,H13,H03,H14,H04]) "C2D3E4"

  -- HData
  --   Получение данных из числа
  test (showHCode $ toHDataN @[Int] 6 1234)    "001234"
  test (showHCode $ toHDataN @[HNumsL] 6 1234) "0000XD"


  -- Code
  --   Кодирование 1 раз
  test (showHCode $ code [H12,H02,H13,H03,H14,H04]) "6B6B6C"
  --   Кодирование N раз
  test (showHCode $ codeN 66 [H12,H02,H13,H03,H14,H04]) "584C32"
  test (showHCode $ [H12,H02,H13,H03,H14,H04] -^> 66) "584C32"

  --   Кодирование через пресет
  let preset66 = getPreset @[HNums16] 6 66
  test (showHCode $ runPreset preset66 [H12,H02,H13,H03,H14,H04]) "584C32"


  --   Декодировать 1 раз
  test (showHCode $ decode [H12,H02,H13,H03,H14,H04]) "425240"
  --   Декодировать N раз
  test (showHCode $ decodeN 66 [H12,H02,H13,H03,H14,H04]) "323BB5"
  test (showHCode $ [H12,H02,H13,H03,H14,H04] <^- 66) "323BB5"

  --   Декодирование через пресет
  let preset_66 = getPreset @[HNums16] 6 (-66)
  test (showHCode $ runPreset preset_66 [H12,H02,H13,H03,H14,H04]) "323BB5"

  -- Tape
  --   Основные данные заданного кода в ленте
  test (show $ toTape [H12,H02,H13,H03,H14,H04]) (show $ HTape {tapeId = [H00,H00,H07,H04,H05,H04], tapeOffset = 275, tapeAntiOffset = 229, tapeLength = 504})

  -- TapeInfo
  --   Узнать какие ленты можно получить из суммы двух, смещая 2 ленту до 500
  test (getSumsList @[Int] [0,0,1] [0,0,3]) [[0,0,4],[0,0,1],[0,0,3],[0,0,2]]

  --   Узнать как из ленты a и b получить ленту c
  --     Результат: (требуемое смещение b, полученное смещение c)
  test (take 10 $ getOfsetsSums @[Int] [0,0,1] [0,0,7] [0,0,3]) [(1,198),(2,305),(3,110),(5,370),(8,233),(10,285),(11,253),(13,332),(15,240),(19,20)]

  putStrLn "Test Ok"




test :: (Eq a, Show a) => a -> a -> IO ()
test a b = if a == b
           then putStrLn ("Ok: " <> show a <> " , " <> show b)
           else error ("Failed: " <> show a <> " , " <> show b)
