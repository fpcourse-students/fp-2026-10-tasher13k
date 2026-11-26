module SpecLevel2 where

import Control.Exception (evaluate)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (sort)
import Level1 (Point (..))
import Level2
import MyIO
import System.IO.Error (isUserError, tryIOError)
import System.Timeout (timeout)
import Test.Prelude

tests :: NamedTests
tests = nameTests 2
  [ testDesugared
  , testSumAndMult
  , testEdges
  , testCenterMass
  , testProbSolution
  ]

testDesugared :: Test
testDesugared = TestList
  [ TestCase $ assertEqual "Maybe: несовпадение образца в успешном чтении"
      Nothing (desugared (Just [1]) (Just ()))
  , propertyToTest "Maybe: успешное сопоставление и результат отметки" \x y mark ->
      let row = Just [x, y] in sugared row mark === desugared row mark
  , TestCase $ assertEqual "список: несовпадение отбрасывает только текущую ветвь"
      [3, 6, 12, 10, 7, 10, 16, 14] (desugared [[], [1, 2], [3, 3, 3], [2, 5]] [()])
  , TestCase do
      trace <- newIORef []
      let readRow = modifyIORef' trace (++ ["read"]) >> pure [2, 3]
      result <- desugared readRow (modifyIORef' trace (++ ["mark"]))
      assertEqual "результат двух чтений" 10 result
      assertEqual "порядок действий" ["read", "mark", "read"] =<< readIORef trace
  , TestCase do
      result <- tryIOError $ desugared (pure [1, 2, 3]) (assertFailure "mark после несовпадения")
      case result of
        Left err -> assertBool "fail в IO" (isUserError err)
        Right n -> assertFailure $ "Ожидалась ошибка сопоставления, получено " ++ show n
  ]

testSumAndMult :: Test
testSumAndMult = TestList
  [ TestCase $ assertEqual "текущая компонента" (Point True False) isFirstComponent
  , propertyToTest "sumAndMult" \(x1 :: Int, y1, x2, y2) ->
      Point (x1 + x2) (y1 * y2) === sumAndMult (Point x1 y1) (Point x2 y2)
  ]

testEdges :: Test
testEdges = TestList
  [ checkPrefix "edges1" edges1
  , checkPrefix "edges2" edges2
  ]
  where
    checkPrefix name triples = TestCase do
      prefix <- timeout (3 * 1000000) $ evaluate $ force $ take (length result) triples
      case prefix of
        Just actual -> assertEqual name result actual
        Nothing -> assertFailure $ name ++ ": за 3 секунды не получены первые 20 троек. "
          ++ "Проверьте, конечен ли внутренний перебор при фиксированной гипотенузе."

    result =
      [ (5, 4, 3)
      , (10, 8, 6)
      , (13, 12, 5)
      , (15, 12, 9)
      , (17, 15, 8)
      , (20, 16, 12)
      , (25, 20, 15)
      , (25, 24, 7)
      , (26, 24, 10)
      , (29, 21, 20)
      , (30, 24, 18)
      , (34, 30, 16)
      , (35, 28, 21)
      , (37, 35, 12)
      , (39, 36, 15)
      , (40, 32, 24)
      , (41, 40, 9)
      , (45, 36, 27)
      , (50, 40, 30)
      , (50, 48, 14)
      ]

testCenterMass :: Test
testCenterMass = TestList
  [ TestCase $ assertEqual "центр масс: отрицательные координаты и div" (Point3 (-2) 1 4) $
      massCenter [Object 1 (Point3 0 4 2), Object 2 (Point3 (-2) 0 5)]
  , TestCase $ assertEqual "one object" (unsafeWorld [] [2, 3, 4]) $ fst $ runUnsafeMyIO centerMain $ world [1, 2, 3, 4]
  , TestCase $ assertEqual "three objs" (unsafeWorld [] [4, 10, 5]) $ fst $ runUnsafeMyIO centerMain $ world
      [ 3, 4, 42, 5
      , 2, 3, 10, 23
      , 10, 5, 1, 2
      ]
  ]

testProbSolution :: Test
testProbSolution = TestList
  [ TestCase $ assertEqual "повторы исходов при двух гранях" [4, 4, 4, 6] $ sort (allEvenOutcomes 2)
  , TestCase $ assertBool "сумма 14 недостижима" $ cmp 0 $ probSolution 4
  , TestCase $ assertBool "probSolution 10" $ cmp 0.138 $ probSolution 10
  , TestCase $ assertBool "probSolution 33" $ cmp 0.00434 $ probSolution 33
  ]
  where
    cmp q w = abs (q - w) < 0.00001
