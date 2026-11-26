module SpecLevel3 where

import Level3
import Test.Prelude

tests :: NamedTests
tests = nameTests 3 [testProbShark]

testProbShark :: Test
testProbShark = TestList
  [ TestCase $ assertDoubleEq "probShark 1" 1 $ probShark 1
  , TestCase $ assertDoubleEq "probShark 2" 0.75 $ probShark 2
  , TestCase $ assertDoubleEq "probShark 3" 0.4444444444 $ probShark 3
  , TestCase $ assertDoubleEq "probShark 4" 0.4453125 $ probShark 4
  , TestCase $ assertDoubleEq "probShark 5" 0.3766400000 $ probShark 5
  , TestCase $ assertDoubleEq "probShark 6" 0.3556884431 $ probShark 6
  ]
  where
    assertDoubleEq msg a b = assertBool msg (abs (a - b) < 0.000001)
