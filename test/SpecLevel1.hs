module SpecLevel1 where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Level1
import Test.Prelude

tests :: NamedTests
tests = nameTests 1
  [ testBossBoss
  , testPointMonad
  , TestList [testBind, testJoin]
  , testDubEven
  ]

testBossBoss :: Test
testBossBoss = TestList
  [ TestCase $ assertEqual "no auth" Nothing $ bossBoss (UserId 1 `Set.delete` auth) users (UserId 1)
  , TestCase $ assertEqual "no user" Nothing $ bossBoss auth (UserId 1 `Map.delete` users) (UserId 1)
  , TestCase $ assertEqual "no boss" Nothing $ bossBoss auth (UserId 2 `Map.delete` users) (UserId 1)
  , TestCase $ assertEqual "у сотрудника нет начальника" Nothing $
      bossBoss auth (Map.insert (UserId 1) (user1 { userBoss = Nothing }) users) (UserId 1)
  , TestCase $ assertEqual "у начальника нет начальника" Nothing $
      bossBoss auth (Map.insert (UserId 2) (user2 { userBoss = Nothing }) users) (UserId 1)
  , TestCase $ assertEqual "нет записи о втором начальнике" Nothing $
      bossBoss auth (Map.delete (UserId 3) users) (UserId 1)
  , TestCase $ assertEqual "достаточно авторизации сотрудника" (Just user3) $
      bossBoss auth users (UserId 1)
  ]
  where
    auth = Set.singleton (UserId 1)
    user1 = User (UserId 1) (UserName "Bob") (Just $ UserId 2)
    user2 = User (UserId 2) (UserName "Alice") (Just $ UserId 3)
    user3 = User (UserId 3) (UserName "Frank") (Just $ UserId 4)
    user4 = User (UserId 4) (UserName "Koka") (Just $ UserId 10)
    users = Map.fromList $ map (\user -> (userId user, user))
      [user1, user2, user3, user4]

testPointMonad :: Test
testPointMonad = TestList
  [ lawsToTest $ functorLaws $ Proxy @Point
  , lawsToTest $ applicativeLaws $ Proxy @Point
  , lawsToTest $ monadLaws $ Proxy @Point
  , TestCase $ assertEqual "пример из условия" (Point 13 16) do
      a <- Point 3 4
      b <- Point 10 12
      pure (a + b)
  , TestCase $ assertEqual "каждая координата выбирает свою ветку" (Point 3 50) $
      Point 2 5 >>= \n -> Point (n + 1) (10 * n)
  ]

deriving stock instance Generic (Point a)

instance Arbitrary a => Arbitrary (Point a) where
  arbitrary = Point <$> arbitrary <*> arbitrary
  shrink = genericShrink

testBind :: Test
testBind = TestList
  [ TestCase $ assertEqual "bind works for just" (Just 42) $ Just 1 `bind` \x -> Just (x + 41)
  , TestCase $ assertEqual "bind works for nothing (1)" Nothing $ Nothing `bind` \x -> Just (x + 41)
  , TestCase $ assertEqual "bind works for nothing (2)" Nothing $ Just 1 `bind` const (Nothing @Int)
  ]

testJoin :: Test
testJoin = TestList
  [ TestCase $ assertEqual "join works for just" (Just 42) $ join' $ Just (Just 42)
  , TestCase $ assertEqual "join works for nothing (1)" Nothing $ join' $ Just (Nothing @Int)
  , TestCase $ assertEqual "join works for nothing (2)" (Nothing @Int) $ join' Nothing
  ]

testDubEven :: Test
testDubEven = TestList
  [ TestCase $ assertEqual "dubEven empty list" [] $ dubEven []
  , TestCase $ assertEqual "dubEven nonempty list" [0, 0, 1, 2, 2, 3, 4, 4, 5] $ dubEven [0, 1, 2, 3, 4, 5]
  , TestCase $ assertEqual "отрицательные и повторные" [-2, -2, -2, -2, -1] $ dubEven [-2, -2, -1]
  ]
