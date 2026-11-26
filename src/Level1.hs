{- HLINT ignore join' "Use join" -}

-- | Обязательный уровень. Монады Maybe, Point и списка.
module Level1 where

import Control.Monad (guard, join)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import MetaUtils (todo, todoImpl)


-- 1.1. Начальник начальника.
-- По идентификатору авторизованного сотрудника найдите в базе начальника его начальника.
-- Авторизацию проверяйте только для исходного сотрудника. Если он не авторизован,
-- не хватает записи в базе или у кого-то в цепочке не указан нужный начальник,
-- верните Nothing. Запишите последовательность поисков с помощью монады Maybe.

newtype UserId = UserId { getUserId :: Int64 }
  deriving (Show, Eq, Ord)

newtype UserName = UserName { getUserName :: String }
  deriving (Show, Eq)

data User = User
  { userId :: UserId
  , userName :: UserName
  , userBoss :: Maybe UserId
  }
  deriving (Show, Eq)

type Auth = Set UserId

hasAuth :: UserId -> Auth -> Bool
hasAuth = Set.member

type UserDb = Map UserId User

bossBoss :: Auth -> UserDb -> UserId -> Maybe User
bossBoss = todo "bossBoss"


-- 1.2. Монада Point.
-- Дана структура данных Point, которая задаёт координату на плоскости.
-- Сделайте её монадой, в которой вычисление ведётся покомпонентно.
-- Замените каждую строку todoImpl полным экземпляром соответствующего класса.
-- Например:
--    do a <- Point 3 4
--       b <- Point 10 12
--       pure (a + b)
--    ~> Point 13 16
--
-- Подсказка. Представьте Point как список ровно из двух элементов.
-- Продолжение в (>>=) строит по новой точке для каждого из них.
-- В результате снова только два места. Какие компоненты новых точек
-- нужно оставить, чтобы каждое вычисление шло в своей координате?

data Point a = Point a a
  deriving (Show, Eq)

todoImpl ''Functor ''Point

todoImpl ''Applicative ''Point

todoImpl ''Monad ''Point


-- 1.3. Выразите (>>=) через join и (<$>), а join — через (>>=).
-- Если известно, что двойное буррито — это буррито и что из начинки буррито X
-- можно сделать буррито Y, то из буррито X можно сделать буррито Y.
-- Действительно: заменим начинку буррито X целиком на буррито Y; получим двойное буррито Y.
-- Но двойное буррито — это буррито.

bind :: Monad m => m a -> (a -> m b) -> m b
bind = todo "bind"

-- Обратно: возьмём двойное буррито Z. Его начинка — уже обычное буррито Z.
-- Как доготовить такую начинку до обычного буррито Z?

join' :: Monad m => m (m a) -> m a
join' = todo "join'"


-- 1.4. Используя монаду списка, напишите функцию, которая каждое чётное число
-- раздваивает. Сохраните порядок элементов.
-- dubEven [3, 0, -2, 5] == [3, 0, 0, -2, -2, 5]

dubEven :: [Int] -> [Int]
dubEven = todo "dubEven"
