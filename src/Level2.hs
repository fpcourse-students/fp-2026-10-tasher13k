-- | Подготовка к сессии. Программы в do-нотации.
module Level2 where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.List (genericLength)
import Level1 (Point (..))
import MetaUtils (todo)
import MyIO


-- 2.1. Рассахаривание do.
-- Перепишите sugared через (>>=) и (>>), без do-нотации и вызова sugared.
-- Сохраните порядок чтений и отметки mark. Что происходит, если первая
-- строка не подходит под образец [x, y]? Выразите эту ветвь с помощью
-- MonadFail. Текст сообщения об ошибке можно выбрать самостоятельно.
-- Например, sugared (Just [1]) (Just ()) == Nothing.

sugared :: MonadFail m => m [Int] -> m () -> m Int
sugared readRow mark = do
  [x, y] <- readRow
  mark
  let offset = x + y
  rest <- readRow
  pure (offset + sum rest)

desugared :: MonadFail m => m [Int] -> m () -> m Int
desugared = todo "desugared"


-- 2.2. Разные операции над компонентами Point.
-- Реализуйте с помощью do-нотации функцию sumAndMult, которая складывает
-- первые компоненты точек и умножает вторые. Для выбора операции используйте
-- isFirstComponent: она определяет, по первой ли компоненте ведётся вычисление.
-- sumAndMult (Point 2 3) (Point 5 7) == Point 7 21

isFirstComponent :: Point Bool
isFirstComponent = todo "isFirstComponent"

sumAndMult :: Num a => Point a -> Point a -> Point a
sumAndMult = todo "sumAndMult"


-- 2.3. Пифагоровы тройки.
-- Сгенерируйте все тройки положительных чисел (c, a, b), где c > a > b
-- и c² = a² + b², в возрастающем лексикографическом порядке.
-- Используйте монаду списка. Первые пять элементов:
-- [(5, 4, 3), (10, 8, 6), (13, 12, 5), (15, 12, 9), (17, 15, 8)]

edges1 :: [(Int, Int, Int)]
edges1 = todo "edges1"

-- Запишите тот же перебор через list comprehension.

edges2 :: [(Int, Int, Int)]
edges2 = todo "edges2"


-- 2.4. Центр масс.
-- Пользователь вводит в учебную консоль массу и три целые координаты каждого
-- небесного тела. Ввод корректен, тел хотя бы одно, все массы положительны.
-- В massCenter вычислите для каждой оси сумму произведений массы на координату
-- и разделите её на общую массу с помощью div.
-- В centerMain прочитайте все тела до конца ввода (isEof) и выведите три
-- координаты центра масс в порядке x, y, z. Используйте операции MyIO.
-- Запустите приложение из app/Main.hs командой make run.

-- Трёхмерная точка из ДЗ 9. Обход идёт в порядке x, y, z.
data Point3 a = Point3 a a a
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Point3 where
  pure x = Point3 x x x

  Point3 f g h <*> Point3 x y z = Point3 (f x) (g y) (h z)

data Object = Object
  { objMass :: Int
  , objCoord :: Point3 Int
  }

massCenter :: [Object] -> Point3 Int
massCenter = todo "massCenter"

centerMain :: MyIO ()
centerMain = todo "centerMain"


-- 2.5. Три броска кубика.
-- Честный n-гранный кубик (n >= 2, грани от 1 до n) бросают трижды независимо.
-- Известно, что сумма чётная. Найдите условную вероятность того, что сумма равна 14.
-- Перечислите в allEvenOutcomes все возможные чётные суммы,
-- по одному элементу на каждую упорядоченную тройку бросков с такой суммой.
-- Для вычисления вероятности дана функция probability.

probability
  :: (outcome -> Bool) -- Какие элементарные исходы желаемые
  -> [outcome]         -- Все равновероятные исходы
  -> Double            -- Вероятность желаемого исхода
probability p outcomes = genericLength (filter p outcomes) / genericLength outcomes

allEvenOutcomes :: Int -> [Int]
allEvenOutcomes = todo "allEvenOutcomes"

probSolution :: Int -> Double
probSolution = todo "probSolution"
