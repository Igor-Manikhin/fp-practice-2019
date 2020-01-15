module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

instance (Show a) => Show (ReverseList a) where
  show xs = "r" ++ (show $ rlistToList xs)

instance (Eq a) => Eq (ReverseList a) where
  (==) x y = (rlistToList x) == (rlistToList y)

instance (Ord a) => Ord (ReverseList a) where
  compare x y = compare (rlistToList x) (rlistToList y)

instance Semigroup (ReverseList a) where
  xs <> ys = listToRList ((rlistToList xs) <> (rlistToList ys))

instance Monoid (ReverseList a) where
  mempty = RNil

instance Functor ReverseList where
  fmap f x = listToRList (f <$> (rlistToList x))


rlistToList :: ReverseList a -> [a]
rlistToList = reverse . rlistToList'
  where
    rlistToList' RNil         = []
    rlistToList' (RCons xs x) = x : rlistToList' xs

listToRList :: [a] -> ReverseList a
listToRList = listToRList' . reverse
  where
    listToRList' [] = RNil
    listToRList' (x:xs) = RCons (listToRList' xs) x

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor