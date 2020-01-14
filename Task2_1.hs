module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Todo(todo)
import Prelude hiding(lookup, (!!))

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = Node Integer (TreeMap v) v (TreeMap v)
               | Nil
               deriving(Show)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = Nil

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains Nil _ = False
contains (Node k l _ r) x | x == k = True
						  | x < k = contains l x 
						  | x > k = contains r x

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup x (Node k l v r) | x == k = v
	                    | x < k = lookup x l 
	                    | x > k = lookup x r

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k', v') Nil = Node k' Nil v' Nil
insert (k', v') (Node k l v r) | k' == k = Node k l v' r
	                           | k' > k = Node k l v (insert (k', v') r)
	                           | k' < k = Node k (insert (k', v') l) v r

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove _ Nil = Nil
remove k' (Node k l v r) | k' == k = removeX (Node k l v r)
                         | k' < k = Node k (remove k' l) v r
                         | k' > k = Node k l v (remove k' r)
  where
    removeX (Node _ Nil _ Nil) = Nil
    removeX (Node _ Nil _ r) = r
    removeX (Node _ l _ Nil) = l
    removeX (Node k l _ r)   = Node k l (right l) r
      where
        right (Node _ _ v Nil) = v
        right (Node _ _ _ r  ) = right r

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i t | contains t (i-1) = ((i-1), lookup (i-1) t)
              | otherwise        = nearestLE (i-1) t

-- Построение дерева из списка пар
treeFromList :: (Ord v) => [(Integer, v)] -> TreeMap v
treeFromList [] = Nil
treeFromList (h@(k, v):t) = tree2 (Node k Nil v Nil) t
  where
    tree2 tr []    = tr
    tree2 tr (h:t) = tree2 (insert h tr) t

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree Nil            = []
listFromTree (Node k l v r) = ((k, v) : listFromTree l) ++ listFromTree r

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = (qsort $ listFromTree t) !! i

(!!) :: [a] -> Integer -> a
(!!) (x:_)  0 = x
(!!) (_:xs) i = xs !! (i-1)

qsort :: [(Integer, v)] -> [(Integer, v)]
qsort [] = []
qsort (h@(k, v):t) = (qsort [(k', v') | (k', v') <- t, k' < k]) ++ [h] ++ (qsort [(k', v') | (k', v') <- t, k' >= k])
