module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

--import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f a []     = a
foldl f a (x:xs) = foldl f (a `f` x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f a []     = a
foldr f a (x:xs) = f x (foldr f a xs)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
                Just (a, b') -> a : unfoldr f b'
                Nothing      -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x : xs) []

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product xs = foldr (*) 1 xs

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes xs = foldr check [] xs
  where
    check (Just y)  ys = y : ys
    check (Nothing) ys = ys

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal = reverse . snd . foldl (\(i, xs) x -> (i+1, (x !! i) : xs)) (0, [])

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p = foldr (\x xs -> if p x then xs else x : xs) []

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem x = foldl (\false x' -> if x == x' then True else false) False

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr (\x -> let x' = x + step in if x > to then Nothing else Just (x, x')) from

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append xs ys = foldr (\x y -> x:y) ys xs

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups xs n = unfoldr (\x -> if length x == 0 then Nothing else Just (take' n x, drop' n x)) xs

take' :: Integer -> [a] -> [a]
take' n _ | n <= 0 =  []
take' n []         =  []
take' n (x:xs)     =  x : take' (n-1) xs

drop' :: Integer -> [a] -> [a]
drop' _ []                     = []
drop' n xs@(_:xs') | n > 0     = drop' (n-1) xs'
                   | otherwise = xs
