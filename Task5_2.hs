module Task5_2 where

import Todo(todo)
-- Зиппер из лекции 

data Zipper a = Zipper [a] [a]

-- Реализуйте экземпляры классов Show и Eq для этого типа

instance Show a => Show (Zipper a) where
    show (Zipper l r) = show l ++ "\n<..>\n" ++ show r
    
instance Eq a => Eq (Zipper a) where
    (Zipper [] x) ==  (Zipper [] y) = x == y
    (Zipper x []) ==  (Zipper y []) = x == y
    z1@(Zipper l1 r1) == z2@(Zipper l2 r2) = l1 == l2 && r1 == r2 || goLeft z1 == goLeft z2
 
fromList :: [a] -> Zipper a
fromList lst = Zipper [] lst

goRight :: Zipper a -> Zipper a
goRight z@(Zipper _ []) = z
goRight (Zipper l (rh:rt)) = Zipper (rh:l) rt

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper [] _) = z
goLeft (Zipper (lh:lt) r) = Zipper lt (lh:r)

putRight :: a -> Zipper a -> Zipper a
putRight x (Zipper l r) = Zipper l (x:r)

putLeft :: a -> Zipper a -> Zipper a
putLeft x (Zipper l r) = Zipper (x:l) r

removeRight :: Zipper a -> Zipper a
removeRight (Zipper l (_:rt)) = Zipper l rt

removeLeft :: Zipper a -> Zipper a
removeLeft (Zipper (_:lt) r) = Zipper lt r


-- my additional functions
-- проверка, что все элементы зиппера находятся с одной стороны
leftSided :: Zipper a -> Bool
leftSided (Zipper _ []) = True
leftSided _ = False

rightSided :: Zipper a -> Bool
rightSided (Zipper [] _) = True
rightSided _ = False

--перекидываем элементы зиппера в одну сторону
allToRight :: Zipper a -> Zipper a
allToRight z = if rightSided z then z else allToRight (goLeft z)
  
allToLeft :: Zipper a -> Zipper a
allToLeft z = if leftSided z then z else allToLeft (goRight z)

--обрезаем зиппер по одной стороне
cutRight :: Zipper a -> Zipper a
cutRight (Zipper l _) = Zipper l []

cutLeft :: Zipper a -> Zipper a
cutLeft (Zipper _ r) = Zipper [] r
-- Используя приведённые выше функции, реализуйте функцию конкатенации
-- вставки подсписка в середину и выделения подсписка


-- не будем по одному перетаскивать значения из одного зиппера в другой
--
-- перекинув содержимое одного зиппера влево, другого - вправо,
-- сольём их в один
concat :: Zipper a -> Zipper a -> Zipper a
concat left right = merge (allToLeft left) (allToRight right)
    where merge (Zipper l []) (Zipper [] r) = Zipper l r

(<+>) = Task5_2.concat

insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt index what into
    | not $ rightSided into = insertManyAt index (allToRight what) (allToRight into)
    | index == 0 = cutRight into <+> what <+> cutLeft into
    | otherwise = insertManyAt (index - 1) what (goRight into)


-- выделяемый кусок перемещаем влево, затем отрезаем правую часть
-- предполагается, что нулевой индекс имеет голова правой части
subZipper :: Int -> Int -> Zipper a -> Zipper a
subZipper from to input
    | to > 0 = subZipper (from - 1) (to - 1) (goRight input)
    | otherwise = cutRight input