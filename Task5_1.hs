module Task5_1 where

import Todo(todo)

-- Структура двусвязного списка из лекции про ленивость

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let u = DCons left h (list2dlist' u t)
    in u


-- Реализуйте функции индексирования, вставки и удаления элементов
index :: DList a -> Int -> a
index DNil _ = error "Index out of bounds"
index (DCons _ x _) 0 = x
index (DCons left _ right) n 
    | n < 0 = index left (abs n - 1)
    | n > 0 = index right (n-1)

flipList :: DList a -> DList a
flipList DNil = DNil
flipList (DCons l x r) = DCons r x l

insertAt :: DList a -> Int -> a -> DList a
insertAt DNil 0 x = DCons DNil x DNil
insertAt DNil _ _ = error "Index out of bounds"
insertAt (DCons left x right) n a
    | n == 1 = let t = DCons left x (insertAt' t a right) in t
    | n == 0 = let t = DCons DNil a (insertAt' t x right) in t
    | otherwise =
        case right of
            DNil -> error "Index out of bounds"
            _    -> DCons left x (insertAt right (n - 1) a)


insertAt' :: DList a -> a -> DList a -> DList a
insertAt' l x DNil = DCons l x DNil
insertAt' l x (DCons l' x' r) = let t = DCons l x (insertAt' t x' r) in t

removeAt :: DList a -> Int -> DList a
removeAt DNil _ = error "Empty list"
removeAt (DCons _ _ DNil) n
    | n == 0 = DNil
    | otherwise = error "Index out of bounds"
removeAt (DCons l _ (DCons _ x r)) 0 = DCons l x r
removeAt (DCons l x r) n = DCons l x (removeAt r (n - 1))

