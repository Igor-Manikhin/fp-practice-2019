module Task4_2 where

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
	fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)
	{-
		Проверка функторных законов:
		1. fmap id x
		fmap id (FourOf a b c d) = FourOf (id a) (id b) (id c) (id d)
		                         = FourOf a b c d
		2. fmap (f . g) x = fmap f (fmap g x)
		fmap (f . g) (FourOf a b c d) = FourOf (f . g $ a) ...
		                              = FourOf (f (g a)) ...
		                              = fmap f (fmap g (FourOf a b c d)) 
	-}

instance Applicative FourOf where
	pure x = FourOf x x x x
	(FourOf f1 f2 f3 f4) <*> (FourOf v1 v2 v3 v4) = FourOf (f1 v1) (f2 v2) (f3 v3) (f4 v4)

instance Monad FourOf where
	return = pure
	(FourOf x1 x2 x3 x4) >>= mf = FourOf y1 y2 y3 y4
		where FourOf y1 _ _ _ = mf x1
		      FourOf _ y2 _ _ = mf x2
		      FourOf _ _ y3 _ = mf x3
		      FourOf _ _ _ y4 = mf x4


{-
Проверка монадных законов:
    1. return x >>= mf = mf x
    очевидно
    2. ma >>= return = ma
    очевидно
    3. (mv >>= f) >>= g  =  mv >>= (\x -> (f x >>= g))
    mv >>= f = (FourOf x1 x2 x3 x4) >>= mf = FourOf y1 y2 y3 y4
    FourOf y1 y2 y3 y4 >>= g = FourOf z1 z2 z3 z4
    в силу "лёгкой" раскрываемости содерижимого
-}