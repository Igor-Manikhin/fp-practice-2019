module Task4_1 where

{-
  Задание 4.1
  Реализация монады над функцией.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
-- Тогда пусть при оборачивании значения в эту монаду fun представляет 
-- константную функцию, на любом значении возвращающую само это значение
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f fm = let g = fun fm in FunMonad $ f . g
  -- g :: String -> a
  -- f :: a -> b
  -- f . g :: String -> b
  -- FunMonad (f . g) :: FunMonad b
  {-
    Проверка законов функторов:
    1. fmap id x == x
      fmap id (FunMonad f) = FunMonad $ id . f = FunMonad f --- верно
    2. fmap (f . g) x = fmap f (fmap g x)
      fmap (f . g) (FunMonad h) = FunMonad (f . g . h) 
                                = FunMonad (f . (g . h)) = fmap f (FunMonad (g . h))
                                = fmap f (fmap g (FunMonad h)) --- верно  
  -}

instance Applicative FunMonad where
  pure x = FunMonad (\_ -> x)
  mf <*> ma = FunMonad (\s -> f s (a s))
    where f = fun mf
          a = fun ma
  {-
    Проверка аппликативных законов:
    1. pure id <*> v = v
      pure id = FunMonad (\s -> a -> a) -- FunMonad g
      pure id <*> (FunMonad f) = FunMonad (\s -> g s (f s)) = FunMonad (\s -> f s) = FunMonad f
      --- верно
    2. pure f <*> pure x = pure (f x)
      pure f = FunMonad (\s -> \t -> f t) = FunMonad g
      pure x = FunMonad (\s -> x) = FunMonad y
      FunMonad g <*> FunMonad y = FunMonad (\s -> g s (y s)) 
                                = FunMonad (\s -> g s x) 
                                = FunMonad (\s -> f x) 
                                = pure (f x)
                                --- верно
    3. u <*> pure y = pure ($ y) <*> u
      FunMonad f <*> FunMonad (\s -> y) = FunMonad (\s -> f s y) --- (*)
      pure ($ y) = FunMonad (\s -> ($ y)) = FunMonad x
      pure ($ y) <*> FunMonad f = FunMonad x <*> FunMonad f
                                = FunMonad (\s -> x s (f s))
                                = FunMonad (\s -> ($ y) (f s))
                                = FunMonad (\s -> f s y) --- (**)
      (*) = (**)
      --- верно

    
  -}
  
instance Monad FunMonad where
  return = pure
  ma >>= mf = mf' <*> ma
    where mf' = FunMonad $ flip $ fun . mf
  {-
    Проверка монадных законов:
    1. return x >>= mf = mf x
      return x = pure x
      pure x >>= mf = mf' <*> pure x
      mf' = FunMonad $ flip (fun . mf)
      mf :: a -> FunMonad b
      fun . mf :: a -> String -> b
      flip $ fun . mf :: String -> a -> b --- просто раскрыли функцию mf
      FunMonad $ ... :: FunMonad (a -> b) --- привели функцию mf к виду f (a -> b)
      FunMonad f <*> pure x = pure ($ x) <*> FunMonad f
                            = pure ($ x) <*> FunMonad (flip $ fun . mf)
                            = FunMonad (\s -> ($ x)) <*> FunMonad (flip $ fun . mf)
                            = FunMonad g <*> ...
                            = FunMonad (\s -> g s (flip $ fun . mf $ s))
                            = FunMonad (\s -> ($ x) (flip $ fun . mf $ s))
                            = FunMonad (fun . mf $ x)
                            = mf x
                            --- верно
    2. ma >>= return = ma
      ma >>= return = ma >>= pure
      fun . pure = \b -> \s -> b = const
      flip const = const'
      FunMonad g = ma, g = \s -> a
      FunMonad const' <*> FunMonad g = FunMonad (\s -> const' s (g s)) 
                                     = FunMonad (\s -> g s) 
                                     = FunMonad g
                                     = ma
      --- верно
    3. (mv >>= f) >>= g  =  mv >>= (\x -> (f x >>= g))
  -}