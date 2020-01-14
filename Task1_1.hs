module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) a b = BinaryTerm (Variable "+") (BinaryTerm a b)
infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) a b = BinaryTerm (Variable "-") (BinaryTerm a b)
infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) a b = BinaryTerm (Variable "*") (BinaryTerm a b)
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
                                            (Variable var) ->
                                                if   varName == var
                                                then replacement
                                                else expression
                                            (BinaryTerm l r) ->
                                                BinaryTerm (replaceVar varName replacement l) (replaceVar varName replacement r)
                                            _ -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (IntConstant a) = IntConstant a
evaluate expr@(BinaryTerm op (BinaryTerm a b)) = case op of
                                                 (Variable "+") -> IntConstant (intValue (evaluate a) + intValue (evaluate b))
                                                 (Variable "-") -> IntConstant (intValue (evaluate a) - intValue (evaluate b))
                                                 (Variable "*") -> IntConstant (intValue (evaluate a) * intValue (evaluate b))
                                                 _ -> expr