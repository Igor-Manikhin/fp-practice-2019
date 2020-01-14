module Task1_2 where
import Prelude hiding (gcd, pow, sin, cos)
{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}

import Todo(todo)

fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact n = n * (fact (n - 1))

factd = fromIntegral . fact

isqrt n = helper n
      where
        helper x | x * x > n = helper (x - 1)
                 | otherwise = x

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = taylor 10
  where
    x' = x - (fromIntegral $ floor (x / (2*pi))) * 2*pi
    taylor  n = foldr1 (+) (take n (map formula [1..]))
    formula n = (-1)**(n' - 1) * x'**(2 * n' - 1) / (factd (2 * n - 1))
      where n' = fromIntegral n
    
-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = taylor 10
  where
    x' = x - (fromIntegral $ floor (x / (2*pi))) * 2*pi
    taylor  n = foldr1 (+) (take n (map formula [0..]))
    formula n = (-1)**(n') * x'**(2 * n') / (factd (2 * n))
      where n' = fromIntegral n

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y | y == 0    = x
        | otherwise = gcd y (x `mod` y)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to | sa * sa == from = True
                               | (sb * sb == to) && (sa == sb - 1) = False
                               | sa < sb = True
                               | otherwise = False
   where
     sa = isqrt from
     sb = isqrt to

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year | (not $ inRange day 31) || (not $ inRange month 12) || (year < 1) = False
                             | isDayCorrect = True
                             | otherwise    = False
   where
     isLeapYear = year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)
     daymonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
     isDayCorrect | isLeapYear && month == 2 = inRange day 29
                  | otherwise                = inRange day (daymonth !! (fromIntegral $ month - 1))
     inRange x a = 1 <= x && x <= a

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x y = foldr1 (*) (replicate y x)
   where
     replicate n x | n == 1    = [x]
                   | otherwise = x : replicate (n-1) x

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x | x > 1     = null [ i | i <- [2..isqrt x], x `mod` i == 0]
          | otherwise = False

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = abs (sum1 + add1 - sum2 - add2) / 2
   where
     n = length points
     sum1 = foldr1 (+) (map (\i -> (fst $ points !! i) * (snd $ points !! (i + 1))) [0..n-2])
     sum2 = foldr1 (+) (map (\i -> (snd $ points !! i) * (fst $ points !! (i + 1))) [0..n-2])
     add1 = (fst $ points !! (n - 1)) * (snd $ head points)
     add2 = (fst $ head points) * (snd $ points !! (n - 1))

-- -- треугольник задан своими координатами.
-- -- функция должна вернуть
-- --  0, если он тупоугольный
-- --  1, если он остроугольный
-- --  2, если он прямоугольный
-- --  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a@(x1, y1) b@(x2, y2) c@(x3, y3) | any (isNaN) angles = -1
                                              | all (< (pi/2)) angles = 1
                                              | any (> (pi/2)) angles = 0
                                              | otherwise = 2
   where
     len (x1, y1) (x2, y2) = sqrt ((x1 - x2)**2 + (y1 - y2)**2)
     a' = len a b
     b' = len b c
     c' = len c a
     p = (a' + b' + c') / 2
     s = sqrt (p * (p - a') * (p - b') * (p - c'))
     angle1 = acos ((b'**2 + c'**2 - a'**2)/(2*b'*c'))
     angle3 = acos ((a'**2 + c'**2 - b'**2)/(2*a'*c'))
     angle2 = acos ((b'**2 + a'**2 - c'**2)/(2*b'*a'))
     angles = [angle1, angle2, angle3]