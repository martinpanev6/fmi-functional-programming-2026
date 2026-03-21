import Prelude hiding(Rational)
--Task 1
volume :: Double -> Double
volume r = 4 * pi * r ^ 3 / 3

--Task 2
hasRealRoots :: Int -> Int -> Int -> Bool
hasRealRoots a b c = b ^ 2 - 4 * a * c >= 0

--Task 3
dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2- y1) ^ 2)

--Task 4
maxNum :: Int -> Int -> Int -> Int
maxNum a b c 
    |a >= b && a >= c = a 
    |b >= a && b >= c = b 
    |otherwise = c

--Task 5
isZero :: Int -> Bool
isZero x = x == 0

--Task 6
(&&&) :: Bool -> Bool -> Bool
(&&&) True True = True
(&&&) _ _ = False

(|||) :: Bool -> Bool -> Bool
(|||) False False = False
(|||) _ _ = True

--Task 7
triangleArea :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double 
triangleArea  p1 p2 p3 = sqrt(semip*(semip - a)*(semip - b) * (semip - c))
    where
        a = dist p1 p2
        b = dist p2 p3
        c = dist p1 p3
        semip = (a + b + c) / 2 

--Task 8
hms :: Int -> (Int, Int, Int)
hms s = (h, m, sec) 
    where 
        h = div s 3600
        m = div (mod s 3600) 60
        sec = mod s 60

--Task 9
type Rational = (Int, Int)

mkRat :: Int -> Int -> Rational
mkRat _ 0 = error "second number cannot be zero"
mkRat a b = (a, b)

simplify :: Rational -> Rational 
simplify (a, b) = (div a d, div b d)
    where d = gcd a b 

add :: Rational -> Rational -> Rational
add (a1, b1) (a2, b2) = simplify(a1 * b2 + a2 * b1, b1 * b2)

--Task 10
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

--Task 11
digitSum :: Int -> Int
digitSum x
    |abs x < 10 = abs x
    |otherwise = abs (mod x 10) + digitSum(div x 10)