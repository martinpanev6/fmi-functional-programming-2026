import Prelude hiding(pow, length, count, )

--Task 22.3 
func1 :: Int -> Bool
func1 x = (mod x 4 == 0) || (mod x 7 == 0)

func2 :: Int -> Int -> Int -> Bool
func2 a b c = b ^ 2 - 4 * a * c < 0

func3 :: (Int, Int) -> Bool
func3 (x, y) = x < 5 && y < 6

func4 :: (Int, Int) -> (Int, Int) -> Int -> Bool
func4 (a, b) (c, d) f = a > c + f && b > d + f

func5 :: (Int, Int) -> Bool
func5 (x, y) = (x <=0 && x >= -5) && (y <= 0 && y >= -5)

func6 :: Int -> Int -> Int -> Int -> Bool
func6 x a b c = x == max a (max b c)

func7 :: Int -> Int -> Int -> Bool
func7 a b c = a < 0 && b < 0 && c < 0

func8 :: Int -> Bool
func8 x = div x 100 == 7 ||  mod (div x 10) 10 == 7 || mod x 10 == 7

--Task 22.4
digitCount :: Int -> Int
digitCount x 
    |x < 10 = 1
    |otherwise = 1 + digitCount(div x 10)

--Task 22.5
digitSum :: Int -> Int
digitSum x
    |x < 10 = x
    |otherwise = mod x 10 + digitSum(div x 10)

--Task 22.6
pow :: Int -> Int -> Int
pow _ 0 = 1
pow x n = x * pow x (n - 1)

--Task 22.10
exactPow :: Int -> Int -> Bool
exactPow n k
    |n == 1 = True
    |n < k = False
    |mod n k /= 0 = False
    |otherwise = exactPow (div n k) k

--Task 22.11
isPerfect :: Int -> Bool
isPerfect n = helper 1 0 == n
    where 
        helper :: Int -> Int -> Int 
        helper d accSum 
            |d == n = accSum
            |mod n d == 0 = helper (d + 1) (accSum + d)
            |otherwise = helper (d + 1) accSum

--Task 23.1
length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs 

--Task 23.2
count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x (l : ls)
    |x == l = 1 + count x ls
    |otherwise = count x ls

--Task 23.3
index' :: Eq a => a -> [a] -> Int
index' _ [] = error "empty list" 
index' x (l : ls)
    |x == l = 0
    |otherwise = 1 + index' x ls

--Task 23.4
subList :: Eq a => [a] -> [a] -> Bool
subList [] _ = True
subList _ [] = False
subList (l1 : l1s) (l2 : l2s)
    |l1 == l2 = subList l1s l2s
    |otherwise = subList (l1 : l1s) l2s

--Task 23.5
common :: Eq a => [a] -> [a] -> Int
common [] _ = 0
common (l1 : l1s) l2s
    |inSecond l2s = 1 + common l1s l2s
    |otherwise = common l1s l2s
    where 
        inSecond [] = False
        inSecond (l2 : l2s)
            |l1 == l2 = True
            |otherwise = inSecond l2s 

--Task 23.6
duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (l : ls) = contains l ls || duplicates ls
    where
        contains :: Eq a => a -> [a] -> Bool
        contains _ [] = False
        contains a (l : ls)
            |a == l = True
            |otherwise = contains a ls