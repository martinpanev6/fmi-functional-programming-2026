--Task 25.1
say :: Int -> String
say n 
    |n == 0 = "zero"
    |n == 1 = "one"
    |n == 2 = "two"
    |n == 3 = "three"
    |n == 4 = "four" 
    |n == 5 = "five"
    |n == 6 = "six"
    |n == 7 = "seven"
    |n == 8 = "eight" 
    |n == 9 = "nine"
    |otherwise = "number is too small or too big"

--Task 25.2
biggestPrefix :: Eq a => [a] -> [a] -> Int
biggestPrefix [] _ = 0
biggestPrefix _ [] = 0
biggestPrefix (x : xs) (y : ys)
    |x == y = 1 + biggestPrefix xs ys 
    |otherwise = 0

--Task 25.3
countEvenOddl :: [Int] -> (Int, Int)
countEvenOddl [] = (0, 0)
countEvenOddl (x : xs)
    |mod x 2 == 0 = (1 + evenCount, oddCount)
    |otherwise = (evenCount, 1 + oddCount)
    where (evenCount, oddCount) = countEvenOddl xs

--Task 25.4
pivotl :: [Int] -> Int -> ([Int], [Int])
pivotl [] _ = ([], [])
pivotl (x : xs) a
    |x < a = (x : less, greater)
    |otherwise = (less, x : greater)
    where (less, greater) = pivotl xs a 
