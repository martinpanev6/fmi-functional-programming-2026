--Task 24.1
evenNums :: Int -> [Int]
evenNums 0 = []
evenNums n = evenNums (n - 1) ++ [2 * n]

arProgression :: Int -> Int -> Int -> [Int]
arProgression n a d 
    |n <= 0 = []
    |otherwise = arProgression (n - 1) a d ++ [a + (n - 1) * d]

fact :: Int -> [Int]
fact n 
    |n < 0 = error "The number must be non-negative"
    |otherwise = helper n 1 
    where 
        helper :: Int -> Int -> [Int]
        helper 0 _ = []
        helper n acc = helper (n - 1) (acc * n) ++ [acc * n]

allEven :: [Int] 
allEven = [0, 2 ..]

allProgression :: Int -> Int -> [Int] 
allProgression a d = [a, a + d ..]

allFact :: [Int]
allFact = allFacthelper 0 1
    where 
        allFacthelper :: Int -> Int -> [Int]
        allFacthelper 0 _ = []
        allFacthelper n acc = allFacthelper (n - 1) (acc * n) ++ [acc * n]

--Task 24.2
reverseList :: Int -> [Int]
reverseList n 
    |n < 10 = [n]
    |otherwise = mod n 10 : reverseList (div n 10)

--Task 24.3
reverseL :: Int -> [Int]
reverseL n = helper n []
    where 
        helper :: Int -> [Int] -> [Int]
        helper 0 result = result
        helper n result 
            |contains (mod n 10) result = helper (div n 10) result 
            |otherwise = helper (div n 10) (mod n 10 : result)
            where 
                contains :: Int -> [Int] -> Bool 
                contains _ [] = False 
                contains x (y : ys)
                    |x == y = True 
                    |otherwise = contains x ys 

--Task 24.4
isPerfect :: Int -> Bool
isPerfect n = helper 1 0 == n
    where 
        helper :: Int -> Int -> Int 
        helper d accSum 
            |d == n = accSum
            |mod n d == 0 = helper (d + 1) (accSum + d)
            |otherwise = helper (d + 1) accSum

perfectNums :: Int -> [Int]
perfectNums n 
    |n < 1 = []
    |isPerfect n = perfectNums (n - 1) ++ [n]
    |otherwise = perfectNums (n - 1)

--Task 24.5
histogram :: String -> [(Char, Int)]
histogram [] = []
histogram (x : xs) 
    |contains x xs = histogram xs
    |otherwise = (x, count x (x : xs)) : histogram xs
    where
        contains :: Char -> String -> Bool 
        contains _ [] = False
        contains x (y : ys)
            |x == y = True
            |otherwise = contains x ys

        count :: Char -> String -> Int 
        count _ [] = 0
        count x (y : ys)
            |x == y = 1 + count x ys
            |otherwise = count x ys

--Task 25.7
pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs [_] = []
pairs (x : y : xs) 
    |x < y = (x, y) : pairs (y : xs)
    |otherwise = pairs (y : xs)

--Task 25.8
groupsOf :: [a] -> Int -> [[a]]
groupsOf [] _ = []
gropsOf l x = takeN l x : groupsOf (dropN l x) x
    where 
        takeN :: [a] -> Int -> [a]
        takeN [] _ = []
        takeN l 0 = []
        takeN (y : ys) n = y : takeN ys (n - 1)

        dropN :: [a] -> Int -> [a]
        dropN [] _ = []
        dropN l 0 = l
        dropN (_ : ys) n = dropN ys (n - 1)

--Task 25.9
flatten :: [[a]] -> [a]
flatten [] = []
flatten (x : xs) = x ++ flatten xs 

--Task 25.10
decode :: [(Int, a)] -> [a]
decode [] = []
decode ((n, x) : xs) 
    |n <= 0 = decode xs
    |otherwise = x : decode ((n - 1, x) : xs)