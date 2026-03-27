--Task 1
maxOrd :: [Int] -> Int
maxOrd [] = 0
maxOrd [x] = 1
maxOrd (x : y : xs) 
    |x <= y = 1 + maxOrd (y : xs) 
    |otherwise = 1

--Task 2
countSq :: [Int] -> [Int] -> Int 
countSq [] _ = 0
countSq (x : xs) l2 
    |elem (x ^ 2) l2 = 1 + countSq xs l2 
    |otherwise = countSq xs l2

--Task 3
least :: [(String, Int)] -> [String]
least lst = map fst (filter (\(_, x) -> x == (leastCount lst)) lst)

leastCount :: [(String, Int)] -> Int
leastCount lst = minimum (map snd lst)

--Task 4
prod :: Int -> Int -> Int -> Int
prod a b k = foldl (*) 1 (filter (\x -> mod x k == 0) [a..b])

--Task 5
reverseSuf :: Int -> Int
reverseSuf n = fromDigits(reverse (descendingSuf (toDigits n)))

isDescending :: [Int] -> Bool
isDescending [] = True
isDescending [x] = True
isDescending (x:y:xs) = x > y && isDescending (y:xs)

descendingSuf :: [Int] -> [Int]
descendingSuf xs 
    |isDescending xs = xs 
    |otherwise = descendingSuf(tail xs)

fromDigits :: [Int] -> Int
fromDigits lst = foldl (\acc x -> acc * 10 + x) 0 lst 

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = toDigits (div n 10) ++ [mod n 10]

--Task 6
maxConsec :: [Int] -> Int 
maxConsec [] = 0
maxConsec [x] = 1
maxConsec (x : y : xs)
    |x == y - 1 = 1 + maxConsec (y : xs)
    |otherwise = 1

--Task 7
singletons :: [Int] -> [Int] -> [Int]
singletons [] _ = []
singletons (x : xs) ys 
    |length (filter(\y -> y == x) ys) == 1 = x : singletons xs ys 
    |otherwise = singletons xs ys

--Task 8
trickiest :: [(String, Int)] -> [String]
trickiest lst  = map fst (filter (\x -> snd x == (highPoints lst)) lst)

highPoints :: [(String, Int)] -> Int 
highPoints lst = maximum (map snd lst) 

--Task 9
prod' :: Int -> Int -> Int -> Int 
prod' a b k = product (filter (\x -> mod k x == 0) [a..b])

--Task 10
reverseSuf' :: Int -> Int 
reverseSuf' n = fromDigits(reverse((ascendingSuf(toDigits n))))

isAscending :: [Int] -> Bool 
isAscending [] = True 
isAscending [x] = True 
isAscending (x : y : xs) = x <= y && isAscending(y : xs)

ascendingSuf :: [Int] -> [Int]
ascendingSuf xs 
    |isAscending xs = xs
    |otherwise = ascendingSuf(tail xs)