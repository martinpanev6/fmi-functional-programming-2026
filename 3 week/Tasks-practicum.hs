import Prelude hiding (product, head, tail, init, last, append, reverse, take, drop, zip, replicate, repeat)

--Task 1
product :: [Int] -> Int 
product lst 
 |null lst = 1
 |otherwise = head lst * product (tail lst)

product2 :: [Int] -> Int
product2 [] = 1
product2 (x : xs) = x * product2 xs

--Task 2
range :: Int -> Int -> [Int]
range a b
    |a > b = []
    |otherwise = a : range (a + 1) b

--Task 3
head :: [a] -> a
head [] = error "empty list"
head (x : _) = x 

tail :: [a] -> [a]
tail [] = error "empty list"
tail (_ : xs) = xs 

init :: [a] -> [a] 
init [] = error "empty list" 
init [_] = []
init (x : xs) = x : init xs

last :: [a] -> a
last [] = error "empty list" 
last [x] = x 
last (x : xs) = last xs 

butLast :: [a] -> a 
butLast [] = error "empty list" 
butLast [_] = error "the elements must be at least two" 
butLast (x : _ : []) = x 
butLast (x : xs) = butLast xs

--Task 4
append :: [a] -> [a] -> [a]
append [] l2 = l2
append (x : xs) l2 = x : append xs l2 

reverse :: [a] -> [a]
reverse [x] = [x]
reverse (x : xs) = reverse xs ++[x]

take :: Int -> [a] -> [a]
take n [] = []
take 0 l = []
take n (x : xs) = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop n [] = []
drop 0 l = l
drop n (x : xs) = drop (n - 1) xs

dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n lst 
    |n <= 0 = error "n must be positive"
    | otherwise = take (n - 1) lst ++ dropEvery n (drop n lst)

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x l2 = x : l2
insertAt _ x [] = [x] 
insertAt n x (y : ys) = y : insertAt (n - 1) x ys

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

--Task 5
splitAt :: Int -> [a] -> ([a], [a])
splitAt n lst = undefined
    where
        go :: Int -> [a] -> [a] -> ([a], [a])
        go 0 acc rest = (acc, rest)
        go n acc (x : xs) = go (n - 1) (acc ++ [x]) xs

splitAt2 :: Int -> [a]-> ([a], [a])
splitAt2 0 l2 = ([], l2)
splitAt2 _ [] = ([], [])
splitAt2 n (x : xs) = 
    let (left, right) = splitAt2 (n - 1) xs
    in (x : left, right) 

--Task 6
palindrome :: Eq a => [a] -> Bool
palindrome lst = lst == reverse lst

--Task 7
mergeSorted :: [Int] -> [Int] -> [Int] 
mergeSorted [] l2 = l2
mergeSorted l1 [] = l1
mergeSorted (x : xs) (y : ys)
    |x < y = x : mergeSorted xs (y : ys)
    |otherwise = y : mergeSorted (x : xs) ys

--Task 8
pack :: Eq a => [a] -> [[a]]
pack lst = go [] lst 
    where 
        go :: Eq a => [a] -> [a] -> [[a]]
        go acc [] = [acc] 
        go acc [x] = [x : acc]
        go acc (x : xs@(y : _ys))
            |x == y = go (x : acc) xs
            |otherwise = (x : acc) : go [] xs

--Task 9
rle :: Eq a => [a] -> [(Int, a)]
rle lst = go (pack lst)
    where 
        go :: Eq a => [[a]] -> [(Int, a)]
        go [] = []
        go (x : xs) = (length x, head x) : go xs


--Task 10 
decodeRle :: [(Int, a)] -> [a]
decodeRle [] = []
decodeRle ((n, x) : xs) = replicate n x ++ decodeRle xs

replicate :: Int -> a -> [a]
replicate n x
  | n <= 0 = []
  | otherwise = x : replicate (n - 1) x

--Task 11 
cartesian :: [a] -> [b] -> [(a, b)]
cartesian [] _ = [] 
cartesian (x : xs) ys = pairWithEvery x ys ++ cartesian xs ys 
    where 
        pairWithEvery :: a -> [b] -> [(a, b)]
        pairWithEvery _ [] = []
        pairWithEvery x (y : ys) = (x, y) : pairWithEvery x ys

--Task 12
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) = let rest = subsets xs in addToEach x rest ++ rest
    where
        addToEach :: a -> [[a]] -> [[a]]
        addToEach _ [] = []
        addToEach x (xs : xss) = (x : xs) : addToEach x xss

--Task 13
combinations :: Int -> [a] -> [[a]]
combinations k lst = filterByLength k (subsets lst)
 where
  filterByLength :: Int -> [[a]] -> [[a]]
  filterByLength _ [] = []
  filterByLength k (xs : xss)
    | length xs == k = xs : filterByLength k xss
    | otherwise = filterByLength k xss

--Task 14
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && xs `isPrefixOf` ys

--Task 15
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations lst = concat [map (x :) res | x <- lst, let res = permutations $ filter (/= x) lst]

--Task 16
repeat :: a -> [a]
repeat x = x : repeat x

--Task 17
nats :: [Int]
nats = go 0
    where 
        go i = i : go (i + 1)

--Task 18
powerOf2 :: [Int] 
powerOf2 = go 0
    where go i = (2 ^ i) : go (i + 1)

--Task 19
primeNumbers :: [Int]
primeNumbers = go 2
    where 
        go :: Int -> [Int]
        go i 
            |isPrime i = i : go (i + 1)
            |otherwise = go (i + 1)

isPrime :: Int -> Bool
isPrime n = hasNoDivisersIn n [2..n-1]
    where 
        hasNoDivisersIn :: Int -> [Int] -> Bool
        hasNoDivisersIn _ [] = True
        hasNoDivisersIn k (x : xs) = mod k x /= 0 && hasNoDivisersIn k xs 

--Task 20
fibonacciNums :: [Int]
fibonacciNums = go 0 1
    where 
        go :: Int -> Int -> [Int]
        go prev cur = prev : go cur (prev + cur)