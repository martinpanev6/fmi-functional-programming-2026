import Prelude hiding (product, head, tail, init, last, append, reverse, take, drop, zip)

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
pack [] = []
pack (x : xs) = (x : ys) : pack zs 
    where 
        ys = takeSome x xs
        zs = dropSome x xs

takeSome :: Eq a => a -> [a] -> [a]
takeSome _ [] = []
takeSome x (y : ys)
    |x == y = y : takeSome x ys 
    |otherwise = []

dropSome :: Eq a => a -> [a] -> [a]
dropSome _ [] = []
dropSome x (y : ys) 
    |x == y = dropSome x ys 
    |otherwise = y : ys

--Task 9
rle :: Eq a => [a] -> [(a, Int)]
rle [] = []
rle (x : xs) 
    |contains x xs = rle xs
    |otherwise = (x, count x (x : xs)) : rle xs
    where
        contains :: Eq a => a -> [a] -> Bool 
        contains _ [] = False
        contains x (y : ys)
            |x == y = True
            |otherwise = contains x ys

        count :: Eq a => a -> [a] -> Int 
        count _ [] = 0
        count x (y : ys)
            |x == y = 1 + count x ys
            |otherwise = count x ys

--Task 10
decode :: [(Int, a)] -> [a]
decode [] = []
decode ((n, x) : xs) 
    |n <= 0 = decode xs
    |otherwise = x : decode ((n - 1, x) : xs)

--Task 11
decart :: [a] -> [b] -> [(a, b)]
decart [] _ = []
decart (x : xs) ys = pairWith x ys ++ decart xs ys
    where 
        pairWith :: a -> [b] -> [(a, b)]
        pairWith _ [] = []
        pairWith x (y : ys) = (x, y) : pairWith x ys
