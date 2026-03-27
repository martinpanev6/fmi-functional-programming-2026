import Prelude hiding (map, filter, foldr, foldl, foldr1, foldl1)

--Task 28.1
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs 

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = [] 
filter p (x : xs) = 
    if p x 
        then x : filter p xs
        else filter p xs

foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr _ z [] = z 
foldr f z (x : xs) = f x (foldr f z xs) 

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ z [] = z 
foldl f z (x : xs) = foldl f (f z x) xs 

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [] = error "empty list"
foldr1 _ [x] = x
foldr1 f (x : xs) = f x (foldr1 f xs) 

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 _ [] = error "empty list"
foldl1 _ [x] = x
foldl1 f (x:y:xs) = foldl1 f (f x y : xs)

--Task 28.2
sumList :: [(Int, Int, Int)] -> [Int]
sumList [] = []
sumList lst = map (\(a, b, c) -> (a + b + c)) lst

sumComp :: [(Int, Int, Int)] -> (Int, Int, Int)
sumComp lst = (firstSum, secondSum, thirdSum)
    where
        firstSum = sum (map (\(a, b, c) -> a) lst)
        secondSum = sum (map (\(a, b, c) -> b) lst)
        thirdSum = sum (map (\(a, b, c) -> c) lst)

tripleCount :: [(Int, Int, Int)] -> Int
tripleCount lst = length filteredLst
    where
        filteredLst = filter (\(a, b, c) -> a + b > c) lst

func :: [(Int, Int, Int)] -> Bool
func lst = length filteredLst >= 1
    where 
        filteredLst = filter (\(a, b, c) -> (a == b) && (b == c)) lst 

--Task 28.3
corIdx :: [Int] -> [Int]
corIdx lst = map fst (filter (\(x, i) -> x == i) (zip lst [1..]))

--Task 28.4
sumEl :: [Int] -> [Int]
sumEl (x : xs) = map (\(a, b) -> a + b) (zip (x : xs) xs) 

--Таск 28.9
isAscending :: [Int] -> Bool
isAscending [] = True
isAscending [x] = True
isAscending l = fst (foldr helper (True, last l) (init l))
    where
        helper x (ok, next) = (ok && x <= next, x)