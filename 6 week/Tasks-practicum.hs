import Prelude hiding (map, filter, all, any, zipWith, takeWhile, dropWhile, span, (.), foldr, foldl, foldr1, foldl1, lookup, findIndex, sortBy)

--Task 1
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs 

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = [] 
filter p (x : xs) = 
    if p x 
        then x : filter p xs
        else filter p xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True 
all p (x : xs) = p x && all p xs 
    
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False 
any p (x : xs) = p x && any p xs

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith _ _ [] = []
zipWith _ [] [] = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys 

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = map (\(x, y) -> f x y) (zip xs ys) 

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs)
    | p x = x : takeWhile p xs
    | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs)
    | p x = dropWhile p xs
    | otherwise = x:xs

span :: (a -> Bool) -> [a] -> ([a], [a])
span p lst = (takeWhile p lst, dropWhile p lst)

--Task 2
fixpoints :: (Int -> Int) -> Int -> Int -> [Int]
fixpoints f a b 
    |a > b = []
    |f a == a = a : fixpoints f (a + 1) b
    |otherwise = fixpoints f (a + 1) b

fixpoints' :: (Int -> Int) -> Int -> Int -> [Int]
fixpoints' f a b = filter (\x -> f x == x) [a..b]

fixpointsCnt :: (Int -> Int) -> Int -> Int -> Int
fixpointsCnt f a b = length (fixpoints' f a b) 

--Task 3
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose g f = \x -> g (f x) 

--Task 4
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

--Task 5
lookup :: (a -> Bool) -> [(a, b)] -> b
lookup _ [] = error "No matching element"
lookup p ((x, y):xs)
    | p x = y
    | otherwise = lookup p xs

findIndex :: (a -> Bool) -> [a] -> (Int, a)
findIndex _ [] = error "No matching element"
findIndex p (x:xs)
    | p x       = (0, x)
    | otherwise = (i + 1, y)
  where
    (i, y) = findIndex p xs

--Task 6
sort :: [Int] -> [Int] 
sort [] = []
sort (pivot : xs) = sort left ++[pivot] ++ sort right 
    where
        left = filter (< pivot) xs 
        right = filter (>= pivot) xs

comparator :: (a -> a -> Ordering) -> [a] -> [a]
comparator _ [] = []

sortBy :: (a -> a -> Bool) -> [a] -> [a]
sortBy _ [] = []
sortBy p (pivot : xs) = sortBy p left ++ [pivot] ++ sortBy p right
    where 
        left = filter (\x -> p x pivot) xs
        right = filter (\x -> not (p x pivot)) xs 

minimumBy :: (a -> a -> Bool) -> [a] -> a 
minimumBy _ [] = error "empty list"
minimumBy p lst = head $ sortBy p lst 

minimumBy' :: (a -> a -> Bool) -> [a] -> a 
minimumBy' _ [] = error "empty list"
minimumBy' cmp (x : xs) = foldr (\y ys -> if cmp y ys then y else ys) x xs 

--Task 7
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = op (f x) (f y)

--Task 8
area :: (Int, (Double, Double), (Double, Double)) -> (Int, Double)
area (n, (x1, y1), (x2, y2)) = (n, abs (x2 - x1) * abs (y1 - y2))

areas :: [(Int, (Double, Double), (Double, Double))] -> [(Int, Double)]
areas lst = map area lst 

biggerAreas :: [(Int, (Double, Double), (Double, Double))] -> Double -> [(Int, Double)]
biggerAreas lst n = filter (\(a, b) -> b >= n) (areas lst)

quadr :: [(Int, (Double, Double), (Double, Double))] -> Int -> [(Int, (Double, Double), (Double, Double))]
quadr lst n 
    |n == 1 = filter (\(_, (x1, y1), (x2, y2)) -> x1 >= 0 && x2 >= 0 && y1 >= 0 && y2 >= 0) lst
    |n == 2 = filter (\(_, (x1, y1), (x2, y2)) -> x1 <= 0 && x2 <= 0 && y1 >= 0 && y2 >= 0) lst
    |n == 3 = filter (\(_, (x1, y1), (x2, y2)) -> x1 <= 0 && x2 <= 0 && y1 <= 0 && y2 <= 0) lst
    |n == 4 = filter (\(_, (x1, y1), (x2, y2)) -> x1 >= 0 && x2 >= 0 && y1 <= 0 && y2 <= 0) lst

degenRect :: [(Int, (Double, Double), (Double, Double))] -> Bool
degenRect lst = length filteredLst >= 1
    where 
        filteredLst = filter (\(_, (x1, y1), (x2, y2)) -> (x1 == x2) || (y1 == y2)) lst 

rectNum :: [(Int, (Double, Double), (Double, Double))] -> Int -> (Int, (Double, Double), (Double, Double))
rectNum lst n  
    |length filteredLst == 1 = head filteredLst
    |otherwise = error "No matching element"
    where
        filteredLst = filter (\(x, _, _) -> x == n) lst

--Task 9
toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = toDigits (div n 10) ++ [mod n 10]

fromDigits :: [Int] -> Int
fromDigits lst = foldl (\acc x -> acc * 10 + x) 0 lst 

sortDigits :: Int -> Int
sortDigits n = fromDigits $ sortedEvens ++ sortedOdds
    where
        digits = toDigits n 
        evens = filter even digits
        odds = filter odd digits
        sortedEvens = sortBy (<) evens 
        sortedOdds = sortBy (>) odds