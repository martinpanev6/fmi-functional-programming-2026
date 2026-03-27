import Text.XHtml (base)
--Task 29.1
dlin :: Double -> Double -> (Double -> Double)
dlin a1 a2 = \x -> if x <= 0 then a1 * x else a2 * x 

--Task 29.2
rectfn :: Double -> Double -> Double -> (Double)
rectfn low high step = \x -> if x >= low && x <= high then step else 0

--Task 29.3
createFn :: Eq a => [(a, b)] -> (a -> b)
createFn lst = \x -> helper x lst 
    where 
        helper x ((a, b) : xs)
            |x == a = b 
            |otherwise = helper x xs 

--Task 29.4
caseof :: [(a -> Bool, a -> b)] -> (a -> b)
caseof lst = \x -> helper x lst
    where
        helper x [(p, f)] = f x
        helper x ((p, f) : xs)
            | p x = f x
            | otherwise = helper x xs

--Task 29.5
makePred :: Eq a => [a] -> (a -> Bool) 
makePred lst = \x -> elem x lst

--Task 29.6
derive :: (Double -> Double) -> (Double -> Double)
derive f = \x -> (f(x + 0.001) - (f x)) / 0.001 
