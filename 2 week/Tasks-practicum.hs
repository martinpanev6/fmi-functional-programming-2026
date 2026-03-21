import Prelude hiding(succ, pred, add, mull, lcm, gcd)

--Task 1
succ :: Int -> Int
succ n = n + 1

pred :: Int -> Int
pred 0 = 0
pred n  = predhelper n 0
    where 
        predhelper :: Int -> Int -> Int 
        predhelper n k 
            |n < 0 = error "The number must be non- negative"
            |n == succ k = k 
            |otherwise = predhelper n (succ k)

add :: Int -> Int -> Int
add 0 n = n 
add m n = add (pred m) (succ n)

mull :: Int -> Int -> Int
mull 0 n = 0
mull n m = add n (mull n (pred m))

ack :: Int -> Int -> Int 
ack 0 n = succ n 
ack m 0 = ack (pred m) 1
ack m n = ack (pred m) (ack m (pred n))

--Task 2
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibonacci :: Int -> Int 
fibonacci n 
    |n < 0 = error "The number must be non-negative" 
    |n <= 1 = n 
    |otherwise = fibonacci (n - 1) + fibonacci (n - 2)

--Task 3
quickPow :: Int -> Int -> Int 
quickPow _ 0 = 1
quickPow x n 
    |mod n 2 == 0 = (quickPow x (div n 2)) ^ 2 
    |otherwise = x * quickPow x (n - 1)

--Task 4 
isPalindrome :: Int -> Bool 
isPalindrome n 
    |n < 0 = error "The number must be non-negative" 
    |otherwise = n == reversenum n 
    where 
        reversenum :: Int -> Int 
        reversenum n = helper n 0 
            where 
                helper :: Int -> Int -> Int 
                helper 0 acc = acc 
                helper n acc = helper (div n 10) (acc * 10 + mod n 10)

--Task 5
lcm :: Int -> Int -> Int 
lcm a b = div (a * b) (gcd a b)
    where 
        gcd :: Int -> Int -> Int 
        gcd 0 b = b
        gcd a 0 = a
        gcd a b = gcd b (mod a b)

--Task 6
kaperkar :: Int -> Bool
kaperkar x = helper (x ^ 2) 0 1
    where 
        helper :: Int -> Int -> Int -> Bool 
        helper lhr lhs i
            |lhs == 0 = False
            |lhr + lhs == x = True 
            |otherwise = helper (lhs `div` 10) (lhs `mod` 10 * i + lhr) (i * 10)

--Task 7
sumDigits :: Int -> Int 
sumDigits n 
    |n < 10 = n 
    |otherwise = mod n 10 + sumDigits (div n 10)

prime :: Int -> Bool 
prime x = x > 1 && noDiv 2
    where
        noDiv :: Int -> Bool 
        noDiv d 
            |d == x = True 
            |mod x d == 0 = False 
            |otherwise = noDiv (d + 1)

func :: Int -> Int 
func n = sumDigits (go 2)
    where 
        go :: Int -> Int 
        go x 
            |mod n x == 0 && prime x = x 
            |otherwise = go (x + 1)