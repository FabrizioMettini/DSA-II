module P02E13 where 
    
-- a

divisors :: Int -> [Int]
divisors x = [d | d <- [1..x], x `mod` d == 0] 

-- b

matches :: Int -> [Int] -> [Int]
matches x xs = [y | y <- xs, y == x]

-- c

cuadrupla :: Int -> [(Int, Int, Int, Int)]
cuadrupla n = [(a, b, c, d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], a^2 + b^2 == c^2 + d^2]

-- d

unique :: [Int] -> [Int]
unique xs = [x | (x,i) <- zip xs [0..], x `notElem` take i xs]
