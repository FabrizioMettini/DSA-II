module P02E01 where

-- a

test :: (Eq a, Num a) => (a -> a) -> a -> Bool
test f x = f x == x + 2

-- b

esMenor :: Ord a => a -> a -> Bool
esMenor y z = y < z

-- c

eq :: Eq a => a -> a -> Bool
eq a b = a == b

-- d

showVal :: Show a => a -> String
showVal x = "Valor:" ++ show x
