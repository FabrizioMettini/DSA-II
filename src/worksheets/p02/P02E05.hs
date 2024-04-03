module P02E05 where

-- a

f :: (a -> a) -> a -> a
f x = x

-- b

greater :: Ord a => (a, a) -> Bool
greater (x, y) = x > y

-- c

f' :: Num a => (a, a) -> a
f' (x, y) = x
