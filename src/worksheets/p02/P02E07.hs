module P02E07 where

-- a

iff :: Bool -> Bool -> Bool
iff True y = not y 
iff False y = y

-- b

alpha :: a -> a
alpha x = x
