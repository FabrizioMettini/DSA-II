{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use id" #-}                     -- b
{-# HLINT ignore "Redundant if" #-}               -- c
{-# HLINT ignore "Avoid lambda using `infix`" #-} -- d

module P02E06 where

-- a

smallest :: Ord a => a -> a -> a -> a
smallest = \x y z -> if x <= y && x <= z 
                       then x
                       else if y <= x && y <= z 
                              then y
                              else z

-- b

second :: a -> a
second = \x -> x

-- c

andThen :: Bool -> Bool -> Bool
andThen = \p q -> if p then q else False

-- d

twice :: (a -> a) -> a -> a
twice = \f x -> f (f x)

-- e

flip :: (b -> a -> c) -> a -> b -> c
flip = \f x y -> f y x

-- f

inc :: Num a => a -> a
inc = \x -> x + 1
