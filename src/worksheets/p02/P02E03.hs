{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant id" #-}

module P02E03 where

import Data.Char (chr, ord)

zero :: Int
zero = 0

-- a

f_a1 :: (Int -> Int) -> Int
f_a1 f = f zero + zero

f_a2 :: (Int -> Int) -> Int
f_a2 f = f zero * zero

-- b

f_b1 :: Int -> (Int -> Int)
f_b1 y = (+y)

f_b2 :: Int -> (Int -> Int)
f_b2 z = (*z)

-- c

f_c1 :: (Int -> Int) -> (Int -> Int)
f_c1 f = (+1) . f

f_c2 :: (Int -> Int) -> (Int -> Int)
f_c2 f = (^2) . f

-- d

f_d1 :: Int -> Bool
f_d1 x = x >= 0

f_d2 :: Int -> Bool
f_d2 x = x `mod` 5 == 0

-- e

f_e1 :: Bool -> (Bool -> Bool)
f_e1 p = (&& p)

f_e2 :: Bool -> (Bool -> Bool)
f_e2 p = (/= p)

-- f

f_f1 :: (Int, Char) -> Bool
f_f1 (x, c) = ord c == x

f_f2 :: (Int, Char) -> Bool
f_f2 (x, c) = chr x == c

-- g

f_g1 :: (Int, Int) -> Int
f_g1 (x, y) = x

f_g2 :: (Int, Int) -> Int
f_g2 (x, y) = x + y

-- h

f_h1 :: Int -> (Int, Int)
f_h1 x = (x, 0)

f_h2 :: Int -> (Int, Int)
f_h2 x = (x, -x)

-- i

f_i1 :: a -> Bool
f_i1 _ = False

f_i2 :: a -> Bool
f_i2 _ = True

-- j

f_j1 :: a -> a
f_j1 x = id x

f_j2 :: a -> a
f_j2 x = (id . id) x
