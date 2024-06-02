module P06E01 where

import Prelude hiding (nth, cons, tabulate, map, take, drop)
import Par ((|||))

-- | We implement Sequence ADT with Binary Trees
data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

-- | Return the size of a sequence
size                 :: BTree a -> Int
size Empty           = 0  -- empty sequence have size 0
size (Node sz _ _ _) = sz -- non empty sequence have size sz

-- | Find the nth element of a sequence
-- WARNING: This function is partial
nth                                :: BTree a -> Int -> a
nth (Node _ l x r) n | n < size l  = nth l n                -- nth element is in l subsequence
                     | n == size l = x                      -- nth element is the value of the current node
                     | otherwise   = nth r (n - size l - 1) -- nth element is in r subsequence

-- | Insert an element at the start of the sequence
cons                   :: a -> BTree a -> BTree a
cons x Empty           = Node 1 Empty x Empty         -- insert element in an empty sequence
cons x (Node sz l y r) = Node (sz + 1) (cons x l) y r -- insert element in l subsequence

-- | Given a function f and an integer n, return the sequence of size n
-- where each element of the sequence is the result of applying f to the index of the element
tabulate     :: (Int -> a) -> Int -> BTree a
tabulate f n = tab f n 0
    where tab f n offset | n <= 0    = Empty                                                       -- empty sequence
                         | otherwise = let nr     = (n-1) `div` 2                                  -- size of r subsequence 
                                           nl     = n - nr - 1                                     -- size of l subsequence
                                           (l, r) = tab f nl offset ||| tab f nr (nl + offset + 1) -- build l and r subsequence in parallel
                                       in  Node n l (f (nl + offset)) r                            -- build current sequence 

-- | Given a function f and a sequence s, return the result of applying f over each element of s
map                   :: (a -> b) -> BTree a -> BTree b
map _ Empty           = Empty                              -- map over an empty sequence                     
map f (Node sz l x r) = let (l', r') = map f l ||| map f r -- l and r are independent, so we can parallelize
                        in Node sz l' (f x) r'

-- | Given an integer n and a sequence s, return the first n elements of s
take                                    :: Int -> BTree a -> BTree a
take _ Empty                            = Empty                                -- there is no elements to take
take n _                  | n <= 0      = Empty                                -- don't take any elements
take n bt@(Node sz l x r) | n >= sz     = bt                                   -- take all elements
                          | n <= size l = take n l                             -- take at most all elements from l
                          | otherwise   = Node n l x (take (n - size l - 1) r) -- take at least all elements from l + x

-- | Given an integer n and a sequence s, return the sequence s without the first n elements
drop                                 :: Int -> BTree a -> BTree a
drop _ Empty                         = Empty                        -- there is no elements to drop
drop n s               | n <= 0      = s                            -- don't drop any elements  
drop n (Node sz l x r) | n >= sz     = Empty                        -- drop all elements
                       | n <= size l = Node (sz - n) (drop n l) x r -- drop at most all elements from l
                       | otherwise   = drop (n - size l - 1) r      -- drop at least all elements from l + x
