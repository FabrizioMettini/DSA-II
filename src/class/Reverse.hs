module Reverse where

reverse' :: [a] -> [a]
reverse' xs = rev xs []
  where
    rev []     ys = ys
    rev (x:xs) ys = rev xs (x : ys)