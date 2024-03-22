module Span where

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p []                   = []
takeWhile' p (x : xs) | p x       = x : (takeWhile' p xs)
                      | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p []                   = []
dropWhile' p (x : xs) | p x       = dropWhile' p xs
                      | otherwise = x : xs

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' p xs = (takeWhile' p xs, dropWhile' p xs)
