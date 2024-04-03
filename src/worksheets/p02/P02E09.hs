module P02E09 where

import Prelude hiding (zip3)

-- recursiva

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3 xs ys zs
zip3 _      _      _      = []

-- con zip

zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' xs ys zs = f xs (zip ys zs)
  where f (a:as) ((b, c):cs) = (a, b, c) : f as cs
        f _      _           = []
