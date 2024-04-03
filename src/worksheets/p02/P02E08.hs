module P02E08 where

f :: c -> d
f = undefined

g :: a -> b -> c
g = undefined

h :: a -> b -> d
h x y = f (g x y)

-- lo cual es equivalente a:
-- h = f . g
-- h x y = (f . g) x y

(.) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.) = undefined
