module Par ((|||)) where

import GHC.Conc(par)

infix 1 |||

(|||) :: a -> b -> (a, b)
a ||| b = a `par` b `par` (a, b)

