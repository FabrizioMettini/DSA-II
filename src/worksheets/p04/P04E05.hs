module P04E05 where

data Color = R | B deriving Show
data RBT a = E | T Color (RBT a) a (RBT a) deriving Show

-- 1

data T234 a = Hoja
             | N2 (T234 a) a (T234 a)
             | N3 (T234 a) a (T234 a) a (T234 a)
             | N4 (T234 a) a (T234 a) a (T234 a) a (T234 a)
             deriving Show

-- 2

rbtTo234 :: RBT a -> T234 a
rbtTo234 E = Hoja
rbtTo234 (T B (T R l1 a1 r1) a (T R l2 a2 r2)) = N4 (rbtTo234 l1) a1 (rbtTo234 r1) a (rbtTo234 l2) a2 (rbtTo234 r2)
rbtTo234 (T B (T R l1 a1 r1) a t2)             = N3 (rbtTo234 l1) a1 (rbtTo234 r1) a (rbtTo234 t2)
rbtTo234 (T B t1 a (T R l2 a2 r2))             = N3 (rbtTo234 t1) a (rbtTo234 l2) a2 (rbtTo234 r2)
rbtTo234 (T B l a r)                           = N2 (rbtTo234 l) a (rbtTo234 r)
