module P04E03 where

data BST a = Hoja | Nodo (BST a) a (BST a) deriving Show

member :: Ord a => a -> BST a -> Bool
member _ Hoja           = False
member a t@(Nodo l b r) = maxLE a t b == a
    where maxLE _ Hoja candidato                     = candidato
          maxLE a (Nodo l b r) candidato | b <= a    = maxLE a r b
                                         | otherwise = maxLE a l candidato
