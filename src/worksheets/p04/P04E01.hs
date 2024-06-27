module P04E01 where

data Tree a = Hoja | Nodo (Tree a) a (Tree a) deriving Show

completo :: a -> Int -> Tree a
completo x 0 = Nodo Hoja x Hoja
completo x d = Nodo s x s
  where s = completo x (d-1)

balanceado :: a -> Int -> Tree a
balanceado _ 0 = Hoja
balanceado x n = Nodo l x r
  where nl = (n - 1) `div` 2
        nr = n - nl - 1
        l  = balanceado x nl
        r  = if nr == nl then l else balanceado x nr
