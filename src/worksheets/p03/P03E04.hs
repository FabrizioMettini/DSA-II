module P03E04 where

data Exp = Lit Int 
           | Add Exp Exp
           | Sub Exp Exp
           | Prod Exp Exp
           | Div Exp Exp
           deriving Show

eval :: Exp -> Int
eval (Lit n)      = n
eval (Add e1 e2)  = eval e1   +   eval e2
eval (Sub e1 e2)  = eval e1   -   eval e2
eval (Prod e1 e2) = eval e1   *   eval e2
eval (Div e1 e2)  = eval e1 `div` eval e2
