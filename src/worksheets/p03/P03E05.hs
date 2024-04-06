module P03E05 where

import P03E04 ( Exp (..), eval )

type Stack = [Exp]

getFirst :: String -> (String, String)
getFirst s = getFirstAux [] (deleteSpaces s)
  where 
    getFirstAux r ""      = (reverse r, [])
    getFirstAux r (' ':s) = (reverse r, deleteSpaces s)
    getFirstAux r (c  :s) = getFirstAux (c : r) s
    deleteSpaces ""      = ""
    deleteSpaces (' ':s) = deleteSpaces s
    deleteSpaces s       = s

updateTop :: (Exp -> Exp -> Exp) -> Stack -> Stack
updateTop op (e0 : e1 : stack) = op e1 e0 : stack
updateTop op _                 = error "Expresion mal formada"

makeExp :: Stack -> String -> Exp
makeExp stack "" = if length stack == 1 
                     then head stack
                     else error "Expresion mal formada"
makeExp stack s  = case f of
                     "+" -> makeExp (updateTop Add  stack) r
                     "-" -> makeExp (updateTop Sub  stack) r
                     "*" -> makeExp (updateTop Prod stack) r
                     "/" -> makeExp (updateTop Div  stack) r
                     x   -> makeExp (Lit (read x) : stack) r
                   where (f, r) = getFirst s

parseRPN :: String -> Exp
parseRPN = makeExp []

evalRPN :: String -> Int
evalRPN s = eval (parseRPN s)
