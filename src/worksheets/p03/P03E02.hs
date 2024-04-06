module P03E02 where

type Caracteres = [Char]

type Cursor = Int

type Linea = (Caracteres, Cursor)

vacia :: Linea
vacia = ([], 0)

moverIzq :: Linea -> Linea
moverIzq (s, 0) = (s, 0)
moverIzq (s, p) = (s, p-1)

moverDer :: Linea -> Linea
moverDer (s, p) = if p == length s 
                    then (s, p)
                    else (s, p+1)

moverIni :: Linea -> Linea
moverIni (s, _) = (s, 0)

moverFin :: Linea -> Linea
moverFin (s, _) = (s, length s)

insertar :: Char -> Linea -> Linea
insertar c (s, p) = (take p s ++ [c] ++ drop p s, p+1)

borrar :: Linea -> Linea
borrar (s, p) = (take (p-1) s ++ drop p s, p-1)
