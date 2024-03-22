module Lab01 where

import Data.List (sort)
import Data.Char (ord, isLetter)

-- 1) Corregir los siguientes programas de modo que sean aceptados por GHCi.

-- a) 

not' :: Bool -> Bool
not' b = case b of
  True  -> False
  False -> True

-- b)

in' :: [a] -> [a]
in' [x]    = []
in' (x:xs) = x : in' xs
in' []     = error "empty list"

-- c)

length' :: Num a => [b] -> a
length' []    = 0
length' (_:l) = 1 + length' l

-- d)

list123 :: [Int]
list123 = 1 : 2 : 3 : []

-- e)

(++!) :: [a] -> [a] -> [a]
(++!) []       ys = ys
(++!) (x : xs) ys = x : xs ++! ys

-- f)

addToTail :: Num a => a -> [a] -> [a]
addToTail x xs = map (+x) (tail xs)

-- g)

listmin :: Ord c => [c] -> c
listmin xs = (head . sort) xs

-- h) (*)

smap :: (t -> a) -> [t] -> [a]
smap f []       = []
smap f [x]      = [f x]
smap f (x : xs) = f x : smap f xs

-- 2. Definir las siguientes funciones y determinar su tipo:

-- a) five, que dado cualquier valor, devuelve 5

five :: Num b => a -> b
five x = 5

-- b) apply, que toma una función y un valor, y devuelve el resultado de 
-- aplicar la función al valor dado

apply :: (a -> b) -> a -> b
apply f = f

-- c) ident, la función identidad

ident :: a -> a 
ident x = x

-- d) first, que toma un par ordenado, y devuelve su primera componente

first :: (a, b) -> a
first (x, _) = x

-- e) derive, que aproxima la derivada de una función dada en un punto dado

derive :: Integral a => (a -> a) -> a -> a -> a
derive f a h = (f (a + h) - f a) `div` h

-- f) sign, la función signo

sign :: (Num a, Ord a) => a -> a
sign x | x < 0     = -1
       | x == 0    = 0
       | otherwise = 1

-- g) vabs, la función valor absoluto (usando sign y sin usarla)

vabs :: (Num a, Ord a) => a -> a
vabs x = if x >= 0 then x else -x

vabs' :: (Num a, Ord a) => a -> a
vabs' x = x * sign x

-- h) pot, que toma un entero y un número, y devuelve el resultado de
-- elevar el segundo a la potencia dada por el primero

pot :: (Integral b, Num a) => b -> a -> a
pot n x = x ^ n

-- i) xor, el operador de disyunción exclusiva

xor :: Bool -> Bool -> Bool
xor p q = (p && not q) || (not p && q)

-- j) max3, que toma tres números enteros y devuelve el máximo entre ellos

max3 :: Int -> Int -> Int -> Int
max3 x y z = max x (max y z)

-- k) swap, que toma un par y devuelve el par con sus componentes invertidas

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- 3) Definir una función que determine si un año es bisiesto o no, de
-- acuerdo a la siguiente definición:

-- año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
-- cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
-- de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

-- ¿Cuál es el tipo de la función definida?

isLeapYear :: Int -> Bool
isLeapYear x | x `mod` 400 == 0                   = True
             | x `mod` 4 == 0 && x `mod` 100 /= 0 = True
             | otherwise                          = False

-- 4)

-- Defina un operador infijo *$ que implemente la multiplicación de un
-- vector por un escalar. Representaremos a los vectores mediante listas
-- de Haskell. Así, dada una lista ns y un número n, el valor ns *$ n
-- debe ser igual a la lista ns con todos sus elementos multiplicados por
-- n. Por ejemplo,

-- [ 2, 3 ] *$ 5 == [ 10 , 15 ].

-- El operador *$ debe definirse de manera que la siguiente
-- expresión sea válida:

-- v = [1, 2, 3] *$ 2 *$ 4

(*$) :: Num a => [a] -> a -> [a]
(*$) xs c = map (*c) xs

-- 5) Definir las siguientes funciones usando listas por comprensión:

-- a) 'divisors', que dado un entero positivo 'x' devuelve la
-- lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)

divisors' :: Int -> [Int]
divisors' x = [d | d <- [1..x], x `mod` d == 0] 

-- b) 'matches', que dados un entero 'x' y una lista de enteros descarta
-- de la lista los elementos distintos a 'x'

matches :: Int -> [Int] -> [Int]
matches x xs = [y | y <- xs, y == x]

-- c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
-- '(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
-- donde 0 <= a, b, c, d <= 'n'

cuadrupla :: Int -> [(Int, Int, Int, Int)]
cuadrupla n = [(a, b, c, d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], a^2 + b^2 == c^2 + d^2]

-- d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista de
-- 'xs' sin elementos repetidos

unique :: [Int] -> [Int]
unique xs = [x | (x,i) <- zip xs [0..], x `notElem` take i xs]

-- 6) El producto escalar de dos listas de enteros de igual longitud
-- es la suma de los productos de los elementos sucesivos (misma
-- posición) de ambas listas.  Definir una función 'scalarProduct' que
-- devuelva el producto escalar de dos listas.

-- Sugerencia: Usar las funciones 'zip' y 'sum'.

scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- 7) Definir mediante recursión explícita
-- las siguientes funciones y escribir su tipo más general:

-- a) 'suma', que suma todos los elementos de una lista de números

suma :: Num a => [a] -> a

-- recursión explícita
suma []       = 0
suma (x : xs) = x + suma xs

-- funciones de alto orden
-- suma = foldr (+) 0 

-- b) 'alguno', que devuelve True si algún elemento de una
-- lista de valores booleanos es True, y False en caso
-- contrario

alguno :: [Bool] -> Bool

-- recursión explícita
alguno []       = False
alguno (x : xs) = x || alguno xs

-- funciones de alto orden
-- alguno = foldr (||) False

-- c) 'todos', que devuelve True si todos los elementos de
-- una lista de valores booleanos son True, y False en caso
-- contrario

todos :: [Bool] -> Bool

-- recursión explícita
todos []       = True
todos (x : xs) = x && todos xs

-- funciones de alto orden
-- todos = foldr (&&) True

-- d) 'codes', que dada una lista de caracteres, devuelve la
-- lista de sus ordinales

codes :: [Char] -> [Int]

-- recursión explícita
codes [] = []
codes (x : xs) = ord x : codes xs

-- listas por comprensión
-- codes xs = [ord x | x <- xs]

-- funciones de alto orden
-- codes = map ord

-- e) 'restos', que calcula la lista de los restos de la
-- división de los elementos de una lista de números dada por otro
-- número dado

restos :: Integral a => [a] -> a -> [a]

-- recursión explícita
restos []       y = []
restos (x : xs) y = x `mod` y : restos xs y

-- listas por comprensión
-- restos xs y = [x `mod` y | x <- xs]

-- funciones de alto orden
-- restos xs y = map (`mod` y) xs

-- f) 'cuadrados', que dada una lista de números, devuelva la
-- lista de sus cuadrados

cuadrados :: Num a => [a] -> [a]

-- recursión explícita
cuadrados []       = []
cuadrados (x : xs) = x ^ 2 : cuadrados xs

-- listas por comprensión
-- cuadrados xs = [x ^ 2 | x <- xs] 

-- funciones de alto orden
-- cuadrados = map (^2)

-- g) 'longitudes', que dada una lista de listas, devuelve la
-- lista de sus longitudes

longitudes :: [[a]] -> [Int]

-- recursión explícita
longitudes []         = []
longitudes (xs : xss) = length xs : longitudes xss

-- listas por comprensión
-- longitudes xss = [length xs | xs <- xss]

-- funciones de alto orden
-- longitudes = map length

-- h) 'orden', que dada una lista de pares de números, devuelve
-- la lista de aquellos pares en los que la primera componente es
-- menor que el triple de la segunda

orden :: (Num a, Ord a) => [(a, a)] -> [(a, a)]

-- recursión explícita
orden []                        = []
orden ((x, y) : xs) | x < 3 * y = (x, y) : orden xs
                    | otherwise = orden xs

-- listas por comprensión
-- orden xs = [(x, y) | (x, y) <- xs, x < 3 * y] 

-- funciones de alto orden
-- orden = filter (\(x, y) -> x < 3 * y)

-- i) 'pares', que dada una lista de enteros, devuelve la lista
-- de los elementos pares

pares :: [Int] -> [Int]

-- recursión explícita
pares []                   = []
pares (x : xs) | even x    = x : pares xs
               | otherwise = pares xs

-- listas por comprensión
-- pares xs = [x | x <- xs, even x]

-- funciones de alto orden
-- pares = filter even

-- j) 'letras', que dada una lista de caracteres, devuelve la
-- lista de aquellos que son letras (minúsculas o mayúsculas)

letras :: [Char] -> [Char]

-- recursión explícita
letras []                    = []
letras (x : xs) | isLetter x = x : letras xs
                | otherwise  = letras xs

-- listas por comprensión
-- letras xs = [x | x <- xs, isLetter x]

-- funciones de alto orden
-- letras = filter isLetter

-- k) 'masDe', que dada una lista de listas 'xss' y un
-- número 'n', devuelve la lista de aquellas listas de 'xss'
-- con longitud mayor que 'n' 

masDe :: [[a]] -> Int -> [[a]]

-- recursión explícita
masDe []         n                 = []
masDe (xs : xss) n | length xs > n = xs : masDe xss n
                   | otherwise     = masDe xss n

-- listas por comprensión
-- masDe xss n = [xs | xs <- xss, length xs > n]

-- funciones de alto orden
-- masDe xss n = filter (\xs -> length xs > n) xss
