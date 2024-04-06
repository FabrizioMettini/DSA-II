module Lab1C where

import Data.List ( nub, permutations, sortOn, subsequences )

type Texto = String

-- Definir una función que dado un caracter y un texto calcule la frecuencia 
-- con la que ocurre el caracter en el texto
-- Por ejemplo: frequency 'a' "casa" = 0.5 

frequency :: Char -> Texto -> Float
frequency c text = fromIntegral (frequencyAbsolute c text) / fromIntegral (length text)
  where frequencyAbsolute c = length . filter (== c)


-- Definir una función frequencyMap que dado un texto calcule la frecuencia 
-- con la que ocurre cada caracter del texto en éste.
-- La lista resultado debe estar ordenada respecto a la frecuencia con la que ocurre 
-- cada caracter, de menor a mayor frecuencia. 

-- Por ejemplo: frequencyMap "casa" = [('c',0.25),('s',0.25),('a',0.5)]

frequencyMap :: Texto -> [(Char, Float)]
frequencyMap text = sortOn snd $ nub $ map (\c -> (c, frequency c text)) text


-- Definir una función subconjuntos, que dada una lista xs devuelva una lista 
-- con las listas que pueden generarse con los elementos de xs.

-- Por ejemplo: subconjuntos [2,3,4] = [[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]

subconjuntos :: [a] -> [[a]]
subconjuntos = reverse . subsequences


-- Definir una función intercala :: a -> [a] -> [[a]]
-- tal que (intercala x ys) contiene las listas que se obtienen
-- intercalando x entre los elementos de ys. 

-- Por ejemplo: intercala 1 [2,3]  ==  [[1,2,3],[2,1,3],[2,3,1]]

intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x ys = (x : ys) : [head ys : r | r <- intercala x (tail ys)]


-- Definir una función permutaciones que dada una lista calcule todas las permutaciones
-- posibles de sus elementos. Ayuda: la función anterior puede ser útil. 

-- Por ejemplo: permutaciones "abc" = ["abc","bac","cba","bca","cab","acb"]

permutaciones :: [a] -> [[a]]
permutaciones = permutations
