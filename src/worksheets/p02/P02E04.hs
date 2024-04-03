{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant ==" #-}             -- c
{-# HLINT ignore "Redundant bracket" #-}        -- e

module P02E04 where

-- a) if true then false else true where false = True; true = False
-- bien formada y denota el valor False

-- b) if if then then else else
-- error sintáctico

-- c) False == (5 > 4)
-- bien formada y denota el valor False

-- d) 1 < 2 < 3
-- error de tipos

-- e) 1 + if ('a' < 'z') then -1 else 0
-- bien formada y denota el valor 0

-- f) if fst p then fst p else snd p where p = (True, 2)
-- error de tipos

-- g) if fst p then fst p else snd p where p = (True, False)
-- bien formada y denota el valor True

-- Expresiones bien formadas

expresionA :: Bool
expresionA = if true then false else true where false = True; true = False

expresionC :: Bool
expresionC = False == (5 > 4)

expresionE :: Integer
expresionE = 1 + if ('a' < 'z') then -1 else 0

expresionG :: Bool
expresionG = if fst p then fst p else snd p where p = (True, False)
