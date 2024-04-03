{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}

module P02E11 where

-- a 

modulus :: [Float] -> Float
modulus = sqrt . sum . map (^2)

-- b 
vmod :: [[Float]] -> [Float]
vmod []     = []
vmod (v:vs) = modulus v : vmod vs
