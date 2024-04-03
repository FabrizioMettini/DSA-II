module P02E12 where

type NumBin = [Bool]

numBinToString :: NumBin -> String
numBinToString = map (\x -> if x then '1' else '0') . reverse

stringToNumBin :: String -> NumBin
stringToNumBin = map (== '1') . reverse

sumaBit :: Bool -> Bool -> Bool -> (Bool, Bool)
sumaBit a b carryIn = ((a /= b) /= carryIn, a && b || (a /= b) && carryIn)

-- a

sumaBin :: NumBin -> NumBin -> NumBin
sumaBin a b = sumaBin' a b False
  where sumaBin' (ai:as) (bi:bs) carryIn = let (s, carryOut) = sumaBit ai bi carryIn
                                           in s : sumaBin' as bs carryOut
        sumaBin' (ai:as) []      carryIn = let (s, carryOut) = sumaBit ai False carryIn
                                           in s : sumaBin' as [] carryOut
        sumaBin' []      (bi:bs) carryIn = let (s, carryOut) = sumaBit False bi carryIn
                                           in s : sumaBin' [] bs carryOut
        sumaBin' []      []      carryIn = [carryIn]

-- b

productoBin :: NumBin -> NumBin -> NumBin
productoBin (ai:as) b = let sumando = if ai then b else []
                        in sumaBin sumando (productoBin as (False : b))
productoBin _       _ = []

-- c

divisionPor2Bin :: NumBin -> (NumBin, NumBin)
divisionPor2Bin [] = ([], [])
divisionPor2Bin a  = (tail a, [head a])
