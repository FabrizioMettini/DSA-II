module P03E01 where

type Color = (Float, Float, Float)
 
mezclar :: Color -> Color -> Color
mezclar (r, g, b) (r', g', b') = (prom r r', prom g g', prom b b') 
    where prom x y = (x + y) / 2
