module P03E06 where

import P03E04 (Exp (..))

seval :: Exp -> Maybe Int
seval (Lit n)      = Just n
seval (Add e1 e2)  = case (seval e1, seval e2) of
                          (Just x, Just y) -> Just (x + y)
                          (_     , _     ) -> Nothing
seval (Sub e1 e2)  = case (seval e1, seval e2) of
                          (Just x, Just y) -> Just (x - y)
                          (_     , _     ) -> Nothing                      
seval (Prod e1 e2) = case (seval e1, seval e2) of
                          (Just x, Just y) -> Just (x * y)
                          (_     , _     ) -> Nothing
seval (Div e1 e2)  = case (seval e1, seval e2) of
                          (Just x, Just y) -> if y /= 0 then Just (x `div` y) else Nothing
                          (_     , _     ) -> Nothing
