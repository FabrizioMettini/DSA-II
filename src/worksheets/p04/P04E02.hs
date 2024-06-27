module P04E02 where

data BST a = Hoja | Nodo (BST a) a (BST a) deriving Show

minimum' :: BST a -> a
minimum' (Nodo Hoja a _) = a
minimum' (Nodo l _ _)    = minimum' l

-- a

maximum' :: BST a -> a
maximum' (Nodo _ a Hoja) = a
maximum' (Nodo _ _ r)    = maximum' r

-- b

checkBST :: Ord a => BST a -> Bool
checkBST Hoja               = True
checkBST (Nodo Hoja a Hoja) = True
checkBST (Nodo l a Hoja)    = a >= maximum' l && checkBST l 
checkBST (Nodo Hoja a r)    = a <  minimum' r && checkBST r
checkBST (Nodo l a r)       = a >= maximum' l && a < minimum' r && checkBST l && checkBST r

-- Alternativamente
-- checkBST :: Ord a => BST a -> Bool
-- checkBST t = sorted (inorder t)

-- inorder :: BST a -> [a]
-- inorder Hoja         = []
-- inorder (Nodo l a r) = inorder l ++ [a] ++ inorder r

-- sorted :: Ord a => [a] -> Bool
-- sorted []           = True
-- sorted [x]          = True
-- sorted l@(x : x' : xs) = (x <= x') && sorted (tail l)

-- c

splitBST :: Ord a => BST a -> a -> (BST a, BST a)
splitBST Hoja         _             = (Hoja, Hoja) 
splitBST (Nodo l a r) x | a <= x    = let (l', r') = splitBST r x in (Nodo l a l', r')
                        | otherwise = let (l', r') = splitBST l x in (l', Nodo r' a r)

-- d

joinBST :: Ord a => BST a -> BST a -> BST a
joinBST Hoja t2         = t2
joinBST (Nodo l a r) t2 = Nodo (joinBST l l') a (joinBST r r')
  where (l', r') = splitBST t2 a
