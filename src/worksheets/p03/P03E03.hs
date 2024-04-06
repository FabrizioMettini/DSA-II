module P03E03 where

data CList a = EmptyCL
               | CUnit a
               | Consnoc a (CList a) a
               deriving Show

-- a

headCL :: CList a -> a
headCL (CUnit x)       = x
headCL (Consnoc x _ _) = x

tailCL :: CList a -> CList a
tailCL (CUnit x)       = EmptyCL
tailCL (Consnoc _ y z) = case y of
                           EmptyCL  -> CUnit z
                           CUnit y' -> Consnoc y' EmptyCL z
                           _        -> Consnoc (headCL y) (tailCL y) z

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _       = False

isCUnit :: CList a -> Bool
isCUnit (CUnit x) = True
isCUnit _         = False

-- b

reverseCL :: CList a -> CList a
reverseCL (Consnoc x y z) = Consnoc z (reverseCL y) x
reverseCL clist           = clist

-- c

inits :: CList a -> CList (CList a)
inits clist = listToCL (initsAux clist)

-- d

lasts:: CList a -> CList (CList a)
lasts clist = listToCL (map reverseCL (initsAux (reverseCL clist)))

-- e

concatCL :: CList (CList a) -> CList a
concatCL EmptyCL         = EmptyCL
concatCL (CUnit x)       = x
concatCL (Consnoc x y z) = x +++ concatCL y +++ z

-- auxiliares

listToCL :: [a] -> CList a
listToCL = foldr consCL EmptyCL

initsAux :: CList a -> [CList a]
initsAux EmptyCL = [EmptyCL]
initsAux clist = EmptyCL : map (consCL (headCL clist)) (initsAux (tailCL clist))

consCL :: a -> CList a -> CList a
consCL w EmptyCL         = CUnit w
consCL w (CUnit x)       = Consnoc w EmptyCL x
consCL w (Consnoc x y z) = Consnoc w (consCL x y) z

initCL :: CList a -> CList a
initCL (CUnit x)             = EmptyCL
initCL (Consnoc x EmptyCL z) = CUnit x
initCL (Consnoc x y z)       = Consnoc x (initCL y) (lastCL y)

lastCL :: CList a -> a
lastCL (CUnit x)       = x
lastCL (Consnoc _ _ z) = z

(+++) :: CList a -> CList a -> CList a
(+++) EmptyCL   y       = y
(+++) x         EmptyCL = x
(+++) (CUnit x) y       = Consnoc x (initCL y) (lastCL y)
(+++) x         y       = Consnoc (headCL x) (tailCL x +++ initCL y) (lastCL y)
