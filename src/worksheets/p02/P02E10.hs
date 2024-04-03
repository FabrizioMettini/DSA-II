module P02E10 where

-- a) [[]] ++ xs = xs
-- xs tiene que ser de tipo [[a]]
-- Es falsa ya que se tiene que [[]] ++ xs = [] : xs /= xs

-- b) [[]] ++ xs = [xs]
-- xs tiene que ser de tipo [[a]]
-- Es falsa ya que se tiene que [xs] tiene tipo [[[a]]] en vez de [[a]]

-- c) [[]] ++ xs = [] : xs
-- xs tiene que ser de tipo [[a]]
-- Es verdadera

-- d) [[]] ++ xs = [[], xs]
-- xs tiene que ser de tipo [[a]]
-- Es falsa ya que se tiene que [[], xs] tiene tipo [[[a]]] en vez de [[a]]

-- e) [[]] ++ [xs] = [[], xs]
-- xs tiene que ser de tipo [a]
-- Es verdadera

-- f) [[]] ++ [xs] = [xs]
-- xs tiene que ser de tipo [a]
-- Es falsa ya que se tiene que [[]] ++ [xs] = [[], xs] /= [xs]

-- g) [] ++ xs = [] : xs
-- xs tiene que ser de tipo [a]
-- Es falsa ya que se tiene que [] ++ xs = xs /= [] : xs

-- h) [] ++ xs = xs
-- xs tiene que ser de tipo [a]
-- Es verdadera

-- i) [xs] ++ [] = [xs]
-- xs tiene que ser de tipo a
-- Es verdadera

-- j) [xs] ++ [xs] = [xs, xs]
-- xs tiene que ser de tipo a
-- Es verdadera
