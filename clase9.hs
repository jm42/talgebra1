
data Arbol = Hoja Integer | Ramificacion Arbol Integer Arbol deriving Show

esHoja :: Arbol -> Bool
esHoja (Hoja _) = True
esHoja otherwise = False

sumaNodos :: Arbol -> Integer
sumaNodos (Hoja v) = v
sumaNodos (Ramificacion p v n) = v + (sumaNodos p) + (sumaNodos n)

altura :: Arbol -> Integer
altura (Hoja _) = 1
altura (Ramificacion p _ n) | al > altura n = 1 + al where al = altura p
altura (Ramificacion p _ n) | otherwise = 1 + (altura n)

pertenece :: Integer -> Arbol -> Bool
pertenece n (Hoja v) = n == v
pertenece n (Ramificacion l v r) = n == v || (pertenece n l) || (pertenece n r)

data Dir = Der | Izq deriving Eq

busqueda :: [Dir] -> Arbol -> Integer
busqueda _ (Hoja v) = v
busqueda [] (Ramificacion _ v _) = v
busqueda (Der:ds) (Ramificacion _ _ r) = busqueda ds r
busqueda (Izq:ds) (Ramificacion l _ _) = busqueda ds l

-- repetir :: Integer -> String -> String
-- repetir 0 _ = ""
-- repetir n c = c ++ repetir (n - 1) c

-- espacios :: Integer -> String
-- espacios n = repetir n " "

-- ancho :: Arbol -> Integer
-- ancho (Hoja _) = 1
-- ancho (Ramificacion l _ r) = (ancho l) + (ancho r)

-- show_arbol :: Integer -> Arbol -> String
-- show_arbol n (Hoja v) = (espacios n) ++ (show v)
-- show_arbol n (Ramificacion l v r) = (espacios n)
--     ++ (espacios (anc * 2)) ++ (show v) ++ "\n"
--     ++ (espacios n) ++ (espacios ((anc * 2) + n - 1)) ++ "/ \\\n"
--     ++ (espacios n) ++ (show_arbol n l)
--     ++ (espacios n) ++ (show_arbol (n + 1) r)
--     where alt = altura l; anc = ancho l

-- instance Show Arbol where show a = show_arbol 0 a

espejo :: Arbol -> Arbol
espejo (Hoja v) = Hoja v
espejo (Ramificacion l v r) = Ramificacion (espejo r) v (espejo l)

data GArbol t = GHoja t | GRamificacion (GArbol t) t (GArbol t)

esGHoja :: GArbol a -> Bool
esGHoja (GHoja _) = True
esGHoja otherwise = False

maximo :: Ord a => GArbol a -> a
maximo (GHoja v) = v
maximo (GRamificacion l v r) = max v (max (maximo l) (maximo r))

minimo :: Ord a => GArbol a -> a
minimo (GHoja v) = v
minimo (GRamificacion l v r) = min v (min (minimo l) (minimo r))

raiz :: GArbol a -> a
raiz (GHoja v) = v
raiz (GRamificacion _ v _) = v

todosIgualesA :: Eq a => a -> GArbol a -> Bool
todosIgualesA n (GHoja v) = v == n
todosIgualesA n (GRamificacion l v r) = v == n && (todosIgualesA n l) && (todosIgualesA n r)

todosIguales :: Eq a => GArbol a -> Bool
todosIguales (GHoja _) = True
todosIguales (GRamificacion l v r) = (todosIgualesA v l) && (todosIgualesA v r)

espejar :: GArbol a -> GArbol a
espejar (GHoja v) = GHoja v
espejar (GRamificacion l v r) = GRamificacion (espejar r) v (espejar l)

esHeap :: Ord a => GArbol a -> Bool
esHeap (GHoja _) = False
esHeap (GRamificacion l v r) = (min (minimo l) (minimo r)) >= v && (esHeap l) && (esHeap r)

