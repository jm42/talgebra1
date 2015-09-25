
pertenece :: Integer -> [Integer] -> Bool
pertenece n [] = False
pertenece n l | head l == n = True
              | otherwise = pertenece n (tail l)

hayRepetidos :: [Integer] -> Bool
hayRepetidos [] = False
hayRepetidos l | pertenece (head l) (tail l) = True
               | otherwise = hayRepetidos (tail l)

menores :: Integer -> [Integer] -> [Integer]
menores n [] = []
menores n l | p < n = [p] ++ menores n (tail l) where p = head l
menores n l | otherwise = menores n (tail l)

quitar :: Integer -> [Integer] -> [Integer]
quitar n [] = []
-- quitar n l | head l == n = quitar (tail l)
quitar n l | head l == n = tail l
           | otherwise = [head l] ++ quitar n (tail l)

maximo :: [Integer] -> Integer
maximo l | length (menores p l) == length l - 1 = p where p = head l
maximo l | otherwise = maximo (tail l)

enBase :: Integer -> Integer -> [Integer]
enBase d 0 = []
enBase d n = enBase d (div n d) ++ [mod n d]

deBase :: Integer -> [Integer] -> Integer
deBase d [] = 0
deBase d l = (head l) * d ^ (length l - 1) + deBase d (tail l)

capicuaPara :: [Integer] -> [Integer]
capicuaPara l | reverse l == l = l
capicuaPara l | otherwise = capicuaPara (enBase 10 (deBase 10 l + deBase 10 (reverse l)))

cambioBase :: Integer -> Integer -> [Integer] -> [Integer]
cambioBase b1 b2 l = enBase b2 (deBase b1 l)

esASC :: [Integer] -> Bool
esASC [] = True
esASC l | length l == 1 = True
esASC l | head l < head (tail l) && esASC (tail l) = True
esASC l | otherwise = False

rmRepetidos :: [Integer] -> [Integer]
rmRepetidos l | not (hayRepetidos l) = l
rmRepetidos l | pertenece (head l) (tail l) = rmRepetidos (tail l)
rmRepetidos l | otherwise = [head l] ++ rmRepetidos (tail l)

rmRepetidosPrimera :: [Integer] -> [Integer]
rmRepetidosPrimera l = reverse (rmRepetidos (reverse l))

unionListasOrdenadas :: [Integer] -> [Integer] -> [Integer]
unionListasOrdenadas a [] = a
unionListasOrdenadas [] b = b
unionListasOrdenadas a b | ap < bp = [ap] ++ [bp] ++ unionListasOrdenadas (tail a) (tail b) where ap = head a ; bp = head b
unionListasOrdenadas a b | otherwise = [bp] ++ [ap] ++ unionListasOrdenadas (tail a) (tail b) where ap = head a ; bp = head b

ordenarLista :: [Integer] -> [Integer]
ordenarLista [] = []
ordenarLista l = ordenarLista (quitar m l) ++ [m] where m = maximo l

