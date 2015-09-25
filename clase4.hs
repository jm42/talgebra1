
-- pontencia
potencia :: Float -> Integer -> Float
potencia b p | p == 0 = 1
potencia b p = b * potencia b (p - 1)

-- suma de impares cuyo cuadrado sea menos que n
sumaImpares :: Integer -> Integer
sumaImpares n = sumaAux n 1

sumaAux :: Integer -> Integer -> Integer
sumaAux umbral x | x^2 >= umbral = 0
                 | otherwise = x + sumaAux umbral (x + 2)

division :: Integer -> Integer -> (Integer, Integer)
division a d | a < d = (0, a)
--           | otherwise = ((fst (division (a - d) d)) + 1, snd (division (a - d) d))
             | otherwise = (fst qr + 1, snd qr) where qr = division (a - d) d

divParcial :: Integer -> Integer -> [Integer]
divParcial n m | m == 1 = [1]
               | mod n m == 0 = [m] ++ divParcial n (m - 1)
               | otherwise = divParcial n (m - 1)

divisores :: Integer -> [Integer]
divisores n = divParcial n n

esPrimo :: Integer -> Bool
esPrimo n = length (divisores n) == 2

-- primerDiv n m | mod n m == 0 && esPrimo m = (m, div n m)
--               | otherwise = primerDiv n (m - 1)

-- todosDivAux n m = [d] ++ todosDivAux n (n - fst d * snd d)
--                     where d = primerDiv n (m - 1)

reverso :: [a] -> [a]
reverso [] = []
reverso l | otherwise = reverso (tail l) ++ [head l]

capicua :: Eq a => [a] -> Bool
capicua l = reverso l == l

sumal :: [Integer] -> [Integer] -> [Integer]
sumal a [] = a
sumal [] b = b
sumal a b = [head a + head b] ++ sumal (tail a) (tail b)

prodInt :: [Float] -> [Float] -> Float
prodInt a [] = sum a
prodInt [] b = sum b
prodInt a b = head a * head b + prodInt (tail a) (tail b)

division2 :: Integer -> Integer -> (Integer, Integer)
division2 a d = (quot a d, rem a d)

noTieneDivisoresHasta :: Integer -> Integer -> Bool
noTieneDivisoresHasta m n | mod n m == 0 = False
                          | m == 2 = True
                          | otherwise = noTieneDivisoresHasta (m - 1) n

esPrimo2 :: Integer -> Bool
esPrimo2 n = noTieneDivisoresHasta (n - 1) n

