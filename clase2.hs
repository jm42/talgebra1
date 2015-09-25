
crearPar :: a -> b -> (a, b)
crearPar p q = (p, q)

invertir :: (a, b) -> (b, a)
invertir t = (snd t, fst t)

distancia :: (Float, Float) -> (Float, Float) -> Float
distancia p1 p2 = sqrt ((fst p1 - fst p2)^2 + (snd p1 - snd p2)^2)

raices :: Float -> Float -> Float -> (Float, Float)
raices a b c | a == 0 = (0.0, 0.0)
raices a b c | otherwise = (-b + (sqrt (b**2 - 4 * a * c)) / (2 * a), -b -( sqrt (b**2 - 4 * a * c)) / (2 * a))

listar :: a -> a -> a -> [a]
listar a b c = [a, b, c]

rangoDePaso :: Integer -> Integer -> Integer -> [Integer]
rangoDePaso n1 n2 n3 = [n1,n1 + n3..n2]

