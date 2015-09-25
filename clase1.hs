
-- doble de un numero
doble :: Integer -> Integer
doble x = 2 * x

-- suma dos numeros
suma x y = x + y

-- norma vectorial euclideana en R^2
normaVectorial v1 v2 = sqrt (v1 ** 2 + v2 ** 2)

-- devuelve ocho
funcionConstante8 x = 8

-- devuelve la respuesta a todo
respuestaATodo = 42

-- es cero
unoSiCero 0 = 1
unoSiCero n = 0

-- el signo de n
signo 0 = 0
signo n | n > 0 = 1
signo n | n < 0 = -1

-- valor absoluto
absoluto n = (signo n) * n

-- maximo
maximo x y | x >= y = x
           | otherwise = y
maximo3 x y z = maximo (maximo x y) z

yLogico x y = x && y

(algo) x y = x && y

