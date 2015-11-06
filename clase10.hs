
data Polinomio = Mono Float Integer
               | Suma Polinomio Polinomio
               | Producto Polinomio Polinomio

evaluar :: Polinomio -> Float -> Float
evaluar (Mono a n) x = a * (x ^ n)
evaluar (Suma p0 p1) x = (evaluar p0 x) + (evaluar p1 x)
evaluar (Producto p0 p1) x = (evaluar p0 x) * (evaluar p1 x)

fillZ :: Integer -> [Float]
fillZ n | n == 0 = []
        | otherwise = (fillZ (n - 1)) ++ [0]

sumarL :: [Float] -> [Float] -> [Float]
sumarL l [] = l
sumarL [] r = r
sumarL (l:ls) (r:rs) = [l + r] ++ (sumarL ls rs)

multL :: [Float] -> [Float] -> [Float]
multL l [] = l
multL [] r = r
multL [x] (l:ls) = (x * l) : (multL [x] ls)
multL (x:xs) l = sumarL (multL [x] l) (0 : (multL xs l))

coeficientes :: Polinomio -> [Float]
coeficientes (Mono a n) = (fillZ (n - 1)) ++ [a]
coeficientes (Suma p0 p1) = sumarL (coeficientes p0) (coeficientes p1)
coeficientes (Producto p0 p1) = multL (coeficientes p0) (coeficientes p1)

sumarP :: Polinomio -> Polinomio -> Polinomio
sumarP (Mono a0 n0) (Mono a1 n1) | n0 == n1 = Mono (a0 + a1) n0
                                 | otherwise = Suma (Mono a0 n0) (Mono a1 n1)

instance Num Polinomio where
    (+) p q = Suma p q
    (*) p q = Producto p q
    negate p = Producto p (Mono (-1) 0)
    fromInteger n = Mono (fromIntegral n) 0
    abs p = undefined
    signum p = undefined

instance Show Polinomio where
    show (Mono a n) | n == 0 = (show a)
                    | n == 1 = (show a) ++ "x"
                    | otherwise = (show a) ++ "x^" ++ (show n)
    show (Suma p q) = (show p) ++ " + " ++ (show q)
    show (Producto p q) = "(" ++ (show p) ++ ") (" ++ (show q) ++ ")"

