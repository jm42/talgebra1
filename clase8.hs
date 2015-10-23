
data Dir = Norte | Este | Sur | Oeste deriving Show
type Pos = (Int, Int)
type Tortu = (Pos, Dir)

arranca :: Tortu
arranca = ((0, 0), Norte)

girarDerecha :: Tortu -> Tortu
girarDerecha (p, Norte) = (p, Este)
girarDerecha (p, Este) = (p, Sur)
girarDerecha (p, Sur) = (p, Oeste)
girarDerecha (p, Oeste) = (p, Norte)

avanzar :: Tortu -> Int -> Tortu
avanzar ((x, y), Norte) o = ((x, y - o), Norte)
avanzar ((x, y), Este) o = ((x + o, y), Este)
avanzar ((x, y), Sur) o = ((x, y + o), Sur)
avanzar ((x, y), Oeste) o = ((x - o, y), Oeste)

data Figura = Circulo Float Float Float | Rectangulo Float Float Float Float deriving Show

ci :: Figura
ci = Circulo pi 0 0

r1 :: Float -> Figura
r1 x = Rectangulo 0 0 a a where a = x / (sqrt 2)

area :: Figura -> Float
area (Circulo r _ _) = pi * (r ** 2)
area (Rectangulo x0 y0 x1 y1) = abs ((x1 - x0) * (y1 - y0))

data Punto = Point Float Float
data Figura2 = Rectangulo2 Punto Punto | Circulo2 Punto Float

area2 :: Figura2 -> Float
area2 (Circulo2 _ r) = pi * (r ** 2)
area2 (Rectangulo2 (Point x0 y0) (Point x1 y1)) = abs ((x1 - x0) * (y1 - y0))

data ProgAritmetica = Vacio | CongruentesA Integer Integer

esMultiplo :: Integer -> Integer -> Bool
esMultiplo a b = mod a b == 0

pertenece :: Integer -> ProgAritmetica -> Bool
pertenece n Vacio = False
pertenece n (CongruentesA a b) = esMultiplo (n - a) b

incluido :: ProgAritmetica -> ProgAritmetica -> Bool
incluido Vacio _ = True
incluido _ Vacio = False
incluido (CongruentesA a0 b0) (CongruentesA a1 b1) = (esMultiplo b0 b1) && (pertenece a0 (CongruentesA a1 b1))

instance Show ProgAritmetica where
    show Vacio = "{}"
    show (CongruentesA x d) = "{a en Z | a = " ++ (show x) ++ " (mod " ++ (show d) ++ ")}"

sumaProg :: ProgAritmetica -> ProgAritmetica -> ProgAritmetica
sumaProg Vacio p = p
sumaProg p Vacio = p
sumaProg (CongruentesA a0 b0) (CongruentesA a1 b1) = CongruentesA (a0 + a1) (gcd b0 b1)

interseccion :: ProgAritmetica -> ProgAritmetica -> ProgAritmetica
interseccion Vacio _ = Vacio
interseccion _ Vacio = Vacio
interseccion (CongruentesA a0 b0) (CongruentesA a1 b1) = CongruentesA (a0 * b1 + a1 * b0) (b0 * b1)

-- euclides :: Integer -> Integer -> (Integer, Integer, Integer)
-- euclides 0 _ =

