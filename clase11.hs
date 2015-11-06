
data RT t = Rose t [RT t]
    deriving Show

r1 :: RT Integer
r1 = Rose 20 []

r2 :: RT Integer
r2 = Rose 20 [Rose 33 [], Rose 45 [], Rose 107 [], Rose 160 []]

r3 :: RT Integer
r3 = Rose 15 [Rose 16 [Rose 17 [], Rose 18 []], Rose 8 [], Rose 3 []]

r4 :: RT Char
r4 = Rose 'p' [Rose 'a' [Rose 'b' [Rose 'c' []], Rose 'l' [], Rose 'o' []], Rose '3' []]

r7 :: RT Char
r7 = Rose '1' [Rose 'a' [Rose 'b' [r4, r4]]]

raiz :: RT t -> t
raiz (Rose t _) = t

hijos :: RT t -> [RT t]
hijos (Rose _ l) = l

sumarTodos :: Num t => [RT t] -> t
sumarTodos [] = 0
sumarTodos [(Rose n l)] = n + (sumarTodos l)
sumarTodos ((Rose n l):xs) = n + (sumarTodos l) + (sumarTodos xs)

sumarTodo :: Num t => RT t -> t
sumarTodo (Rose n []) = n
sumarTodo (Rose n l) = n + (sumarTodos l)

sumaTodo :: Num t => RT t -> t
sumaTodo (Rose r h) = r + (sum sumaHijos)
    where sumaHijos = sumaCadaHijo h

sumaCadaHijo :: Num t => [RT t] -> [t]
sumaCadaHijo [] = []
sumaCadaHijo (a:as) = (sumaTodo a) : (sumaCadaHijo as)

esHoja :: RT t -> Bool
esHoja (Rose _ []) = True
esHoja (Rose _ _) = False

hojas :: RT t -> [t]
hojas (Rose t []) = [t]
hojas (Rose t (x:[])) = hojas x
hojas (Rose t (x:xs)) = (hojas x) ++ (hojas (Rose t xs))

espejarTodos :: [RT t] -> [RT t]
espejarTodos [] = []
espejarTodos [(Rose t [])] = [(Rose t [])]
espejarTodos ((Rose t l):xs) = [(Rose t (reverse l))] ++ (espejarTodos xs)

espejar :: RT t -> RT t
espejar (Rose t []) = Rose t []
espejar (Rose t l) = Rose t (espejarTodos l)

--altura :: RT t -> Integer
--altura (Rose _ []) = 1
--altura (Rose _ (x:[])) = 1 + (altura x)
--altura (Rose t (x:xs)) = 1 + (max (altura x) (altura (Rose t xs)))

alturaTodos :: [RT t] -> [Integer]
alturaTodos [] = []
alturaTodos ((Rose _ []):xs) = [1] ++ (alturaTodos xs)
alturaTodos ((Rose _ l):xs) = [1 + (maximum (alturaTodos l))] ++ (alturaTodos xs)

altura :: RT t -> Integer
altura (Rose _ []) = 1
altura (Rose _ l) = 1 + (maximum (alturaTodos l))

prependLista :: t -> [[t]] -> [[t]]
prependLista t [] = []
prependLista t (l:ls) = [(t : l)] ++ (prependLista t ls)

caminos :: [RT t] -> [[t]]
caminos [] = []
caminos ((Rose t []):xs) = [[t]] ++ (caminos xs)
caminos ((Rose t l):xs) = (camino (Rose t l)) ++ (caminos xs)

camino :: RT t -> [[t]]
camino (Rose t []) = [[t]]
camino (Rose t l) = prependLista t (caminos l)

