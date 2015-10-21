
funcion l| length l == 0 = []
         | length l == 1 = [head l]
         | otherwise = (head l) : (funcion ((head l) + (head (tail l)) : (tail (tail l))))

