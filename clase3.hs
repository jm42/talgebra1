
doblefact :: Integer -> Integer
doblefact 2 = 2
doblefact n | mod n 2 == 0 = n * doblefact (n - 2)
            | otherwise = undefined

combinatorio :: Integer -> Integer -> Integer
combinatorio n 0 = 1
combinatorio 0 m = 0
combinatorio 1 m = 1
combinatorio n 1 = n
combinatorio n m | n == m = 1
combinatorio n m | m /= 0 = combinatorio (n - 1) m + combinatorio (n - 1) (m - 1)

