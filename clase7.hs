
emp :: [Integer] -> [[Integer]]
emp [] = []
emp [o] = [[o]]
emp (x:xs) | x == head xs = [(x : head r)] ++ (tail r) where r = emp xs
emp (x:xs) | otherwise = [[x]] ++ (emp xs)

