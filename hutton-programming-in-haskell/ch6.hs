factorial :: Int -> Int
factorial n | n == 0 = 1

sumdown :: Int -> Int
sumdown n | n == 0 = 0
          | n > 0 = n + sumdown (n - 1)

mult :: Int -> Int -> Int
mult 0 _ = 0
mult _ 0 = 0
mult a b | b > 1 = a + mult a (b - 1) 
         | b == 1 = a

exp' :: Int -> Int -> Int
exp' a 0 = 1
exp' a b = a * exp' a (b - 1)

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && (and' xs)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs   

replicate' :: Int -> a -> [a]
replicate' 0 x = [] 
replicate' n x = [x] ++ (replicate' (n - 1) x) 

elem' y [] = False
elem' y (x:xs) = y == x || elem y xs

merge' xs [] = xs
merge' [] ys = ys
merge' (x:xs) (y:ys) = 
  if x < y
    then [x] ++ merge' xs (y:ys)
  else [y] ++ merge' (x:xs) ys

halve xs = (take half xs, drop half xs)
  where half = (length xs) `div` 2

-- could also do pattern matching approach 
msort [] = []
msort [x] = [x]
msort xs = merge' (msort first_half) (msort second_half) 
    where (first_half, second_half) = halve xs 
