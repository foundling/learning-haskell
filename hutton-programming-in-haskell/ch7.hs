-- ex.1: rewrite [ f x | x <- xs, p x ] using map and filter

mapFilter f p xs = [ f x | x <- xs, p x ] 
mapFilter' f p = map f . filter p

-- ex. 2a
all' p [] = True
all' p (x:xs) = p x && all' p xs

-- ex. 2b
any' p [] = False
any' p (x:xs) = p x || any' p xs

-- ex. 2c
takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs 
                    | otherwise = []

-- ex. 2d
dropWhile' p [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = (x:xs)

-- ex. 3: rewrite map f and filter p using foldr

-- map f using foldr
mapEquiv' f = foldr (\x acc -> (f x) : acc) []

 -- filter p using foldr
filterEquiv f = foldr (\x acc -> if (f x) then [x] ++ acc else acc) []

-- ex. 4
dec2int :: [Int] -> Int
dec2int xs = foldl sumProducts 0 pairs 
  where
    pairs = zip digits weights
    digits = reverse xs
    weights = iterate (*10) 1
    sumProducts = (\acc (digit, weight) -> acc + (digit * weight))

and' :: [Bool] -> Bool
and' = foldr (&&) True

or' :: [Bool] -> Bool
or' = foldr (||) False

-- my own recursive implementation of foldr
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)
