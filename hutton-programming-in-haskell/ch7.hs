filter' p (x:xs) | p x = x : filter' p xs 
                 | otherwise = filter' p xs

all' p [] = True
all' p (x:xs) = p x && all' p xs

any' p [] = False
any' p (x:xs) = p x || any' p xs

takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs 
                    | otherwise = []

dropWhile' p [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = (x:xs)

and' :: [Bool] -> Bool
and' = foldr (&&) True

or' :: [Bool] -> Bool
or' = foldr (||) False

-- foldr' (+) 0 [1,2,3]
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)
