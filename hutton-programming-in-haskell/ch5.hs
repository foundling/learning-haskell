-- ex 1
v = sum [ n ^ 2 | n <- [1..100]]

-- ex 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- ex 3
square' n = [ (x,y) | (x,y) <- grid n n, x /= y]

-- ex 4
replicate' :: Int -> a -> [a]
replicate' n x = [ x | n <- [0..n-1]]

-- ex 5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2, maximum [x,y,z] <= 10]

-- ex 6

factors n = [x | x <- [1..(n-1)], n `mod` x == 0]
perfects n = [ x | x <- [1..n], sum (factors x) == x]

-- ex 7
-- re-write this [(x,y) | x <- [1,2], y <- [3,4]]
-- using a nested comprehension and concat 
--
concat [[ (x,y) | y <- [3,4]] | x <- [1,2]]

