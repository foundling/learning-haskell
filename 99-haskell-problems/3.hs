-- kth element of a list
elementAt :: [a] -> Int -> a 
elementAt (x:xs) 1 = x
elementAt (x:xs) k = elementAt xs (k - 1)
