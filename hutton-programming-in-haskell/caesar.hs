import Data.Char

-- cipher text a-z maps to 0-25
-- map a-z to int in range 0-25

let2int :: Char -> Int
let2int c = ord c - 97

-- map 0-25 to char range a-z
int2let :: Int -> Char
int2let n = chr (n + 97)


-- shift any lowercase letters by n characters, keeping in range a-z
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

-- encrypt a text w/ caesar cipher by shifting n characters
encipher :: Int -> String -> String
encipher n xs = map (shift n) xs

-- count lowercase lettes in a string
lowers :: String -> Int
lowers "" = 0 
lowers (x:xs) = (if isLower x then 1 else 0) + lowers xs

-- count occurrences of char c in string s
count :: Char -> String -> Int
count _ "" = 0
count c (x:xs) = (if c == x then 1 else 0) + (count c xs)

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [ percent (count c xs) n | c <- ['a'..'z']]
  where n = lowers xs -- only compute lowers in xs one time

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

testcase = encipher 3 "haskell is fun"

-- approximate percentage frequencies of the alphabet in english text based on ... (MUCH HAND-WAVING)
table :: [Float] = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o,e) <- zip os es]

positions x xs = [ i | (i,x') <- zip [0..25] xs, x == x']

crack code = encipher (-shiftfactor) code
  where
    shiftfactor = head (positions (minimum chitab) chitab) 
    chitab = [chisqr (rotate n table') table | n <-[0..25]]
    table' = freqs code
