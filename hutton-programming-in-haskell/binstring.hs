import Data.Char

{--
 -
 - bin2int: convert an array of bits into an integer
 - NOTE: the LSB is represented here as the left-most bit.
 -
--}

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum products
  where
    products = [ bit * weight | (bit,weight) <- pairs]
    pairs = zip bits weights
    weights = iterate (*2) 1 

-- expressing the sum of weights*bits like this: a + 2b + 4c + 8d
-- we can factor out the 2* and arrive at:
-- a + 2(b + 2(c + 2(d + 0)))
-- which looks a lot like how foldr with the empty list works,
-- except that [] is 0 and the cons operation becomes 'the first
-- term plus 2 * the second. 

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2*y) 0

