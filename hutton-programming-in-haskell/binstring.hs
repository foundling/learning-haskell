import Data.Char

type Bit = Int

-- take [0,1,1,0] and produce 6
bin2int :: [Bit] -> Int
bin2int bits = sum products
  where
    products = [ bit * weight | (bit,weight) <- pairs]
    pairs = zip bits $ iterate (*2) 1 

