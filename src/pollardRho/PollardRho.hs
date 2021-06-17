module PollardRho where 

newtype Keys = Keys (Integer, Integer) deriving (Read)

breakKey :: Integer -> Integer
breakKey n = pollard 1 2 n 2 2

pollard :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
pollard i k n x y
  | d /= 1 && d /= n = d
  | i == k = pollard (i + 1) (2 * k) n x1 x1
  | otherwise = pollard (i + 1) k n x1 y
  where
    d = gcd n $ abs $ y - x
    x1 = inverseMod x n
      where
        inverseMod x n = mod (x * x - 1) n
