module Cypher where

import Data.Char (chr, ord)
import Debug.Trace ()
import System.Random ( newStdGen, mkStdGen, Random(randomRs) )

newtype Cypher = Tokens [Integer] deriving (Read, Show)

newtype Keys = Keys (Integer, Integer) deriving (Read, Show)

generateKeysPair :: IO ()
generateKeysPair = do
  (n, e, d) <- runRsa
  let public = Keys (n, e)
      private = Keys (n, d)
  writeFile "public" (show public)
  writeFile "private" (show private)
  putStrLn "The keys are created with success!"

encode :: String -> IO ()
encode file = do
  text <- readFile file
  code <- loadKey "public"
  writeFile ("encoded-" ++ file) (show (encrypt text code))
  putStrLn ("The file " ++ file ++ " was encrypted with success!")

encrypt :: String -> Keys -> Cypher
encrypt file private = Tokens $ map (\m -> expMod m e n) blocks
  where
    blocks = map stringToInteger $ divide 1 file
    (n, e) = readKey private where readKey (Keys c) = c

decode :: String -> IO ()
decode file = do
  text <- readFile file
  private <- loadKey "private"
  putStrLn ("The file " ++ file ++ " was decrypted with success:\n")
  putStrLn (decrypt (read text) private)

decrypt :: Cypher -> Keys -> String
decrypt text private =
  concatMap (\c -> integerToString (expMod c d n)) blocks
  where
    blocks = getCode text where getCode (Tokens a) = a
    (n, d) = readKey private where readKey (Keys c) = c

divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n list = head : divide n tail
  where
    (head, tail) = splitAt n list

findD :: Integer -> IO Integer
findD fiN = do
  head . filter (\x -> gcd x fiN == 1) . 
    randomRs (2 :: Integer, (fiN -1) :: Integer) <$> newStdGen

runRsa :: IO (Integer, Integer, Integer)
runRsa = do
  p <- primeNumber (2 ^ 1, 2 ^ 52 -1)
  q <- primeNumber (2 ^ 1, 2 ^ 52 -1)
  let n = p * q
      fiN = (p -1) * (q -1)
  d <- findD fiN
  let (e, _) = eucExtended d fiN
  return (n, if e < 0 then fiN - e else e, d)

loadKey :: FilePath -> IO Keys
loadKey file = do
  s <- readFile file
  return (read s)

stringToInteger :: String -> Integer
stringToInteger s =
  sum [toInteger (ord c) * 1000 ^ x | (c, x) <- zip s [(0 :: Integer) ..]]

integerToString :: Integer -> String
integerToString n
  | q == 0 = [chr r]
  | otherwise = chr r : integerToString q
  where
    r = fromIntegral ra
    (q, ra) = quotRem n 1000

primeNumber :: (Integer, Integer) -> IO Integer
primeNumber (bot, top) = do
  head . filter (`mrTest` 20) . randomRs (bot, top) <$> newStdGen

-- Miller Rabin test
mrTest :: Integer -> Integer -> Bool
mrTest n k
  | any (\b -> not (pNumber b n)) [2 .. 40] = False
  | otherwise = all (\a -> fail (expMod a d n)) notFail
  where
    pNumber p1 p2 = expMod p1 (p2 -1) p2 == 1
    notFail = 
      take (fromIntegral k) $ randomRs (2 :: Integer, (n -2) :: Integer) seed
    seed = mkStdGen (fromIntegral n)
    fail x
      | x == 1 || x == n -1 = True
      | witness (s -1) x = True
      | otherwise = False
    witness counter x
      | counter <= 0 = False
      | newX == 1 = False
      | newX == n -1 = True
      | otherwise = witness (counter -1) newX
      where
        newX = expMod x 2 n
    (s, d) = findPrivateKey (n -1) 0

findPrivateKey :: Integer -> Integer -> (Integer, Integer)
findPrivateKey n s
  | remainder == 0 = findPrivateKey newN (s + 1)
  | otherwise = (s, n)
  where
    (newN, remainder) = divMod n 2

expMod :: Integer -> Integer -> Integer -> Integer
expMod a x m
  | x == 0 = 1
  | x == 1 = a `mod` m
  | even x =
    let p = expMod a (x `div` 2) m `mod` m
     in p ^ 2 `mod` m
  | otherwise = a * expMod a (x -1) m `mod` m

-- https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
eucExtended :: Integer -> Integer -> (Integer, Integer)
eucExtended a b
  | b == 0 = (1, 0)
  | otherwise = (t, s - q * t)
  where
    (q, r) = quotRem a b
    (s, t) = eucExtended b r
