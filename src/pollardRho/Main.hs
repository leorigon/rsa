module Main where

import PollardRho ( Keys(Keys), breakKey )
import System.TimeIt (timeIt)


main :: IO ()
main = do
  content <- readFile "public"
  let Keys (n, y) = read content
  timeIt $ print (breakKey n)
