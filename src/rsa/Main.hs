module Main where

import Cypher ( generateKeysPair, encode, decode )

main :: IO ()
main = do
  generateKeysPair
  encode "entry"
  decode "encoded-entry"