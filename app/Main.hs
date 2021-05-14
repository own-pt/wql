module Main where

import SparqlGenerator

--main :: IO String
main = do
  s <- getLine
  putStrLn $ generateSPARQL s
