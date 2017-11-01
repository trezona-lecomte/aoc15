module Lib
    ( dayOne
    ) where

dayOne :: IO ()
dayOne = do
  input <- readFile "/Users/kieran/dev/haskell/aoc15/input/dayOne"
  let answer = foldl parseParen 0 input
  print answer
  return ()

parseParen :: Integer -> Char -> Integer
parseParen n '(' = n + 1
parseParen n ')' = n - 1
parseParen _ _ = undefined
