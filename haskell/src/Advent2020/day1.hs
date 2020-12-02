module Advent2020.Day1 (run, part1) where

import Data.List.Split
import Relude
import Relude.Unsafe (fromJust, read)

run :: FilePath -> ([Int] -> Int) -> IO Int
run file runner = do
  contents <- readFileText file
  let ls = lines contents
  let xs = map ((\x -> read x :: Int) . toString) ls
  return $ runner xs

part1 :: [Int] -> Int
part1 entries = let (x, y) = fromJust $ find (\(a, b) -> a + b == 2020) (pairs entries) in x * y

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]
