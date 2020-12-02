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

run' :: FilePath -> ([Int] -> Int) -> Int
run' file runner = undefined
  where

-- contents =

id' :: t -> t
id' x = x

foo = id' 5

readFileAbsurd :: FilePath -> Text
readFileAbsurd filepath = undefined

part1 :: (MonadIO m) => Text -> m Text
part1 input = do
  print input
  return "hello"
