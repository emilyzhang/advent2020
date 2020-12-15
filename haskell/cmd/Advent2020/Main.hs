module Main (main) where

import qualified Advent2020.Day1 as Day1
import qualified Advent2020.Day2 as Day2
import Options.Applicative (ParserInfo, briefDesc, execParser, help, helper, info, long, progDesc, strOption)
import Relude

data Options = Options
  { day :: Text,
    part :: Text,
    input :: FilePath
  }
  deriving (Show)

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (briefDesc <> progDesc "advent of code 2020 solutions")
  where
    options =
      Options
        <$> strOption (long "day")
        <*> strOption (long "part")
        <*> strOption (long "input" <> help "if more than one input file is necessary, separate the filepath by commas")

invalidPart :: IO ()
invalidPart = putStrLn "not a valid part for this day"

main :: IO ()
main = do
  Options {..} <- execParser opts
  case day of
    "1" -> case part of
      "1" -> do
        Day1.run input Day1.part1 >>= print
      "2" -> do
        Day1.run input Day1.part2 >>= print
      _ -> invalidPart
    "2" -> case part of
      "1" -> do
        Day2.run input Day2.part1 >>= print
      -- "2" -> do
      -- Day1.run input Day1.part2 >>= print
      _ -> invalidPart
    _ -> putStrLn "not a valid day"
