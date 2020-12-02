module Main (main) where

import qualified Advent2020.Day1 as Day1
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

main :: IO ()
main = do
  Options {..} <- execParser opts
  case day of
    "1" -> case part of
      "1" -> do
        putStrLn "solution for day 1 part 1:"
        Day1.run input Day1.part1 >>= print
      _ -> putStrLn "not a valid part for this day"
    _ -> putStrLn "not a valid day"
