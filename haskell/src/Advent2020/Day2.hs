{-# LANGUAGE TypeApplications #-}

module Advent2020.Day2 (part1, run) where

import Data.Either.Extra (mapLeft)
import Data.Text (foldl)
import Relude
import Text.Megaparsec (Parsec, eof, errorBundlePretty, runParser, someTill)
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    digitChar,
    newline,
    spaceChar,
  )

data PasswordCheck = PasswordCheck
  { character :: Char,
    password :: Text,
    minLetters :: Int,
    maxLetters :: Int
  }
  deriving (Show)

type Parser = Parsec Void Text

run :: FilePath -> (Text -> Either Text Int) -> IO Int
run file runner = do
  contents <- readFileText file
  let solution = runner contents
  case solution of
    Left _ -> return 0
    Right i -> return i

parse :: Text -> Either Text [PasswordCheck]
parse input = traceShowId (mapLeft (toText . errorBundlePretty) $ runParser (passwordCheckParser `someTill` eof) "" input)

bundle :: [Either b a] -> Either b [a]
bundle l = foldr f (Right []) l
  where
    f :: Either b a -> Either b [a] -> Either b [a]
    f a b = do
      item <- a
      previousList <- b
      return (item : previousList)

-- f a b = a >>= (\item -> (b >>= \previousList -> return (item : previousList)))

passwordCheckParser :: Parser PasswordCheck
passwordCheckParser = do
  (minLetters', maxLetters') <- validationParser
  minLetters <- case minLetters' of
    Right a -> return a
    Left e -> fail $ toString e
  maxLetters <- case maxLetters' of
    Right a -> return a
    Left e -> fail $ toString e
  character <- characterParser
  password <- passwordParser
  return $ PasswordCheck {..}
  where
    validationParser :: Parser (Either Text Int, Either Text Int)
    validationParser = do
      minLetters <- digitChar `someTill` char '-'
      maxLetters <- digitChar `someTill` spaceChar
      return (readEither @Int (toString minLetters), readEither @Int (toString maxLetters))

    characterParser :: Parser Char
    characterParser = do
      character <- alphaNumChar
      _ <- char ':'
      _ <- spaceChar
      return $ character

    passwordParser :: Parser Text
    passwordParser = do
      password <- alphaNumChar `someTill` (void newline <|> eof)
      return $ toText password

check :: PasswordCheck -> Bool
check PasswordCheck {..} = do
  let count = countChar character password
  count >= minLetters && count <= maxLetters
  where
    countChar :: Char -> Text -> Int
    countChar c pw = foldl f 0 pw
      where
        f :: Int -> Char -> Int
        f count ch =
          if ch == c
            then count + 1
            else count

part1 :: Text -> Either Text Int
part1 input = do
  passwords <- parse input
  let validPasswords = check <$> passwords
  return $ foldr f 0 validPasswords
  where
    f True i = i + 1
    f False i = i
