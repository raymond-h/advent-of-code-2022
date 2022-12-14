module Day1 where

import Common (Parser, parseFromFile)
import Data.List (sort)
import Text.Megaparsec (MonadParsec (eof, lookAhead, try), sepBy1)
import Text.Megaparsec.Char (digitChar, newline)
import Text.Megaparsec.Char.Lexer (decimal)

integerGroupParser :: Parser [Integer]
integerGroupParser = sepBy1 decimal (try (newline >> lookAhead digitChar))

inputParser :: Parser [[Integer]]
inputParser = sepBy1 integerGroupParser (try (newline >> newline)) <* newline <* eof

part1 :: FilePath -> IO ()
part1 inputPath = do
  res <- parseFromFile inputParser inputPath

  print $ maximum $ map sum res

part2 :: FilePath -> IO ()
part2 inputPath = do
  res <- parseFromFile inputParser inputPath

  print $ sum $ take 3 $ reverse $ sort $ map sum res
