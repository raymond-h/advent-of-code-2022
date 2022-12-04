module Day4 where

import Common
import qualified Data.Set as S
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

type Range = (Integer, Integer)

rangeParser :: Parser Range
rangeParser = do
  start <- decimal
  _ <- char '-'
  end <- decimal
  return (start, end)

inputLineParser :: Parser (Range, Range)
inputLineParser = do
  range1 <- rangeParser
  _ <- char ','
  range2 <- rangeParser
  return (range1, range2)

inputParser :: Parser [(Range, Range)]
inputParser = sepEndBy1 inputLineParser newline

rangeToSet :: Range -> S.Set Integer
rangeToSet (start, end) = S.fromList [start .. end]

isFullyContainedIn :: Range -> Range -> Bool
isFullyContainedIn a b = (rangeToSet a `S.union` rangeToSet b) == rangeToSet a

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile inputParser inputPath

  print $ length $ filter (\(a, b) -> isFullyContainedIn a b || isFullyContainedIn b a) input

isOverlapping :: Range -> Range -> Bool
isOverlapping a b = not $ S.null $ rangeToSet a `S.intersection` rangeToSet b

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile inputParser inputPath

  print $ length $ filter (uncurry isOverlapping) input
