module Day6 where

import Common (letters1, parseFromFile)
import Data.List (nub)
import qualified Data.Text.Lazy as LT
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (space)

windows :: Int -> [a] -> [[a]]
windows n xs
  | length xs < n = []
  | otherwise = take n xs : windows n (tail xs)

solution :: Int -> FilePath -> IO ()
solution n inputPath = do
  input <- parseFromFile (letters1 <* (space >> eof)) inputPath

  print $ head $ filter (\(_, xs) -> xs == nub xs) $ zip [n ..] $ windows n $ LT.unpack input

part1 :: FilePath -> IO ()
part1 = solution 4

part2 :: FilePath -> IO ()
part2 = solution 14
