module Day3 where

import Common (Parser, letters1, parseFromFile)
import Data.Bifunctor (bimap)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import Text.Megaparsec (eof, sepEndBy1)
import Text.Megaparsec.Char (newline)

linesParser :: Parser [LT.Text]
linesParser = sepEndBy1 letters1 newline

splitAtMiddle :: [Char] -> ([Char], [Char])
splitAtMiddle cs = splitAt (length cs `div` 2) cs

scoreMap :: M.Map Char Integer
scoreMap = M.fromList (zip ['a' .. 'z'] [1 .. 26]) <> M.fromList (zip ['A' .. 'Z'] [27 .. 52])

part1Solution :: [LT.Text] -> Integer
part1Solution input = sum $ map ((scoreMap M.!) . head . S.elems . uncurry S.intersection . bimap S.fromList S.fromList . splitAtMiddle . LT.unpack) input

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile (linesParser <* eof) inputPath

  print $ part1Solution input

groupTriples :: [a] -> [[a]]
groupTriples [] = []
groupTriples xs = take 3 xs : groupTriples (drop 3 xs)

part2Solution :: [LT.Text] -> Integer
part2Solution input = sum $ map ((scoreMap M.!) . head . S.elems . foldr1 S.intersection) $ groupTriples $ map (S.fromList . LT.unpack) input

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile (linesParser <* eof) inputPath

  print $ part2Solution input
