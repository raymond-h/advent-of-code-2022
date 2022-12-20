{-# LANGUAGE NumericUnderscores #-}

module Day20 (part1, part2) where

import Common (Parser, decimal, parseFromFile)
import Control.Monad (guard)
import Data.Foldable (foldlM)
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import Text.Megaparsec (eof, sepEndBy1)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (signed)

type InputNumber = Int

inputParser :: Parser [InputNumber]
inputParser = sepEndBy1 (signed (pure ()) decimal) newline <* eof

wrapIndex :: Int -> Int -> Int
wrapIndex upperBound index
  | upperBound <= 1 = index
  | divResult > 0 && modResult == 0 = upperBound - 1
  | otherwise = modResult
  where
    (divResult, modResult) = index `divMod` (upperBound - 1)

mixNumberAtIndex :: Int -> Seq.Seq (Int, InputNumber) -> Maybe (Seq.Seq (Int, InputNumber))
mixNumberAtIndex index ns = do
  guard $ index >= 0 && index < Seq.length ns

  (prefix, n, suffix) <- case Seq.breakl ((== index) . fst) ns of
    (prefix', (_, n') Seq.:<| suffix') -> Just (prefix', n', suffix')
    _ -> Nothing

  let actualIndexOfN = Seq.length prefix

      ns' = prefix Seq.>< suffix
      newIndexOfN = wrapIndex (Seq.length ns) (actualIndexOfN + n)

      resultNs = Seq.insertAt newIndexOfN (index, n) ns'

  return resultNs

mixRoutine :: Seq.Seq (Int, InputNumber) -> Maybe (Seq.Seq (Int, InputNumber))
mixRoutine indexedNs = foldlM (flip mixNumberAtIndex) indexedNs [0 .. Seq.length indexedNs - 1]

findCoords :: Seq.Seq InputNumber -> Maybe [InputNumber]
findCoords ns' = do
  indexOfZero <- Seq.elemIndexL 0 ns'

  let is = map ((`mod` Seq.length ns') . (+ indexOfZero)) [1_000, 2_000, 3_000]

  mapM (ns' Seq.!?) is

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile inputParser inputPath

  let indexedNs = Seq.fromList $ zip [0 :: Int ..] input
      indexedNs' = fromJust $ mixRoutine indexedNs

      ns' = snd <$> indexedNs'

  print $ sum <$> findCoords ns'

decryptionKey :: Int
decryptionKey = 811_589_153

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile inputParser inputPath

  let input' = map (* decryptionKey) input
      indexedNs = Seq.fromList $ zip [0 :: Int ..] input'
      indexedNs' = last $ take 11 $ iterate (fromJust . mixRoutine) indexedNs
      ns' = snd <$> indexedNs'

  print $ sum <$> findCoords ns'
