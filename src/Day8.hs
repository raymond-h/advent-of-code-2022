{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day8 where

import Common (Parser, parseFromFile)
import Control.Comonad (Comonad)
import Control.Comonad.Store (Comonad (..))
import Control.Monad (guard)
import Data.Array (Array, array, bounds, indices)
import qualified Data.Array as A
import Data.Char (digitToInt, isDigit)
import Data.Foldable (Foldable (toList))
import Data.Ix (Ix (..))
import Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import Text.Megaparsec (MonadParsec (eof, takeWhile1P), sepEndBy1)
import Text.Megaparsec.Char (newline)

type Index = (Int, Int)

the :: forall a f. (Foldable f, Ord a) => f a -> Maybe a
the xs = do
  let xs' = S.fromList $ toList xs
  guard $ S.size xs' == 1
  return $ S.findMin xs'

mk2dArray :: forall a. [[a]] -> Maybe (Array Index a)
mk2dArray xss = do
  let h = length xss
  w <- the $ map length xss
  let bounds' :: (Index, Index)
      bounds' = ((1, 1), (w, h))

      elems :: [(Index, a)]
      elems = concatMap go $ zip [1 :: Int ..] xss

      go :: (Int, [a]) -> [(Index, a)]
      go (y, as) = zipWith (\x a -> ((x, y), a)) [1 :: Int ..] as
  guard $ length elems == w * h
  return $ array bounds' elems

showArray :: Show a => Array Index a -> String
showArray arr = intercalate "\n" (map showLine ys)
  where
    ((xMin, yMin), (xMax, yMax)) = bounds arr
    xs = range (xMin, xMax)
    ys = range (yMin, yMax)

    showLine :: Int -> String
    showLine y = [head $ show $ arr A.! (x, y) | x <- xs]

digits1 :: Parser LT.Text
digits1 = takeWhile1P (Just "digits") isDigit

unpackJust :: MonadFail m => m (Maybe a) -> m a
unpackJust mma = do
  ma <- mma
  case ma of
    Nothing -> fail "received Nothing, expected Just"
    (Just a) -> pure a

inputParser :: Parser (Array Index Int)
inputParser = unpackJust $ (fmap . fmap) digitToInt . mk2dArray . map LT.unpack <$> sepEndBy1 digits1 newline

addTuple :: Num a => (a, a) -> (a, a) -> (a, a)
addTuple (a, b) (c, d) = (a + c, b + d)

lineOfIndices :: (Index, Index) -> Index -> (Int, Int) -> [Index]
lineOfIndices bounds' start k = tail $ takeWhile (inRange bounds') $ iterate (addTuple k) start

lineOfValues :: Array Index a -> Index -> (Int, Int) -> [a]
lineOfValues arr start k = map (arr A.!) $ lineOfIndices (bounds arr) start k

data FocusedArray i a = FocusedArray i (Array i a) deriving (Eq, Show, Functor)

focusAt :: i -> Array i a -> FocusedArray i a
focusAt = FocusedArray

unfocus :: FocusedArray i a -> Array i a
unfocus (FocusedArray _ arr) = arr

instance Ix i => Comonad (FocusedArray i) where
  extract :: FocusedArray i a -> a
  extract (FocusedArray focusIndex arr) = arr A.! focusIndex

  duplicate :: FocusedArray i a -> FocusedArray i (FocusedArray i a)
  duplicate (FocusedArray focusIndex arrOuter) = FocusedArray focusIndex (go arrOuter)
    where
      go :: Array i a -> Array i (FocusedArray i a)
      go arr = array (bounds arr) $ map (\i -> (i, focusAt i arr)) (indices arr)

extendArray :: Ix i => (FocusedArray i a -> b) -> Array i a -> Array i b
extendArray f = unfocus . extend f . focusAt undefined

count :: Foldable f => (a -> Bool) -> f a -> Int
count p = length . filter p . toList

everyDirection :: [(Int, Int)]
everyDirection = [(1, 0), (0, 1), (-1, 0), (0, -1)]

isVisible :: FocusedArray Index Int -> Bool
isVisible (FocusedArray focusIndex arr) = any visibleInDirection everyDirection
  where
    x = arr A.! focusIndex

    visibleInDirection dir = not $ any (>= x) (lineOfValues arr focusIndex dir)

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  print $ count (== True) $ extendArray isVisible input

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x : rest)
  | not (p x) = [x]
  | otherwise = x : takeWhileInclusive p rest

scenicScore :: FocusedArray Index Int -> Integer
scenicScore (FocusedArray focusIndex arr) = product $ map (toInteger . scoreForDirection) everyDirection
  where
    x = arr A.! focusIndex

    scoreForDirection :: (Int, Int) -> Int
    scoreForDirection dir = length $ takeWhileInclusive (< x) $ lineOfValues arr focusIndex dir

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  print $ maximum $ extendArray scenicScore input
