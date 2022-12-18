{-# LANGUAGE TupleSections #-}

module Day18 (part1, part2) where

import Common (Parser, char, count, decimal, parseFromFile, tuplify, untuplify)
import Common.Array (defaultArray)
import Control.Comonad (extract)
import qualified Data.Array as A
import Data.Maybe (fromMaybe)
import Day8 (FocusedArray (FocusedArray), extendArray)
import Linear (V3 (V3))
import Optics (each, over)
import Text.Megaparsec (eof, sepEndBy1)
import Text.Megaparsec.Char (newline)

type Coord = V3 Integer

(!?) :: A.Ix i => A.Array i a -> i -> Maybe a
arr !? p
  | A.inRange (A.bounds arr) p = Just $ arr A.! p
  | otherwise = Nothing

lavaDropletParser :: Parser Coord
lavaDropletParser = V3 <$> decimal <* char ',' <*> decimal <* char ',' <*> decimal

inputParser :: Parser [Coord]
inputParser = sepEndBy1 lavaDropletParser newline

findBounds :: [Coord] -> (Coord, Coord)
findBounds ps = (untuplify mins, untuplify maxs)
  where
    components = unzip3 $ map tuplify ps
    mins = over each minimum components
    maxs = over each maximum components

lavaDropletArray :: [Coord] -> A.Array Coord Bool
lavaDropletArray ps = defaultArray bounds' False A.// map (,True) ps
  where
    bounds' = findBounds ps

neighbors :: [V3 Integer]
neighbors =
  [ V3 1 0 0,
    V3 (-1) 0 0,
    V3 0 1 0,
    V3 0 (-1) 0,
    V3 0 0 1,
    V3 0 0 (-1)
  ]

countSurfaces :: FocusedArray Coord Bool -> Int
countSurfaces fa@(FocusedArray p arr)
  | not (extract fa) = 0
  | otherwise = 6 - count id (map (isBlocked . (+ p)) neighbors)
  where
    isBlocked :: Coord -> Bool
    isBlocked p' = fromMaybe False $ arr !? p'

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let arr = lavaDropletArray input
      arr' = extendArray countSurfaces arr

  print $ sum arr'

iterateUntilFixpoint :: (Eq a, Show a) => (a -> a) -> a -> a
iterateUntilFixpoint f x
  | x == x' = x
  | otherwise = iterateUntilFixpoint f x'
  where
    x' = f x

isReachableByAir :: A.Array Coord Bool -> FocusedArray Coord Bool -> Bool
isReachableByAir solidArr fa@(FocusedArray p prevArr)
  | extract fa = True
  | fromMaybe False (solidArr !? p) = False
  | otherwise = any (isPrevReachable . (+ p)) neighbors
  where
    isPrevReachable :: Coord -> Bool
    isPrevReachable p' = fromMaybe True $ prevArr !? p'

countSurfacesTouchingAir :: A.Array Coord Bool -> FocusedArray Coord Bool -> Int
countSurfacesTouchingAir airArray fa@(FocusedArray p _)
  | not (extract fa) = 0
  | otherwise = count id (map (isOpen . (+ p)) neighbors)
  where
    isOpen :: Coord -> Bool
    isOpen p' = fromMaybe True $ airArray !? p'

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let arr = lavaDropletArray input
      baseAirArray = defaultArray (A.bounds arr) False
      airArray = iterateUntilFixpoint (extendArray (isReachableByAir arr)) baseAirArray

      arr' = extendArray (countSurfacesTouchingAir airArray) arr

  print $ sum arr'
