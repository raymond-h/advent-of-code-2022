{-# LANGUAGE TupleSections #-}

module Day14 where

import Common (Parser, count, lexeme, parseFromFile, sepBy1NonGreedy)
import Common.Array (defaultArray)
import Data.Array (Array, Ix (inRange), assocs, bounds, range, (!), (//))
import qualified Data.Set as S
import Linear (V2 (V2))
import Text.Megaparsec (MonadParsec (eof), sepEndBy1)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Coord = V2 Integer

coordParser :: Parser Coord
coordParser = V2 <$> lexeme decimal <*> (lexeme (char ',') *> lexeme decimal)

pathParser :: Parser [Coord]
pathParser = sepBy1NonGreedy coordParser (lexeme $ string "->")

inputParser :: Parser [[Coord]]
inputParser = sepEndBy1 pathParser newline

isCardinalDirection :: V2 Integer -> Bool
isCardinalDirection (V2 _ 0) = True
isCardinalDirection (V2 0 _) = True
isCardinalDirection _ = False

lineBetween :: Coord -> Coord -> [Coord]
lineBetween a b =
  if isCardinalDirection delta
    then b : takeWhile (/= b) (iterate (+ delta) a)
    else error "not a straight line between coords"
  where
    delta :: V2 Integer
    delta = signum $ b - a

v2x :: V2 a -> a
v2x (V2 x _) = x

v2y :: V2 a -> a
v2y (V2 _ y) = y

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs (tail xs)

boundsOf :: Ord a => [V2 a] -> (V2 a, V2 a)
boundsOf cs = (V2 minX minY, V2 maxX maxY)
  where
    minX = minimum $ map v2x cs
    minY = minimum $ map v2y cs
    maxX = maximum $ map v2x cs
    maxY = maximum $ map v2y cs

sandOutlet :: Coord
sandOutlet = V2 500 0

data TileType = Air | Rock | Sand deriving (Eq, Show)

showTileTypeAsChar :: TileType -> Char
showTileTypeAsChar Air = '.'
showTileTypeAsChar Rock = '#'
showTileTypeAsChar Sand = 'o'

type CaveMap = Array Coord TileType

isBlocked :: Coord -> CaveMap -> Bool
isBlocked c m
  | inRange (bounds m) c = isSolid $ m ! c
  | otherwise = False
  where
    isSolid Rock = True
    isSolid Sand = True
    isSolid _ = False

nextSandPosition :: CaveMap -> Coord -> Coord
nextSandPosition m c = inner (isBlocked (c + V2 (-1) 1) m) (isBlocked (c + V2 0 1) m) (isBlocked (c + V2 1 1) m)
  where
    inner _ False _ = c + V2 0 1
    inner False True _ = c + V2 (-1) 1
    inner True True False = c + V2 1 1
    inner True True True = c

sandRestingPosition :: CaveMap -> Coord -> Maybe Coord
sandRestingPosition m c
  | c == c' = Just c
  | inRange (bounds m) c' = sandRestingPosition m c'
  | otherwise = Nothing
  where
    c' = nextSandPosition m c

iterateUntilEqual :: Eq a => (a -> a) -> a -> [a]
iterateUntilEqual f x
  | x == x' = []
  | otherwise = x' : iterateUntilEqual f x'
  where
    x' = f x

sandIteration :: CaveMap -> CaveMap
sandIteration m = case sandRestingPosition m sandOutlet of
  Nothing -> m
  Just sandCoord -> m // [(sandCoord, Sand)]

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let stoneCoords = S.fromList $ concatMap (concatMap (uncurry lineBetween) . pairwise) input

      bounds' = boundsOf $ sandOutlet : S.toList stoneCoords

      theMap :: CaveMap
      theMap = defaultArray bounds' Air // map (,Rock) (S.toList stoneCoords)

      theFinalMap :: CaveMap
      theFinalMap = last $ iterateUntilEqual sandIteration theMap

  -- print input
  -- putStrLn $ showArray showTileTypeAsChar theFinalMap
  print $ count (== Sand) theFinalMap

expandBounds :: Num a => V2 a -> V2 a -> (V2 a, V2 a) -> (V2 a, V2 a)
expandBounds minV maxV (min', max') = (min' + minV, max' + maxV)

withBounds :: Ix i => (i, i) -> a -> Array i a -> Array i a
withBounds newBounds e arr = defaultArray newBounds e // assocs arr

mapBounds :: Ix i => ((i, i) -> (i, i)) -> a -> Array i a -> Array i a
mapBounds fBounds e arr = withBounds (fBounds (bounds arr)) e arr

putFloor :: CaveMap -> CaveMap
putFloor m = m // map (,Rock) floorCoords
  where
    (V2 minX _, V2 maxX maxY) = bounds m
    bottomLeftmostCorner = V2 minX maxY
    bottomRightmostCorner = V2 maxX maxY
    floorCoords = lineBetween bottomLeftmostCorner bottomRightmostCorner

expandMapIfNeeded :: CaveMap -> CaveMap
expandMapIfNeeded m
  | needsExpansion = putFloor $ mapBounds (expandBounds (V2 (-1) 0) (V2 1 0)) Air m
  | otherwise = m
  where
    needsExpansion :: Bool
    needsExpansion = elem Sand $ map (m !) edges

    (V2 minX minY, V2 maxX maxY) = bounds m
    edges = range (V2 minX minY, V2 (minX + 1) maxY) ++ range (V2 (maxX - 1) minY, V2 maxX maxY)

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let stoneCoords = S.fromList $ concatMap (concatMap (uncurry lineBetween) . pairwise) input

      bounds' = expandBounds (V2 (-2) 0) (V2 2 2) $ boundsOf $ sandOutlet : S.toList stoneCoords

      theMap :: CaveMap
      theMap = putFloor $ defaultArray bounds' Air // map (,Rock) (S.toList stoneCoords)

      theMaps :: [CaveMap]
      theMaps = iterateUntilEqual (expandMapIfNeeded . sandIteration) theMap

      theFinalMap = last theMaps

  -- putStrLn $ showArray showTileTypeAsChar theFinalMap
  print $ count (== Sand) theFinalMap
