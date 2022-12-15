{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}

module Day15 where

import Common (Parser, lexeme, parseFromFile, the)
import Control.Monad (void)
import Data.Ix (Ix (inRange, range))
import qualified Data.Set as S
import GHC.Generics (Generic)
import Linear (V2 (V2))
import qualified Linear.V2 as V
import Optics (Lens, each, lensVL, mapped, over, view, (%))
import Text.Megaparsec (MonadParsec (eof), sepEndBy1)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

type Coord = V2 Integer

_x :: Lens (V2 a) (V2 a) a a
_x = lensVL V._x

_y :: Lens (V2 a) (V2 a) a a
_y = lensVL V._y

data Sensor = Sensor {position :: Coord, closestBeacon :: Coord} deriving (Eq, Show, Generic)

positionParser :: Parser Coord
positionParser = do
  void $ lexeme $ char 'x'
  void $ lexeme $ char '='
  x <- lexeme $ signed (pure ()) decimal
  void $ lexeme $ char ','
  void $ lexeme $ char 'y'
  void $ lexeme $ char '='
  y <- lexeme $ signed (pure ()) decimal
  return $ V2 x y

sensorParser :: Parser Sensor
sensorParser = do
  void $ lexeme $ string "Sensor"
  void $ lexeme $ string "at"
  position' <- positionParser
  void $ lexeme $ string ":"

  void $ lexeme $ string "closest"
  void $ lexeme $ string "beacon"
  void $ lexeme $ string "is"
  void $ lexeme $ string "at"
  Sensor position' <$> positionParser

inputParser :: Parser [Sensor]
inputParser = sepEndBy1 sensorParser newline

manhattanMagnitude :: (Foldable t, Num a, Functor t) => t a -> a
manhattanMagnitude = sum . fmap abs

relativeRangeAtRelativeYLevel :: Integer -> Integer -> Maybe (Integer, Integer)
relativeRangeAtRelativeYLevel maxDist relY
  | abs relY > maxDist = Nothing
  | otherwise = Just (-(maxDist - abs relY), maxDist - abs relY)

rangeAtYLevel :: Integer -> Coord -> Integer -> Maybe (Integer, Integer)
rangeAtYLevel targetY (V2 x y) maxDist =
  over (mapped % each) (+ x) $ relativeRangeAtRelativeYLevel maxDist (targetY - y)

maybeRangeToSet :: Maybe (Integer, Integer) -> S.Set Integer
maybeRangeToSet = maybe S.empty (S.fromList . range)

sensorPositionAndManhattanDistance :: Sensor -> (Coord, Integer)
sensorPositionAndManhattanDistance s =
  let p = view #position s
      bP = view #closestBeacon s
   in (p, manhattanMagnitude (bP - p))

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let targetY = 10
      -- targetY = 2_000_000

      xs = foldMap (maybeRangeToSet . uncurry (rangeAtYLevel targetY) . sensorPositionAndManhattanDistance) input

      beaconXToExclude = S.fromList $ map (view _x) $ filter ((== targetY) . view _y) $ map (view #closestBeacon) input

  print $ S.size (xs `S.difference` beaconXToExclude)

rotateCounterClockwise90 :: Num a => V2 a -> V2 a
rotateCounterClockwise90 (V2 x y) = V2 (-y) x

quarterRotations :: V2 Integer -> [V2 Integer]
quarterRotations = take 4 . iterate rotateCounterClockwise90

manhattanBoundary :: Integer -> [Coord]
manhattanBoundary 0 = pure 0
manhattanBoundary dist = concatMap quarterRotations $ [V2 i (dist - i) | i <- [0 .. dist - 1]]

manhattanBoundaryOfCoord :: Coord -> Integer -> [Coord]
manhattanBoundaryOfCoord v dist = map (+ v) $ manhattanBoundary dist

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let go :: Sensor -> [Coord]
      go s = manhattanBoundaryOfCoord p (dist + 1)
        where
          (p, dist) = sensorPositionAndManhattanDistance s

      isOutsideRange :: Coord -> (Coord, Integer) -> Bool
      isOutsideRange v (p, dist) = manhattanMagnitude (v - p) > dist

      isInsideField :: Coord -> Bool
      isInsideField (V2 x y) = inRange (0, 4_000_000) x && inRange (0, 4_000_000) y
      -- isInsideField (V2 x y) = inRange (0, 20) x && inRange (0, 20) y

      tuningFrequency (V2 x y) = x * 4_000_000 + y

      solution = the $ filter (\p -> isInsideField p && all (isOutsideRange p . sensorPositionAndManhattanDistance) input) $ foldMap go input

  print $ tuningFrequency <$> solution
