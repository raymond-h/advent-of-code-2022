{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Day22 (part1, part2, showMap, showMapWithOverlay, withPath) where

import Common (Parser, char, decimal, parseFromFile, tuplify)
import Common.Array (defaultArray, mk2dArray, showArray)
import Control.Monad (join, replicateM_, void, when)
import Control.Monad.State.Strict (MonadState, execStateT)
import Control.Monad.Writer.Strict (MonadWriter, runWriter, tell)
import qualified Data.Array as A
import Data.Foldable (find)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Linear (V2 (V2), perp)
import MagicaVoxelXrawWriter (padToMinimum)
import Optics (allOf, assign', at, each, mapped, modifying', over, to, use, view, (%), _2)
import Text.Megaparsec (MonadParsec (eof, notFollowedBy), choice, lookAhead, many, manyTill, sepBy1, some, try)
import Text.Megaparsec.Char (newline, spaceChar)
import qualified Text.Megaparsec.Char as C

(!?) :: A.Ix i => A.Array i a -> i -> Maybe a
arr !? p
  | A.inRange (A.bounds arr) p = Just $ arr A.! p
  | otherwise = Nothing

type Coord = V2 Integer

type Facing = V2 Integer

pattern UpFacing :: (Eq a, Num a) => V2 a
pattern UpFacing = V2 0 (-1)

pattern DownFacing :: (Eq a, Num a) => V2 a
pattern DownFacing = V2 0 1

pattern LeftFacing :: (Eq a, Num a) => V2 a
pattern LeftFacing = V2 (-1) 0

pattern RightFacing :: (Eq a, Num a) => V2 a
pattern RightFacing = V2 1 0

data Step = Move Integer | TurnClockwise | TurnCounterClockwise deriving (Eq, Show)

data Cell = Open | Wall deriving (Eq, Show)

cellToChar :: Cell -> Char
cellToChar Open = '.'
cellToChar Wall = '#'

stepParser :: Parser Step
stepParser =
  choice
    [ Move <$> decimal,
      TurnClockwise <$ char 'R',
      TurnCounterClockwise <$ char 'L'
    ]

stepsParser :: Parser [Step]
stepsParser = manyTill stepParser (lookAhead newline)

cellParser :: Parser Cell
cellParser =
  choice
    [ Open <$ C.char '.',
      Wall <$ C.char '#'
    ]

mapParser :: Parser [[Maybe Cell]]
mapParser = sepBy1 rowParser (try $ newline >> notFollowedBy newline)
  where
    rowParser :: Parser [Maybe Cell]
    rowParser = (++) <$> many (Nothing <$ spaceChar) <*> some (Just <$> cellParser)

inputParser :: Parser ([[Maybe Cell]], [Step])
inputParser = do
  map' <- mapParser
  void $ newline >> newline
  steps <- stepsParser
  void newline
  eof
  return (map', steps)

mk2dArray' :: [[Maybe a]] -> Maybe (A.Array Coord (Maybe a))
mk2dArray' xss = mk2dArray $ map (padToMinimum maxSubListLength Nothing) xss
  where
    maxSubListLength = maximum $ map length xss

type PasswordMap = A.Array Coord (Maybe Cell)

showMap :: PasswordMap -> String
showMap = showArray (maybe ' ' cellToChar)

type PasswordMapWithOverlay = A.Array Coord (Either Char (Maybe Cell))

showMapWithOverlay :: PasswordMapWithOverlay -> String
showMapWithOverlay = showArray $ either id (maybe ' ' cellToChar)

startingPosition :: PasswordMap -> Maybe Coord
startingPosition m = find (isJust . (m A.!)) potentialPositions
  where
    potentialPositions :: [Coord]
    potentialPositions = takeWhile (A.inRange (A.bounds m)) $ iterate (+ V2 1 0) (pure 1)

data MapWalkState = MapWalkState {position :: Coord, facing :: Facing} deriving (Eq, Show, Generic)

initialMapWalkState :: Coord -> MapWalkState
initialMapWalkState p = MapWalkState p (V2 1 0)

passwordFromMapWalkState :: MapWalkState -> Integer
passwordFromMapWalkState (MapWalkState (V2 column row) dir) = row * 1000 + 4 * column + encodeFacing dir
  where
    encodeFacing :: Facing -> Integer
    encodeFacing (V2 1 0) = 0
    encodeFacing (V2 0 1) = 1
    encodeFacing (V2 (-1) 0) = 2
    encodeFacing (V2 0 (-1)) = 3
    encodeFacing _ = error "unreachable"

type PositionWrapperFunction = Facing -> Coord -> (Coord, Facing)

wrapAroundIfNeeded :: PasswordMap -> PositionWrapperFunction
wrapAroundIfNeeded m dir p
  | isNothing (join (m !? p)) = (p', dir)
  | otherwise = (p, dir)
  where
    p' :: Coord
    p' = last $ takeWhile (isJust . join . (m !?)) $ tail $ iterate (subtract dir) p

tellState :: (MonadState MapWalkState m, MonadWriter [(Coord, Facing)] m) => m ()
tellState = do
  p <- use #position
  dir <- use #facing
  tell [(p, dir)]

runStep :: (MonadState MapWalkState m, MonadWriter [(Coord, Facing)] m) => PasswordMap -> PositionWrapperFunction -> Step -> m ()
runStep m wrapAroundIfNeeded' (Move n) = do
  replicateM_ (fromInteger n) $ do
    dir <- use #facing
    p <- use #position

    let (newP, newDir) = wrapAroundIfNeeded' dir $ p + dir
        cell = join $ m !? newP

    when (cell == Just Open) $ do
      assign' #position newP
      assign' #facing newDir
      tellState
runStep _ _ TurnClockwise = modifying' #facing perp >> tellState
runStep _ _ TurnCounterClockwise = modifying' #facing (perp . perp . perp) >> tellState

part1 :: FilePath -> IO ()
part1 inputPath = do
  (map', steps) <- parseFromFile inputParser inputPath

  let map'' = fromJust $ mk2dArray' map'
      p = fromJust $ startingPosition map''

      runStep' = runStep map'' (wrapAroundIfNeeded map'')

      (finalMapWalkState, _) = runWriter $ execStateT (mapM_ runStep' steps) (initialMapWalkState p)

  print $ passwordFromMapWalkState finalMapWalkState

divisibleBy :: Integral a => a -> a -> Bool
divisibleBy a b = a `rem` b == 0

sideSideLengthTiles :: PasswordMap -> Integer
sideSideLengthTiles m
  | allOf (_2 % to tuplify % each) (`divisibleBy` 50) (A.bounds m) = 50
  | allOf (_2 % to tuplify % each) (`divisibleBy` 4) (A.bounds m) = 4
  | otherwise = error "unknown side length of cube sides, expected 50 or 4"

type CubeNetCoord = V2 Integer

type SidesMap = A.Array CubeNetCoord (Maybe Int)

sidesMap :: PasswordMap -> SidesMap
sidesMap m = defaultArray newBounds Nothing A.// over (mapped % _2) Just entries
  where
    n = sideSideLengthTiles m

    newBounds :: (CubeNetCoord, CubeNetCoord)
    newBounds = over _2 (fmap (`div` n)) $ A.bounds m

    allPositionsToCheck :: [CubeNetCoord]
    allPositionsToCheck = [V2 i j | i <- [1 .. 4], j <- [1 .. 4]]

    entries :: [(CubeNetCoord, Int)]
    entries = flip zip [1 ..] $ sortOn (swap . tuplify) $ filter (isJust . join . (m !?) . fmap (* n)) allPositionsToCheck

type Faces = M.Map Int Face

data Face = Face
  { offset :: V2 Integer,
    newFaces :: M.Map Facing Int,
    rotations :: M.Map Facing Int
  }
  deriving (Eq, Show, Generic)

-- [up, left, down, right]
mkFace :: Integer -> Integer -> [Int] -> [Int] -> Face
mkFace column row newFaces' rotations' = Face (V2 row column) newFaces'' rotations''
  where
    newFaces'' = M.fromList $ zip [UpFacing, LeftFacing, DownFacing, RightFacing] newFaces'
    rotations'' = M.fromList $ zip [UpFacing, LeftFacing, DownFacing, RightFacing] rotations'

-- hardcoded sins, because I can't be bothered to generalize it rn
hardcodedFacesForExampleInput :: Faces
hardcodedFacesForExampleInput =
  M.fromList
    [ (1, mkFace 0 8 [2, 3, 4, 6] [2, 1, 0, 2]),
      (2, mkFace 4 0 [1, 6, 5, 3] [2, 3, 2, 0]),
      (3, mkFace 4 4 [1, 2, 5, 4] [3, 0, 1, 0]),
      (4, mkFace 4 8 [1, 3, 5, 6] [0, 0, 0, 3]),
      (5, mkFace 8 8 [4, 3, 2, 6] [0, 3, 2, 0]),
      (6, mkFace 8 12 [4, 5, 2, 1] [1, 0, 1, 2])
    ]

hardcodedFacesForRealInput :: Faces
hardcodedFacesForRealInput =
  M.fromList
    [ (1, mkFace 0 50 [6, 4, 3, 2] [3, 2, 0, 0]),
      (2, mkFace 0 100 [6, 1, 3, 5] [0, 0, 3, 2]),
      (3, mkFace 50 50 [1, 4, 5, 2] [0, 1, 0, 1]),
      (4, mkFace 100 0 [3, 1, 6, 5] [3, 2, 0, 0]),
      (5, mkFace 100 50 [3, 4, 6, 2] [0, 0, 3, 2]),
      (6, mkFace 150 0 [4, 1, 2, 5] [0, 1, 0, 1])
    ]

applyN :: (a -> a) -> Int -> a -> a
applyN _ 0 = id
applyN f n = applyN f (n - 1) . f

absoluteCoordToFace :: Integer -> SidesMap -> Coord -> Maybe Int
absoluteCoordToFace size sidesMap' p = join $ (sidesMap' !?) $ (+ 1) $ fmap (`div` size) (p - 1)

absoluteCoordToRelativeCoord :: Integer -> Coord -> Coord
absoluteCoordToRelativeCoord size p = 1 + fmap (`mod` size) (p - 1)

wrapAroundIfNeededPart2 :: PasswordMap -> SidesMap -> Faces -> PositionWrapperFunction
wrapAroundIfNeededPart2 m sidesMap' faces dir p
  | isOnSameFace p (p - dir) = (p, dir)
  | otherwise = (p', dir')
  where
    isOnSameFace :: Coord -> Coord -> Bool
    isOnSameFace a b =
      absoluteCoordToFace size sidesMap' a
        == absoluteCoordToFace size sidesMap' b

    size = sideSideLengthTiles m

    rotatePositionOnFace :: Coord -> Coord
    rotatePositionOnFace (V2 row col) = V2 (size - col + 1) row

    currentFaceIndex = fromJust $ absoluteCoordToFace size sidesMap' (p - dir)
    rP = absoluteCoordToRelativeCoord size p

    currentFace = faces M.! currentFaceIndex

    newFaceIndex = fromJust $ view (#newFaces % at dir) currentFace
    newFace = faces M.! newFaceIndex

    rotationCount = fromJust $ view (#rotations % at dir) currentFace

    rPAfterRotation = applyN rotatePositionOnFace (4 - rotationCount) rP

    p' :: Coord
    p' = (+ offset newFace) rPAfterRotation

    dir' :: Coord
    dir' = applyN perp (4 - rotationCount) dir

withPath :: PasswordMap -> [(Coord, Facing)] -> PasswordMapWithOverlay
withPath m facings = fmap Right m A.// over (mapped % _2) (Left . facingToChar) facings
  where
    facingToChar :: Facing -> Char
    facingToChar UpFacing = '^'
    facingToChar DownFacing = 'v'
    facingToChar LeftFacing = '<'
    facingToChar RightFacing = '>'
    facingToChar _ = '?'

part2 :: FilePath -> IO ()
part2 inputPath = do
  (map', steps) <- parseFromFile inputParser inputPath

  let map'' = fromJust $ mk2dArray' map'
      p = fromJust $ startingPosition map''

      sidesMap' = sidesMap map''

      faces = case sideSideLengthTiles map'' of
        50 -> hardcodedFacesForRealInput
        4 -> hardcodedFacesForExampleInput
        n -> error $ "the given side length for cube sides is unsupported: " ++ show n

      runStep' = runStep map'' (wrapAroundIfNeededPart2 map'' sidesMap' faces)

      (finalMapWalkState, _) = runWriter $ execStateT (mapM_ runStep' steps) (initialMapWalkState p)

  print $ passwordFromMapWalkState finalMapWalkState
