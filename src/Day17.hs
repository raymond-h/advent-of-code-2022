{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}

module Day17 (part1, part2, showChamberMap) where

import Common (Parser, char, parseFromFile, the)
import Common.Array (mk2dArrayWithBasis, showArray)
import Control.Monad.ST (runST)
import Control.Monad.State.Strict (MonadState, evalState, execState, gets, replicateM)
import qualified Data.Array as A
import Data.Bool (bool)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.STRef (modifySTRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import Day12 (defaultArray, whileM_)
import Linear (V2 (V2))
import Optics (assign, modifying, use, view, _1, _2, _3)
import Text.Megaparsec (choice, some)
import Text.Megaparsec.Char (newline)

data JetDirection = JetLeft | JetRight deriving (Eq, Show)

jetDirectionParser :: Parser JetDirection
jetDirectionParser = choice [JetLeft <$ char '<', JetRight <$ char '>']

inputParser :: Parser [JetDirection]
inputParser = some jetDirectionParser <* newline

type RockShape = A.Array (V2 Int) Bool

rockShapeFromAscii :: [LT.Text] -> RockShape
rockShapeFromAscii lines' = fromJust $ mk2dArrayWithBasis (pure 0) $ map (map asciiToBool . LT.unpack) lines'
  where
    asciiToBool '#' = True
    asciiToBool '.' = False
    asciiToBool c = error $ "Unknown value for rock shape: " ++ [c]

rockShapeToVertices :: RockShape -> [V2 Int]
rockShapeToVertices rs = map fst $ filter snd $ A.assocs rs

rockShapes :: [RockShape]
rockShapes =
  [ rockShapeFromAscii
      [ "####"
      ],
    rockShapeFromAscii
      [ ".#.",
        "###",
        ".#."
      ],
    rockShapeFromAscii
      [ "..#",
        "..#",
        "###"
      ],
    rockShapeFromAscii
      [ "#",
        "#",
        "#",
        "#"
      ],
    rockShapeFromAscii
      [ "##",
        "##"
      ]
  ]

type ChamberMap = M.Map (V2 Int) Bool

chamberMapToArray :: ChamberMap -> A.Array (V2 Int) Bool
chamberMapToArray m = defaultArray bounds False A.// M.toList m
  where
    bounds :: (V2 Int, V2 Int)
    bounds = (V2 0 (highestRockYLevel m), V2 6 0)

showChamberMap :: ChamberMap -> String
showChamberMap m = showArray (bool '.' '#') $ chamberMapToArray m

chamberHorizontalBounds :: (Int, Int)
chamberHorizontalBounds = (0, 6)

isPointOccupied :: V2 Int -> ChamberMap -> Bool
isPointOccupied p@(V2 x y) m = y > 0 || not (A.inRange chamberHorizontalBounds x) || M.lookup p m == Just True

isRockColliding :: RockShape -> V2 Int -> ChamberMap -> Bool
isRockColliding rs offset m = any (`isPointOccupied` m) relevantPositions
  where
    relevantPositions :: [V2 Int]
    relevantPositions = map (+ offset) $ rockShapeToVertices rs

highestRockYLevel :: ChamberMap -> Int
highestRockYLevel m = maybe 0 minimum $ nonEmpty $ map ((\(V2 _ y) -> y - 1) . fst) $ filter snd $ M.toList m

rockSpawnOffset :: RockShape -> ChamberMap -> V2 Int
rockSpawnOffset rs m = V2 2 (settledY - 3 - rockBottomY)
  where
    settledY :: Int
    settledY = highestRockYLevel m

    rockBottomY :: Int
    (_, V2 _ rockBottomY) = A.bounds rs

jetToVector :: Num a => JetDirection -> V2 a
jetToVector JetLeft = V2 (-1) 0
jetToVector JetRight = V2 1 0

v2Down :: V2 Int
v2Down = V2 0 1

rockSettleOffset :: RockShape -> [(Int, JetDirection)] -> ChamberMap -> (V2 Int, [(Int, JetDirection)])
rockSettleOffset rockShape jets m = runST $ do
  jetsRef <- newSTRef jets
  offsetRef <- newSTRef $ rockSpawnOffset rockShape m
  doneRef <- newSTRef False

  let popJet = do
        (_, jet) <- head <$> readSTRef jetsRef
        modifySTRef jetsRef tail
        return jet

      moveByJet jetV offset =
        if isRockColliding rockShape (offset + jetV) m
          then offset
          else offset + jetV

  whileM_ (not <$> readSTRef doneRef) $ do
    jetV <- jetToVector <$> popJet
    modifySTRef' offsetRef (moveByJet jetV)

    offset <- readSTRef offsetRef
    if isRockColliding rockShape (offset + v2Down) m
      then writeSTRef doneRef True
      else writeSTRef offsetRef $ offset + v2Down

  (,) <$> readSTRef offsetRef <*> readSTRef jetsRef

putRock :: [V2 Int] -> ChamberMap -> ChamberMap
putRock ps m = foldl go m ps
  where
    go :: ChamberMap -> V2 Int -> ChamberMap
    go m' p = M.insert p True m'

putRockShape :: RockShape -> V2 Int -> ChamberMap -> ChamberMap
putRockShape rockShape offset = putRock ps
  where
    ps :: [V2 Int]
    ps = map (+ offset) $ rockShapeToVertices rockShape

type RockFallState = ([(Int, RockShape)], [(Int, JetDirection)], ChamberMap)

rockFallLoop :: MonadState RockFallState m => m ()
rockFallLoop = do
  (_, rock) <- head <$> use _1
  modifying _1 tail

  input <- use _2
  chamberMap <- use _3

  let (offset, input') = rockSettleOffset rock input chamberMap

  let chamberMap' = putRockShape rock offset chamberMap

  assign _2 input'
  assign _3 chamberMap'

cycleWithIndex :: [a] -> [(Int, a)]
cycleWithIndex xs = cycle $ zip [0 ..] xs

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile inputParser inputPath

  let chamberMap :: ChamberMap
      chamberMap = M.empty

      (_, _, chamberMap') = execState (replicateM 2_022 rockFallLoop) (cycleWithIndex rockShapes, cycleWithIndex input, chamberMap)

  print $ -(highestRockYLevel chamberMap')

fingerprintChamberMap :: ChamberMap -> [Int]
fingerprintChamberMap m = map (subtract highestY) denormalizedFingerprint
  where
    highestY = minimum denormalizedFingerprint
    denormalizedFingerprint = map (highestYPerX M.!) [0 .. 6]

    defaultHighestYPerX :: M.Map Int Int
    defaultHighestYPerX = M.fromList $ map (,0) [0 .. 6]

    highestYPerX :: M.Map Int Int
    highestYPerX = M.union (M.fromListWith min $ map (\(V2 x y, _) -> (x, y - 1)) $ filter snd $ M.toList m) defaultHighestYPerX

type StateFingerprint = (Int, Int, [Int])

fingerprintState :: RockFallState -> StateFingerprint
fingerprintState s = (curRockIdx, curJetIdx, f)
  where
    f = fingerprintChamberMap $ view _3 s
    (curRockIdx, _) = head $ view _1 s
    (curJetIdx, _) = head $ view _2 s

findFirstDuplicate' :: Ord a => S.Set a -> Int -> [a] -> Maybe (Int, a)
findFirstDuplicate' _ _ [] = Nothing
findFirstDuplicate' seen i (x : xs)
  | S.member x seen = Just (i, x)
  | otherwise = findFirstDuplicate' (S.insert x seen) (i + 1) xs

findFirstDuplicate :: Ord a => [a] -> Maybe (Int, a)
findFirstDuplicate = findFirstDuplicate' S.empty 0

targetIteration :: Int
targetIteration = 1_000_000_000_000

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile inputParser inputPath

  let chamberMap :: ChamberMap
      chamberMap = M.empty

      loop = replicateM 10_000 $ do
        rockFallLoop

        (,) <$> gets fingerprintState <*> use _3

      res = evalState loop (cycleWithIndex rockShapes, cycleWithIndex input, chamberMap)

      (idx, firstDuplicate) = fromJust $ findFirstDuplicate $ map fst res

      heightAtIdx i = negate $ highestRockYLevel $ snd $ res !! i

      xs = filter ((== firstDuplicate) . fst . snd) $ zip [0 :: Int ..] res

      is, hs :: [Int]
      is = map fst xs
      hs = map (negate . highestRockYLevel . snd . snd) xs

      iDiff = fromJust $ the $ zipWith subtract is (tail is)
      hDiff = fromJust $ the $ zipWith subtract hs (tail hs)

      n = (targetIteration - idx) `div` iDiff

      height = heightAtIdx idx + hDiff * n

      iterationsLeft = targetIteration - iDiff * n - idx

      finalHeightAdded = heightAtIdx (idx + iterationsLeft - 1) - heightAtIdx idx

  print $ height + finalHeightAdded