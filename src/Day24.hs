{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day24 (part1, part2, debugPrintSolution) where

import Common (Parser, parseFromFile)
import Common.Array (mk2dArray, showArray, (!?))
import Control.DeepSeq (NFData, force)
import Control.Monad (forM_, guard, join, when)
import Control.Monad.ST (ST)
import Control.Monad.ST.Strict (runST)
import qualified Data.Array as A
import Data.Bifunctor (second)
import Data.Char (intToDigit)
import Data.Foldable (find)
import Data.Functor ((<&>))
import qualified Data.HashPSQ as P
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe, maybeToList)
import Data.STRef (STRef)
import Data.STRef.Strict (modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Set as S
import Day12 (whileJustM_)
import GHC.Generics (Generic)
import Linear (V2 (V2), distance, quadrance)
import Optics (over, view)
import Text.Megaparsec (MonadParsec (eof), choice, sepEndBy1, some, (<|>))
import qualified Text.Megaparsec.Char as C

type Coord = V2 Integer

type Facing = V2 Integer

data Cell = Floor | Wall | Blizzard' Facing deriving (Eq, Show)

data Cell' = Floor' | Wall' deriving (Eq, Show)

type PuzzleMap = A.Array Coord Cell'

data Blizzard = Blizzard {position :: Coord, facing :: Facing} deriving (Eq, Show, Generic, NFData)

pattern UpFacing :: (Eq a, Num a) => V2 a
pattern UpFacing = V2 0 (-1)

pattern DownFacing :: (Eq a, Num a) => V2 a
pattern DownFacing = V2 0 1

pattern LeftFacing :: (Eq a, Num a) => V2 a
pattern LeftFacing = V2 (-1) 0

pattern RightFacing :: (Eq a, Num a) => V2 a
pattern RightFacing = V2 1 0

facingToChar :: Facing -> Char
facingToChar UpFacing = '^'
facingToChar DownFacing = 'v'
facingToChar LeftFacing = '<'
facingToChar RightFacing = '>'
facingToChar _ = '?'

facingParser :: Parser Facing
facingParser =
  choice
    [ UpFacing <$ C.char '^',
      DownFacing <$ (C.char 'v' <|> C.char 'V'),
      LeftFacing <$ C.char '<',
      RightFacing <$ C.char '>'
    ]

cellParser :: Parser Cell
cellParser =
  choice
    [ Floor <$ C.char '.',
      Wall <$ C.char '#',
      Blizzard' <$> facingParser
    ]

lineParser :: Parser [Cell]
lineParser = some cellParser

inputParser :: Parser [[Cell]]
inputParser = sepEndBy1 lineParser C.newline <* eof

extractPartsFromInput :: A.Array Coord Cell -> (Coord, Coord, [Blizzard], PuzzleMap)
extractPartsFromInput arr = (startP, endP, blizzards, arr')
  where
    (V2 minX minY, V2 maxX maxY) = A.bounds arr
    startP = fromJust $ find ((== Just Floor) . (arr !?)) [V2 x minY | x <- [minX .. maxX]]
    endP = fromJust $ find ((== Just Floor) . (arr !?)) [V2 x maxY | x <- [minX .. maxX]]

    blizzards :: [Blizzard]
    blizzards = mapMaybe blizzardFromCell $ A.assocs arr

    blizzardFromCell :: (Coord, Cell) -> Maybe Blizzard
    blizzardFromCell (p, Blizzard' dir) = Just $ Blizzard {position = p, facing = dir}
    blizzardFromCell _ = Nothing

    arr' :: PuzzleMap
    arr' = (\c -> if c == Wall then Wall' else Floor') <$> arr

updateBlizzard :: PuzzleMap -> Blizzard -> Blizzard
updateBlizzard m b = over #position (wrapAroundIfNeeded . (+ facing')) b
  where
    facing' = view #facing b

    isEmpty :: Coord -> Bool
    isEmpty p = (m !? p) == Just Floor'

    wrapAroundIfNeeded :: Coord -> Coord
    wrapAroundIfNeeded p
      | not (isEmpty p) = last $ takeWhile isEmpty $ tail $ iterate (subtract facing') p
      | otherwise = p

showPuzzleMap :: PuzzleMap -> Maybe Coord -> [Blizzard] -> String
showPuzzleMap m explorerP bs = showArray id $ fmap cellToChar m A.// fusedBlizzardAssocs A.// maybeToList ((,'E') <$> explorerP)
  where
    blizzardToArrayAssoc :: Blizzard -> (Coord, Char)
    blizzardToArrayAssoc b = (view #position b, facingToChar $ view #facing b)

    blizzardAssocs = map blizzardToArrayAssoc bs

    fusedBlizzardAssocs =
      M.toList $
        M.map (\cs -> if length cs > 1 then intToDigit (length cs) else head cs) $
          M.fromListWith (++) $
            map (second pure) blizzardAssocs

    cellToChar :: Cell' -> Char
    cellToChar Floor' = '.'
    cellToChar Wall' = '#'

precalculateBlizzards :: PuzzleMap -> [Blizzard] -> (Int, M.Map Int (S.Set Coord))
precalculateBlizzards m initialBs = (cycleLength, everyBlizzardStateMap)
  where
    (minB, maxB) = A.bounds m
    V2 w h = maxB - minB - 1
    cycleLength = fromIntegral $ lcm w h
    everyBlizzardState = map (S.fromList . map (view #position)) $ take cycleLength $ iterate (map (updateBlizzard m)) initialBs
    everyBlizzardStateMap = M.fromList $ zip [0 :: Int ..] everyBlizzardState

modifySTRefWithResultMaybe :: STRef s a -> (a -> Maybe (b, a)) -> ST s (Maybe b)
modifySTRefWithResultMaybe ref f = do
  x <- readSTRef ref
  sequence $
    f x <&> \(result, x') -> do
      writeSTRef ref x'
      return result

oneAndFour :: (a, b, c, d) -> (a, d)
oneAndFour (x, _, _, y) = (x, y)

aStarPathFind ::
  forall a c.
  (Hashable a, Ord a, Num c, Ord c) =>
  (a -> [a]) ->
  (a -> Bool) ->
  (a -> a -> c) ->
  (a -> c) ->
  a ->
  Maybe [a]
aStarPathFind neighborsFn isGoal costFn heuristicFn start = runST $ do
  frontierRef <- newSTRef $ P.singleton start 0 ()
  cameFromRef <- newSTRef $ M.singleton start Nothing
  costSoFarRef <- newSTRef $ M.singleton start 0
  goalRef <- newSTRef (Nothing :: Maybe a)

  let popFrontier = modifySTRefWithResultMaybe frontierRef (fmap oneAndFour . P.minView)

      followMap :: a -> M.Map a (Maybe a) -> [a]
      followMap from map' = catMaybes $ takeWhile isJust $ iterate (>>= join . (`M.lookup` map')) (Just from)

  whileJustM_ popFrontier $ \x ->
    if isGoal x
      then do
        writeSTRef goalRef $ Just x
        writeSTRef frontierRef P.empty
      else do
        forM_ (neighborsFn x) $ \next -> do
          costSoFar <- readSTRef costSoFarRef
          let newCost = costFn x next + fromMaybe 0 (M.lookup x costSoFar)
          when (maybe True (newCost <) $ M.lookup next costSoFar) $ do
            modifySTRef' costSoFarRef $ M.insert next newCost

            let priority = newCost + heuristicFn next
            modifySTRef' frontierRef $ P.insert next priority ()

            modifySTRef' cameFromRef $ M.insert next $ Just x

  mGoal <- readSTRef goalRef
  case mGoal of
    Nothing -> return Nothing
    Just goal -> do
      Just . reverse . followMap goal <$> readSTRef cameFromRef

-- testNeighbors :: PuzzleMap -> Coord -> [Coord]
-- testNeighbors m p = mapMaybe f [UpFacing, DownFacing, LeftFacing, RightFacing]
--   where
--     f :: Facing -> Maybe Coord
--     f dir = do
--       x <- m !? (p + dir)
--       guard $ x == Floor'
--       return $ p + dir

squaredDistance :: Coord -> Coord -> Int
squaredDistance a b = fromInteger $ quadrance (a - b)

type BlizzardNode = (Coord, Int)

puzzleNeighbors :: PuzzleMap -> (BlizzardNode -> Bool) -> BlizzardNode -> [BlizzardNode]
puzzleNeighbors m isOpen (p, minute) = mapMaybe f [0, UpFacing, DownFacing, LeftFacing, RightFacing]
  where
    f :: Facing -> Maybe BlizzardNode
    f dir = do
      x <- m !? (p + dir)
      guard $ x == Floor'
      let x' = (p + dir, minute + 1)
      guard (isOpen x')
      return x'

debugPrintSolution :: PuzzleMap -> Coord -> [Blizzard] -> [BlizzardNode] -> IO ()
debugPrintSolution map' startP initialBs path = do
  let moveToString UpFacing = "move up"
      moveToString DownFacing = "move down"
      moveToString LeftFacing = "move left"
      moveToString RightFacing = "move right"
      moveToString 0 = "wait"
      moveToString _ = "???"
      moves = zipWith (\(p, _) (p', minute) -> (minute, p', moveToString (p' - p))) path (tail path)

      bss = iterate (map (updateBlizzard map')) initialBs

  putStrLn "Initial state:"
  putStrLn $ showPuzzleMap map' (Just startP) (head bss)
  putStrLn ""

  forM_ moves $ \(minute, p, move) -> do
    putStrLn $ "Minute " ++ show minute ++ ", " ++ move ++ ":"
    putStrLn $ showPuzzleMap map' (Just p) (bss !! minute)
    putStrLn ""

pathFind :: PuzzleMap -> (BlizzardNode -> Bool) -> BlizzardNode -> Coord -> Maybe [BlizzardNode]
pathFind map' isOpen startP endP = aStarPathFind neighborsFn isGoalFn costFn heuristicFn startP
  where
    neighborsFn = puzzleNeighbors map' isOpen

    isGoalFn = (== endP) . fst

    costFn (a, aMinute) (b, bMinute) = fromIntegral @_ @Double $ squaredDistance a b + (bMinute - aMinute)

    endP' = fmap fromInteger endP
    heuristicFn (p, _) = distance endP' (fmap fromInteger p)

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile inputParser inputPath

  let input' = fromMaybe (error "unable to turn input into array") $ mk2dArray input
      (startP, endP, initialBs, map') = extractPartsFromInput input'
      !(cycleLength, bsMap) = force $ precalculateBlizzards map' initialBs

      isOpen :: BlizzardNode -> Bool
      isOpen (p, minute) = S.notMember p $ bsMap M.! (minute `mod` cycleLength)

  let !path = fromJust $ pathFind map' isOpen (startP, 0) endP

  print $ snd $ last path

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile inputParser inputPath

  let input' = fromMaybe (error "unable to turn input into array") $ mk2dArray input
      (startP, endP, initialBs, map') = extractPartsFromInput input'
      !(cycleLength, bsMap) = force $ precalculateBlizzards map' initialBs

      isOpen :: BlizzardNode -> Bool
      isOpen (p, minute) = S.notMember p $ bsMap M.! (minute `mod` cycleLength)

  let firstTripEndP = last $ fromJust $ pathFind map' isOpen (startP, 0) endP
      secondTripEndP = last $ fromJust $ pathFind map' isOpen firstTripEndP startP
      thirdTripEndP = last $ fromJust $ pathFind map' isOpen secondTripEndP endP

  print $ snd thirdTripEndP
