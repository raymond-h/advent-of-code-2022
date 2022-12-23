{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Day23 where

import Common (Parser, char, count, parseFromFile, tuplify, untuplify)
import Common.Array (defaultArray, mk2dArray)
import Control.Monad (guard)
import qualified Data.Array as A
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Linear (V2 (V2))
import Optics (each, over, sequenceOf, _2)
import Text.Megaparsec (MonadParsec (eof), choice, sepEndBy1, some)
import Text.Megaparsec.Char (newline)

type Coord = V2 Integer

type Facing = V2 Integer

data Cell = Floor | Elf deriving (Eq, Show)

cellToChar :: Cell -> Char
cellToChar Floor = '.'
cellToChar Elf = '#'

type PuzzleMap = M.Map Coord ()

findBounds :: [Coord] -> (Coord, Coord)
findBounds ps = (untuplify mins, untuplify maxs)
  where
    components = unzip $ map tuplify ps
    mins = over each minimum components
    maxs = over each maximum components

puzzleMapToArray :: PuzzleMap -> A.Array Coord Cell
puzzleMapToArray m = defaultArray bounds Floor A.// map (,Elf) (M.keys m)
  where
    bounds = findBounds $ M.keys m

at :: PuzzleMap -> Coord -> Cell
at m p = maybe Floor (const Elf) $ M.lookup p m

expandBoundsBy1 :: a -> A.Array Coord a -> A.Array Coord a
expandBoundsBy1 x arr = defaultArray newBounds x A.// A.assocs arr
  where
    (minB, maxB) = A.bounds arr

    newBounds :: (Coord, Coord)
    newBounds = (minB - 1, maxB + 1)

pattern North :: (Eq a, Num a) => V2 a
pattern North = V2 0 (-1)

pattern South :: (Eq a, Num a) => V2 a
pattern South = V2 0 1

pattern West :: (Eq a, Num a) => V2 a
pattern West = V2 (-1) 0

pattern East :: (Eq a, Num a) => V2 a
pattern East = V2 1 0

pattern NorthEast :: (Eq a, Num a) => V2 a
pattern NorthEast = V2 1 (-1)

pattern NorthWest :: (Eq a, Num a) => V2 a
pattern NorthWest = V2 (-1) (-1)

pattern SouthEast :: (Eq a, Num a) => V2 a
pattern SouthEast = V2 1 1

pattern SouthWest :: (Eq a, Num a) => V2 a
pattern SouthWest = V2 (-1) 1

allDirections :: [Facing]
allDirections = [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]

cellParser :: Parser Cell
cellParser = choice [Floor <$ char '.', Elf <$ char '#']

rowParser :: Parser [Cell]
rowParser = some cellParser

inputParser :: Parser [[Cell]]
inputParser = sepEndBy1 rowParser newline <* eof

none :: Foldable t => (a -> Bool) -> t a -> Bool
none f xs = not $ any f xs

data ProposalDirectionConsideration = ConsiderNorth | ConsiderSouth | ConsiderWest | ConsiderEast
  deriving (Eq, Show, Generic)

initialProposalDirectionConsiderations :: [ProposalDirectionConsideration]
initialProposalDirectionConsiderations = [ConsiderNorth, ConsiderSouth, ConsiderWest, ConsiderEast]

proposeStep :: PuzzleMap -> [ProposalDirectionConsideration] -> Coord -> Maybe Coord
proposeStep arr pdcs p
  | none isElf allDirections = Just 0
  | otherwise = Just $ head $ catMaybes subProposals
  where
    isElf :: Facing -> Bool
    isElf dir = (== Elf) $ arr `at` (p + dir)

    subProposals = map evaluatePDC pdcs ++ [Just 0]

    evaluatePDC :: ProposalDirectionConsideration -> Maybe Coord
    evaluatePDC ConsiderNorth = guard (none isElf [North, NorthEast, NorthWest]) >> return (p + North)
    evaluatePDC ConsiderSouth = guard (none isElf [South, SouthEast, SouthWest]) >> return (p + South)
    evaluatePDC ConsiderWest = guard (none isElf [West, NorthWest, SouthWest]) >> return (p + West)
    evaluatePDC ConsiderEast = guard (none isElf [East, NorthEast, SouthEast]) >> return (p + East)

swapMaybeTuple :: (a, Maybe b) -> Maybe (a, b)
swapMaybeTuple = sequenceOf _2

filterProposalsForSamePosition :: [(Coord, Coord)] -> [(Coord, Coord)]
filterProposalsForSamePosition proposal =
  map swap $
    mapMaybe (swapMaybeTuple . over _2 single) $
      M.toList $
        M.fromListWith (++) $
          map (over _2 pure . swap) proposal
  where
    single :: [a] -> Maybe a
    single [x] = Just x
    single _ = Nothing

runStep :: PuzzleMap -> [ProposalDirectionConsideration] -> (PuzzleMap, [ProposalDirectionConsideration])
runStep map' pdcs = (map'', tail pdcs ++ [head pdcs])
  where
    proposal = mapMaybe (\i -> (i,) <$> proposeStep map' pdcs i) $ M.keys map'
    proposalMap = M.fromList $ filterProposalsForSamePosition proposal
    map'' = M.mapKeys (\p -> fromMaybe p (M.lookup p proposalMap)) map'

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile inputParser inputPath

  let inputArr = fromMaybe (error "unable to turn input into 2D array") $ mk2dArray input

      input' :: PuzzleMap
      input' = M.fromList $ map (,()) $ filter ((== Elf) . (inputArr A.!)) $ A.indices inputArr

      ss = take 11 $ map fst $ iterate (uncurry runStep) (input', initialProposalDirectionConsiderations)

      mapArr = puzzleMapToArray $ last ss

  print $ count (== Floor) mapArr

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile inputParser inputPath

  let inputArr = fromMaybe (error "unable to turn input into 2D array") $ mk2dArray input

      input' :: PuzzleMap
      input' = M.fromList $ map (,()) $ filter ((== Elf) . (inputArr A.!)) $ A.indices inputArr

      ss = map fst $ iterate (uncurry runStep) (input', initialProposalDirectionConsiderations)

      ss' = mapMaybe (\(i', a, b) -> if a == b then Just (i', a) else Nothing) $ zip3 [1 :: Integer ..] ss (tail ss)

  print $ fst $ head ss'
