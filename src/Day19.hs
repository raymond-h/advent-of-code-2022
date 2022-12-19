{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}

module Day19 (part1, part2) where

import Common (Parser, parseFromFile)
import Control.Monad (forM_, guard, void)
import Control.Monad.State.Strict (MonadState (get), execState, execStateT)
import Control.Parallel.Strategies (NFData)
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Monoid (Any (Any), getAny)
import Data.Ord (Down (Down), comparing)
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import GHC.Generics (Generic)
import Optics (assign', at, modifying', non, over, set, use, view, (%))
import Text.Megaparsec (choice, eof, many, manyTill, sepBy1)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as CL

data ResourceType = Ore | Clay | Obsidian | Geode deriving (Eq, Ord, Show, Generic, NFData)

data Recipe = Recipe {robotType :: ResourceType, craftingInputs :: [(Int, ResourceType)]} deriving (Eq, Show, Generic)

data Blueprint = Blueprint {identifier :: Integer, recipes :: [Recipe]} deriving (Eq, Show, Generic)

lexeme :: Parser a -> Parser a
lexeme = CL.lexeme C.space

decimal :: Num a => Parser a
decimal = lexeme CL.decimal

string :: LT.Text -> Parser LT.Text
string = lexeme . C.string

resourceTypeParser :: Parser ResourceType
resourceTypeParser =
  choice
    [ Ore <$ string "ore",
      Clay <$ string "clay",
      Obsidian <$ string "obsidian",
      Geode <$ string "geode"
    ]

recipeParser :: Parser Recipe
recipeParser = do
  let craftingInputParser = (,) <$> decimal <*> resourceTypeParser

  void $ string "Each"
  robotType' <- resourceTypeParser
  mapM_ string ["robot", "costs"]
  craftingInputs' <- sepBy1 craftingInputParser (string "and")
  void $ string "."
  return $ Recipe robotType' craftingInputs'

blueprintParser :: Parser Blueprint
blueprintParser = do
  void $ string "Blueprint"
  identifier' <- decimal
  void $ string ":"
  recipes' <- many recipeParser
  return $ Blueprint identifier' recipes'

inputParser :: Parser [Blueprint]
inputParser = manyTill blueprintParser eof

type RecipeMap = M.Map ResourceType [(Int, ResourceType)]

recipeMapFromBlueprint :: Blueprint -> RecipeMap
recipeMapFromBlueprint bp = M.fromList $ map (\r -> (robotType r, craftingInputs r)) $ recipes bp

data ProblemState = ProblemState
  { pendingRobot :: Maybe ResourceType,
    currentRobotCounts :: M.Map ResourceType Int,
    currentResourceAmounts :: M.Map ResourceType Int
  }
  deriving (Eq, Ord, Show, Generic, NFData)

initialProblemState :: ProblemState
initialProblemState =
  ProblemState
    { pendingRobot = Nothing,
      currentRobotCounts = M.singleton Ore 1,
      currentResourceAmounts = M.empty
    }

craftRecipe :: RecipeMap -> ProblemState -> ResourceType -> Maybe ProblemState
craftRecipe rs s rt = flip execStateT s $ do
  let recipeInputs = rs M.! rt

      haveEnoughResourceOfType :: ProblemState -> Int -> ResourceType -> Bool
      haveEnoughResourceOfType s' n rt' = n <= view (#currentResourceAmounts % at rt' % non 0) s'

  s' <- get
  guard $ all (uncurry (haveEnoughResourceOfType s')) recipeInputs

  assign' #pendingRobot $ Just rt

  forM_ recipeInputs $ \(n, rt') -> do
    modifying' (#currentResourceAmounts % at rt' % non 0) (subtract n)

robotCollecting :: ProblemState -> ProblemState
robotCollecting = execState $ do
  currentRobots <- M.toList <$> use #currentRobotCounts
  forM_ currentRobots $ \(rt, n) -> do
    modifying' (#currentResourceAmounts % at rt % non 0) (+ n)

finishPendingRobot :: ProblemState -> ProblemState
finishPendingRobot s = case view #pendingRobot s of
  Nothing -> s
  Just rt -> set #pendingRobot Nothing $ over (#currentRobotCounts % at rt % non 0) (+ 1) s

possibleOptions :: RecipeMap -> ProblemState -> [ProblemState]
possibleOptions rs s =
  map (finishPendingRobot . robotCollecting) $
    disallowNotCraftingIfCanCraftAllRobots $
      mapMaybe (craftRecipe rs s) [Ore, Clay, Obsidian, Geode]
  where
    disallowNotCraftingIfCanCraftAllRobots :: [ProblemState] -> [ProblemState]
    disallowNotCraftingIfCanCraftAllRobots ss
      | length ss == 4 = ss
      | otherwise = s : ss

maxNeededForCrafting :: RecipeMap -> ResourceType -> Int
maxNeededForCrafting rs rt = maximum $ map (sum . mapMaybe f) $ M.elems rs
  where
    f :: (Int, ResourceType) -> Maybe Int
    f (n, rt') = if rt == rt' then Just n else Nothing

hasMoreRobotsThanNeeded :: RecipeMap -> ProblemState -> Any
hasMoreRobotsThanNeeded rs s = Any $ any go [Ore, Clay, Obsidian]
  where
    go :: ResourceType -> Bool
    go rt = view (#currentRobotCounts % at rt % non 0) s > maxNeededForCrafting rs rt

isPruneable :: RecipeMap -> ProblemState -> Bool
isPruneable rs s = getAny $ p rs s
  where
    p = hasMoreRobotsThanNeeded

keepBest :: Int -> [ProblemState] -> [ProblemState]
keepBest n = take n . sortBy comparator
  where
    comparator :: ProblemState -> ProblemState -> Ordering
    comparator =
      comparing (Down . resourceCount Geode)
        <> comparing (Down . robotCount Geode)
        <> comparing (Down . resourceCount Obsidian)
        <> comparing (Down . robotCount Obsidian)
        <> comparing (Down . resourceCount Clay)
        <> comparing (Down . robotCount Clay)
        <> comparing (Down . resourceCount Ore)
        <> comparing (Down . robotCount Ore)

    resourceCount :: ResourceType -> ProblemState -> Int
    resourceCount rt = view (#currentResourceAmounts % at rt % non 0)

    robotCount :: ResourceType -> ProblemState -> Int
    robotCount rt = view (#currentRobotCounts % at rt % non 0)

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile inputParser inputPath

  let maxGeodeCount :: Blueprint -> Int
      maxGeodeCount (recipeMapFromBlueprint -> rs) = maxGeodes
        where
          maxGeodes = maximum $ map (view (#currentResourceAmounts % at Geode % non 0)) $ last ss
          ss :: [[ProblemState]]
          ss = take 25 $ iterate (keepBest 1_000 . nub . (>>= filter (not . isPruneable rs) . possibleOptions rs)) (return initialProblemState)

      allMaxGeodeCounts = map (\bp -> (identifier bp, toInteger $ maxGeodeCount bp)) input

  print $ sum $ map (uncurry (*)) allMaxGeodeCounts

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- take 3 <$> parseFromFile inputParser inputPath

  let maxGeodeCount :: Blueprint -> Int
      maxGeodeCount (recipeMapFromBlueprint -> rs) = maxGeodes
        where
          maxGeodes = maximum $ map (view (#currentResourceAmounts % at Geode % non 0)) $ last ss
          ss :: [[ProblemState]]
          ss = take 33 $ iterate (keepBest 1_000 . nub . (>>= filter (not . isPruneable rs) . possibleOptions rs)) (return initialProblemState)

      allMaxGeodeCounts = map (\bp -> (identifier bp, toInteger $ maxGeodeCount bp)) input

  print $ product $ map snd allMaxGeodeCounts
