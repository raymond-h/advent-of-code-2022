{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day11 where

import Common (Parser, lexeme, parseFromFile, sepBy1NonGreedy)
import Control.Monad (forM_, replicateM_, void)
import Control.Monad.RWS.Strict (MonadReader, MonadState, asks, execRWS, gets, tell)
import Data.Foldable (toList)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Monoid (Endo (Endo))
import Data.Ord (Down (Down))
import Debug.Trace (traceM)
import GHC.Generics (Generic)
import Optics
  ( At (at),
    assign,
    modifying',
    to,
    use,
    view,
    (%),
    _Just,
  )
import Text.Megaparsec (choice, eof, try)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

data MonkeyOperation
  = Add Integer
  | Multiply Integer
  | MultiplyOld
  deriving (Eq, Show)

evaluateMonkeyOperation :: MonkeyOperation -> Integer -> Integer
evaluateMonkeyOperation (Add m) n = n + m
evaluateMonkeyOperation (Multiply m) n = n * m
evaluateMonkeyOperation MultiplyOld n = n * n

data MonkeyTest = MonkeyTest
  { ifDivisibleBy :: Integer,
    thenThrowTo :: Integer,
    elseThrowTo :: Integer
  }
  deriving (Eq, Show, Generic)

data Monkey = Monkey
  { number :: Integer,
    items :: [Integer],
    operation :: MonkeyOperation,
    test :: MonkeyTest
  }
  deriving (Eq, Show, Generic)

indent :: Parser ()
indent = void (string "  ")

monkeyOperationParser :: Parser MonkeyOperation
monkeyOperationParser =
  lexeme (string "new")
    *> lexeme (char '=')
    *> lexeme (string "old")
    *> choice
      [ try (Add <$> (lexeme (char '+') *> lexeme decimal)),
        try (Multiply <$> (lexeme (char '*') *> lexeme decimal)),
        MultiplyOld <$ (lexeme (char '*') *> lexeme (string "old"))
      ]

monkeyTestParser :: Parser indent -> Parser MonkeyTest
monkeyTestParser beforeLine = do
  ifDivisibleBy' :: Integer <- beforeLine *> lexeme (string "Test:") *> lexeme (string "divisible by") *> lexeme decimal <* newline
  thenThrowTo' :: Integer <- beforeLine *> indent *> lexeme (string "If true:") *> lexeme (string "throw to monkey") *> lexeme decimal <* newline
  elseThrowTo' :: Integer <- beforeLine *> indent *> lexeme (string "If false:") *> lexeme (string "throw to monkey") *> lexeme decimal <* newline

  return $ MonkeyTest ifDivisibleBy' thenThrowTo' elseThrowTo'

monkeyParser :: Parser Monkey
monkeyParser = do
  number' :: Integer <- lexeme (string "Monkey") *> lexeme decimal <* lexeme (char ':') <* newline
  items' :: [Integer] <- indent *> lexeme (string "Starting items:") *> sepBy1NonGreedy (lexeme decimal) (string ", ") <* newline
  operation' <- indent *> lexeme (string "Operation:") *> monkeyOperationParser <* newline
  test' <- monkeyTestParser indent
  return $ Monkey number' items' operation' test'

inputParser :: Parser [Monkey]
inputParser = sepBy1NonGreedy monkeyParser newline

type MonkeyBusinessState = M.Map Integer Monkey

initialMonkeyBusinessState :: [Monkey] -> MonkeyBusinessState
initialMonkeyBusinessState ms = M.fromList $ map (\m -> (view #number m, m)) ms

newtype MonkeyBusinessEnv = MonkeyBusinessEnv {worryReducer :: Integer -> Integer}

monkeyTest :: MonkeyTest -> Integer -> Integer
monkeyTest t worryLevel
  | worryLevel `rem` view #ifDivisibleBy t == 0 = view #thenThrowTo t
  | otherwise = view #elseThrowTo t

runMonkeyTurn :: (MonadReader MonkeyBusinessEnv m, MonadState MonkeyBusinessState m) => (Integer -> Integer -> m ()) -> Integer -> m ()
runMonkeyTurn onInspect' monkeyNum = do
  worryReducer' <- asks worryReducer

  monkey <- use $ at monkeyNum % to fromJust
  assign (at monkeyNum % _Just % #items) []

  let items' = view #items monkey
      operation' = evaluateMonkeyOperation $ view #operation monkey
      test' = monkeyTest $ view #test monkey

  forM_ items' $ \i -> do
    onInspect' monkeyNum i

    let worryLevel = worryReducer' $ operation' i
        targetMonkeyNum = test' worryLevel

    modifying' (at targetMonkeyNum % _Just % #items) (++ [worryLevel])

runRound :: (MonadState MonkeyBusinessState m, MonadReader MonkeyBusinessEnv m) => (Integer -> Integer -> m ()) -> m ()
runRound onInspect' = do
  traceM "Start round"

  -- M.keys returns in ascending order, which is exactly the order we want
  nums <- gets M.keys
  forM_ nums $ \num -> runMonkeyTurn onInspect' num

countOccurences :: (Foldable f, Ord a) => f a -> M.Map a Integer
countOccurences = foldl1 (M.unionWith (+)) . map (`M.singleton` 1) . toList

monkeyBusinessLevel :: M.Map a Integer -> Integer
monkeyBusinessLevel = product . map snd . take 2 . sortOn (Down . snd) . M.toList

solution :: (Integer -> Integer) -> Int -> [Monkey] -> Integer
solution worryReducer' roundCount input =
  let onInspect' monkeyNum _ = tell $ Endo (monkeyNum :)

      env = MonkeyBusinessEnv worryReducer'

      business = replicateM_ roundCount (runRound onInspect')

      (_, Endo inspections) = execRWS business env $ initialMonkeyBusinessState input
      inspectionsByMonkeyNum = countOccurences $ inspections []
   in monkeyBusinessLevel inspectionsByMonkeyNum

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  print $ solution (`div` 3) 20 input

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let testProduct :: Integer
      testProduct = product $ map (view (#test % #ifDivisibleBy)) input

  print $ solution (`rem` testProduct) 10_000 input
