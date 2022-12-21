{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Day21 (part1, part2) where

import Common (Parser, char, decimal, lexeme, parseFromFile)
import qualified Common
import Data.Fix (Fix (Fix))
import Data.Foldable (find)
import Data.Functor.Foldable (cata)
import qualified Data.Map.Lazy as LM
import qualified Data.Text.Lazy as LT
import Text.Megaparsec (MonadParsec (eof), choice, sepEndBy1)
import Text.Megaparsec.Char (newline)
import Text.Show.Deriving (deriveShow1)

letters1 :: Parser LT.Text
letters1 = lexeme Common.letters1

data MathOperation = Add | Subtract | Multiply | Divide deriving (Eq, Show)

evaluate :: MathOperation -> (Integer -> Integer -> Integer)
evaluate Add = (+)
evaluate Subtract = (-)
evaluate Multiply = (*)
evaluate Divide = div

data MonkeyShoutF a = Literal Integer | Operation a MathOperation a | HumanInput deriving (Eq, Show, Functor)

$(deriveShow1 ''MonkeyShoutF)

type MonkeyShout = Fix MonkeyShoutF

data Monkey = Monkey {name :: LT.Text, shout :: MonkeyShoutF LT.Text} deriving (Eq, Show)

mathOperationParser :: Parser MathOperation
mathOperationParser = choice $ map (\(v, c) -> v <$ char c) ops
  where
    ops = [(Add, '+'), (Subtract, '-'), (Multiply, '*'), (Divide, '/')]

monkeyShoutParser :: Parser (MonkeyShoutF LT.Text)
monkeyShoutParser =
  choice
    [ Literal <$> decimal,
      Operation <$> letters1 <*> mathOperationParser <*> letters1
    ]

monkeyParser :: Parser Monkey
monkeyParser = Monkey <$> letters1 <* char ':' <*> monkeyShoutParser

inputParser :: Parser [Monkey]
inputParser = sepEndBy1 monkeyParser newline <* eof

hydrateTree :: [Monkey] -> LM.Map LT.Text MonkeyShout
hydrateTree ms = nameShoutMap
  where
    nameShoutMap :: LM.Map LT.Text MonkeyShout
    nameShoutMap = LM.fromList $ map (\m -> (name m, Fix $ (nameShoutMap LM.!) <$> shout m)) ms

evaluateTree :: MonkeyShout -> Integer
evaluateTree = cata go
  where
    go :: MonkeyShoutF Integer -> Integer
    go (Literal n) = n
    go (Operation n o m) = evaluate o n m
    go HumanInput = error "cannot evaluate HumanInput"

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile inputParser inputPath

  print $ evaluateTree $ (LM.! "root") $ hydrateTree input

containsHumanInput :: MonkeyShout -> Bool
containsHumanInput = cata go
  where
    go :: MonkeyShoutF Bool -> Bool
    go (Literal _) = False
    go HumanInput = True
    go (Operation a _ b) = a || b

swapHumanInputFirst :: (MonkeyShout, MonkeyShout) -> (MonkeyShout, MonkeyShout)
swapHumanInputFirst (a, b)
  | containsHumanInput a = (a, b)
  | otherwise = (b, a)

adjustInputForPart2 :: [Monkey] -> Maybe (MonkeyShout, MonkeyShout)
adjustInputForPart2 ms = do
  Monkey _ shout' <- find ((== "root") . name) ms

  (a, b) <- case shout' of
    Operation a _ b -> Just (a, b)
    _ -> Nothing

  let tree = hydrateTree $ map (\m -> if name m == "humn" then m {shout = HumanInput} else m) ms

  return (tree LM.! a, tree LM.! b)

solveForHumanInput :: (MonkeyShout, MonkeyShout) -> MonkeyShout
solveForHumanInput = uncurry go . swapHumanInputFirst
  where
    addOp a b = Fix (Operation a Add b)
    subtractOp a b = Fix (Operation a Subtract b)
    multiplyOp a b = Fix (Operation a Multiply b)
    divideOp a b = Fix (Operation a Divide b)

    go :: MonkeyShout -> MonkeyShout -> MonkeyShout
    go (Fix (Literal _)) _ = error "cannot solve Literal"
    go (Fix HumanInput) b = b
    go (Fix (Operation a op b)) x
      | containsHumanInput a = go a $ case op of
          Add -> x `subtractOp` b
          Subtract -> x `addOp` b
          Multiply -> x `divideOp` b
          Divide -> x `multiplyOp` b
      | otherwise = go b $ case op of
          Add -> x `subtractOp` a
          Subtract -> a `subtractOp` x
          Multiply -> x `divideOp` a
          Divide -> a `divideOp` x

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile inputParser inputPath

  print $ evaluateTree . solveForHumanInput <$> adjustInputForPart2 input
