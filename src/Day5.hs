{-# LANGUAGE RecordWildCards #-}

module Day5 where

import Common (Parser, parseFromFile, sepBy1NonGreedy)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isDigit, isSpace)
import Data.Foldable (foldl')
import Data.Functor (void)
import Data.List (transpose)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Text.Megaparsec (between, choice, eof, sepEndBy1, some, takeWhileP)
import Text.Megaparsec.Char (char, hspace, hspace1, letterChar, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

newtype Crate = Crate Char deriving (Eq, Show)

unCrate :: Crate -> Char
unCrate (Crate c) = c

data Instruction = Move {moveCount :: Integer, moveFrom :: Integer, moveTo :: Integer} deriving (Eq, Show)

type CrateStacks = M.Map Integer [Crate]

crateParser :: Parser Crate
crateParser = Crate <$> between (char '[') (char ']') letterChar

maybeCrateParser :: Parser (Maybe Crate)
maybeCrateParser =
  choice
    [ Nothing <$ string "   ",
      Just <$> crateParser
    ]

crateStackLineParser :: Parser [Maybe Crate]
crateStackLineParser = do
  crates <- sepBy1NonGreedy maybeCrateParser (char ' ')
  hspace
  void newline
  return crates

cleanUpCrateStacks :: [[Maybe Crate]] -> [[Crate]]
cleanUpCrateStacks = filter (not . null) . map catMaybes

crateStackParser :: Parser (M.Map Integer [Crate])
crateStackParser =
  M.fromList
    . zip [1 :: Integer ..]
    . cleanUpCrateStacks
    . transpose
    <$> some crateStackLineParser

instructionParser :: Parser Instruction
instructionParser = do
  void $ string "move"
  hspace1
  moveCount <- decimal
  hspace1
  void $ string "from"
  hspace1
  moveFrom <- decimal
  hspace1
  void $ string "to"
  hspace1
  moveTo <- decimal
  return Move {..}

inputParser :: Parser (CrateStacks, [Instruction])
inputParser = do
  crateStacks <- crateStackParser

  void $ takeWhileP Nothing (\c -> isSpace c || isDigit c)

  instructions <- sepEndBy1 instructionParser newline

  return (crateStacks, instructions)

type CrateStackSplitter = Integer -> [Crate] -> ([Crate], [Crate])

interpretInstruction :: CrateStackSplitter -> CrateStacks -> Instruction -> CrateStacks
interpretInstruction splitter cs Move {..} =
  let (c, rest) = splitter moveCount $ cs M.! moveFrom
   in M.adjust (c ++) moveTo $ M.insert moveFrom rest cs

splitterWithPostProc :: ([Crate] -> [Crate]) -> CrateStackSplitter
splitterWithPostProc f n = first f . splitAt (fromInteger n)

solution :: ([Crate] -> [Crate]) -> FilePath -> IO ()
solution cratePostproc inputPath = do
  (cs, instrs) <- parseFromFile (inputParser <* eof) inputPath

  let interpretInstruction' = interpretInstruction (splitterWithPostProc cratePostproc)

  print $ map (unCrate . head) $ M.elems $ foldl' interpretInstruction' cs instrs

part1 :: FilePath -> IO ()
part1 = solution reverse

part2 :: FilePath -> IO ()
part2 = solution id
