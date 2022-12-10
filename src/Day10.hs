{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Day10 where

import Common (Parser, lexeme, parseFromFile)
import Control.Monad.State.Strict (MonadState, State, execState, execStateT, replicateM_, when)
import Control.Monad.Writer.Strict (MonadWriter (tell), runWriter)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import GHC.Generics (Generic)
import Optics
  ( Each (each),
    Field2 (_2),
    assign,
    modifying',
    over,
    use,
    view,
    (%),
  )
import Text.Megaparsec (MonadParsec (eof), choice, sepEndBy1)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Instruction = AddX Integer | NoOp deriving (Eq, Show)

cycleCount :: Instruction -> Integer
cycleCount (AddX _) = 2
cycleCount NoOp = 1

instructionParser :: Parser Instruction
instructionParser =
  choice
    [ AddX <$> (lexeme (string "addx") *> lexeme (signed (pure ()) decimal)),
      NoOp <$ lexeme (string "noop")
    ]

inputParser :: Parser [Instruction]
inputParser = sepEndBy1 instructionParser newline

clock :: [()]
clock = replicate 300 ()

data ProcessorState = ProcessorState
  { x :: Integer,
    currentInstruction :: Maybe Instruction,
    cyclesLeftOnInstruction :: Integer,
    instructionsToExecute :: [Instruction]
  }
  deriving (Eq, Show, Generic)

initialProcessorState :: [Instruction] -> ProcessorState
initialProcessorState = ProcessorState 1 Nothing 0

popInstruction :: MonadState ProcessorState m => m (Maybe Instruction)
popInstruction = do
  is <- use #instructionsToExecute
  case is of
    [] -> return Nothing
    i : rest -> do
      assign #instructionsToExecute rest
      return $ Just i

process :: MonadState ProcessorState m => m ()
process = do
  mI <- use #currentInstruction
  case mI of
    Just i -> do
      n <- use #cyclesLeftOnInstruction
      if n > 0
        then modifying' #cyclesLeftOnInstruction (subtract 1)
        else do
          case i of
            AddX toAdd -> modifying' #x (+ toAdd)
            NoOp -> return ()
          assign #currentInstruction Nothing
    Nothing -> do
      mNewI <- popInstruction
      case mNewI of
        Nothing -> return ()
        Just newI -> do
          assign #currentInstruction $ Just newI
          assign #cyclesLeftOnInstruction $ cycleCount newI - 1
          process

stateToFold :: State s a -> s -> () -> s
stateToFold m s () = execState m s

relevantCycles :: [Integer]
relevantCycles = [20, 60, 100, 140, 180, 220]

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let ss = scanl (stateToFold process) (initialProcessorState input) clock
      ss' = filter ((`elem` relevantCycles) . fst) $ zip [1 :: Integer ..] ss

  print $ sum $ map (uncurry (*)) $ over (each % _2) (view #x) ss'

data VideoState = VideoState
  { processor :: ProcessorState,
    currentPosition :: Integer
  }
  deriving (Eq, Show, Generic)

initialVideoState :: [Instruction] -> VideoState
initialVideoState is = VideoState (initialProcessorState is) 0

decomposeScreenPosition :: Integer -> (Integer, Integer)
decomposeScreenPosition n = n `quotRem` 40

doOutputNewline :: Integer -> Bool
doOutputNewline n = (n + 1) `rem` 40 == 0

processVideo :: (MonadState VideoState m, MonadWriter LT.Text m) => m ()
processVideo = do
  (_, x') <- decomposeScreenPosition <$> use #currentPosition
  doOutputNewline' <- doOutputNewline <$> use #currentPosition
  spriteX <- use $ #processor % #x

  let isLit = abs (x' - spriteX) <= 1

  tell $ if isLit then "#" else "."
  when doOutputNewline' $ tell "\n"

  modifying' #currentPosition (+ 1)

  modifying' #processor $ execState process

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let (_, output) = runWriter $ execStateT (replicateM_ 240 processVideo) (initialVideoState input)

  LT.putStrLn output
