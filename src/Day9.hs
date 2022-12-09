{-# LANGUAGE ScopedTypeVariables #-}

module Day9 where

import Common (Parser, parseFromFile)
import Control.Monad (forM_)
import Data.Array (Array, array)
import qualified Data.Array as A
import Data.Array.MArray (MArray (newArray), readArray, writeArray)
import Data.Array.ST (runSTArray)
import Data.List (nub)
import Linear (V2 (..))
import Text.Megaparsec (MonadParsec (eof), choice, sepEndBy1)
import Text.Megaparsec.Char (char, hspace, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Text.Megaparsec.Char.Lexer as L

data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Eq, Show)

data Move = Move Direction Integer deriving (Eq, Show)

directionParser :: Parser Direction
directionParser =
  choice
    [ DirUp <$ char 'U',
      DirDown <$ char 'D',
      DirLeft <$ char 'L',
      DirRight <$ char 'R'
    ]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

moveParser :: Parser Move
moveParser = Move <$> lexeme directionParser <*> lexeme decimal

inputParser :: Parser [Move]
inputParser = sepEndBy1 moveParser newline

expandMove :: Move -> [Direction]
expandMove (Move dir n) = replicate (fromInteger n) dir

type Coordinate = V2 Integer

directionToVector :: Direction -> V2 Integer
directionToVector DirUp = V2 0 1
directionToVector DirDown = V2 0 (-1)
directionToVector DirLeft = V2 (-1) 0
directionToVector DirRight = V2 1 0

newTailPosition :: Coordinate -> Coordinate -> Coordinate -> Coordinate
newTailPosition oldTailPos _ newHeadPos
  | maximum (abs (newHeadPos - oldTailPos)) >= 2 = oldTailPos + signum (newHeadPos - oldTailPos)
  | otherwise = oldTailPos

type RopeState = (Coordinate, Coordinate)

updateRopeState :: RopeState -> Direction -> RopeState
updateRopeState (oldHeadPos, oldTailPos) dir = (newHeadPos, newTailPos)
  where
    newHeadPos = oldHeadPos + directionToVector dir
    newTailPos = newTailPosition oldTailPos oldHeadPos newHeadPos

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  print $ length $ nub $ map snd $ scanl updateRopeState (pure 0, pure 0) $ concatMap expandMove input

type RopeState10 = Array Int Coordinate

initialRopeState10 :: RopeState10
initialRopeState10 = array (1, 10) $ zip [1 ..] $ replicate 10 (pure 0)

updateRopeState10 :: RopeState10 -> Direction -> RopeState10
updateRopeState10 oldPoses dir = runSTArray $ do
  newPoses <- newArray (1 :: Int, 10) undefined

  let oldHead = oldPoses A.! 1
  writeArray newPoses 1 $ directionToVector dir + oldHead

  forM_ [2 .. 10] $ \i -> do
    let oldHeadPos = oldPoses A.! (i - 1)
        oldTailPos = oldPoses A.! i
    newHeadPos <- readArray newPoses (i - 1)

    let newTail = newTailPosition oldTailPos oldHeadPos newHeadPos
    writeArray newPoses i newTail

  return newPoses

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  print $ length $ nub $ map (A.! 10) $ scanl updateRopeState10 initialRopeState10 $ concatMap expandMove input
