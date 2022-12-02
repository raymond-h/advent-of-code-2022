module Day2 where

import Common (Parser, parseFromFile)
import Data.List (find)
import Data.Maybe (fromJust)
import Text.Megaparsec (choice, eof, some)
import Text.Megaparsec.Char (char, hspace, newline)
import Data.Bifunctor (Bifunctor(bimap))

data Column1 = A | B | C deriving (Eq, Show)

data Column2 = X | Y | Z deriving (Eq, Show)

column1Parser :: Parser Column1
column1Parser =
  choice
    [ A <$ char 'A',
      B <$ char 'B',
      C <$ char 'C'
    ]

column2Parser :: Parser Column2
column2Parser =
  choice
    [ X <$ char 'X',
      Y <$ char 'Y',
      Z <$ char 'Z'
    ]

inputLinesParser :: Parser [(Column1, Column2)]
inputLinesParser = some $ do
  c1 <- column1Parser
  _ <- hspace
  c2 <- column2Parser
  _ <- newline
  return (c1, c2)

data RockPaperScissorsAction = Rock | Paper | Scissors deriving (Eq, Enum, Bounded, Show)

data RockPaperScissorsResult = Player1Win | Player2Win | Draw deriving (Eq, Show)

winsRockPaperScissors :: RockPaperScissorsAction -> RockPaperScissorsAction -> Bool
winsRockPaperScissors Paper Rock = True
winsRockPaperScissors Rock Scissors = True
winsRockPaperScissors Scissors Paper = True
winsRockPaperScissors _ _ = False

rockPaperScissors :: RockPaperScissorsAction -> RockPaperScissorsAction -> RockPaperScissorsResult
rockPaperScissors a b
  | winsRockPaperScissors a b = Player1Win
  | winsRockPaperScissors b a = Player2Win
  | otherwise = Draw

column1ToRps :: Column1 -> RockPaperScissorsAction
column1ToRps A = Rock
column1ToRps B = Paper
column1ToRps C = Scissors

part1Column2ToRps :: Column2 -> RockPaperScissorsAction
part1Column2ToRps X = Rock
part1Column2ToRps Y = Paper
part1Column2ToRps Z = Scissors

scoreLine :: RockPaperScissorsAction -> RockPaperScissorsAction -> Integer
scoreLine opponent self = scoreAction self + scoreOutcome (rockPaperScissors self opponent)
  where
    scoreAction Rock = 1
    scoreAction Paper = 2
    scoreAction Scissors = 3
    scoreOutcome Player2Win = 0
    scoreOutcome Draw = 3
    scoreOutcome Player1Win = 6

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile (inputLinesParser <* eof) inputPath

  print $ sum $ map (uncurry scoreLine . bimap column1ToRps part1Column2ToRps) input

part2Column2ToRpsResult :: Column2 -> RockPaperScissorsResult
part2Column2ToRpsResult X = Player2Win
part2Column2ToRpsResult Y = Draw
part2Column2ToRpsResult Z = Player1Win

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

part2NeededAction :: RockPaperScissorsAction -> RockPaperScissorsResult -> RockPaperScissorsAction
part2NeededAction opponent neededResult = fromJust $ find (\x -> rockPaperScissors x opponent == neededResult) enumerate

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile (inputLinesParser <* eof) inputPath

  let
    mapper :: (Column1, Column2) -> (RockPaperScissorsAction, RockPaperScissorsAction)
    mapper (col1, col2) =
      let
        opponent :: RockPaperScissorsAction
        opponent = column1ToRps col1
      in
        (opponent, part2NeededAction opponent (part2Column2ToRpsResult col2))

    input' :: [(RockPaperScissorsAction, RockPaperScissorsAction)]
    input' = map mapper input

    finalScore :: Integer
    finalScore = sum $ map (uncurry scoreLine) input'

  print finalScore
