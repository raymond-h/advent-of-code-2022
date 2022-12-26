module Day25 where

import Common (Parser, lexeme, parseFromFile)
import Text.Megaparsec (choice, eof, sepEndBy1, some)
import qualified Text.Megaparsec.Char as C

data SnafuDigit = DoubleMinus | Minus | Zero | One | Two deriving (Eq, Show)

snafuDigitToChar :: SnafuDigit -> Char
snafuDigitToChar DoubleMinus = '='
snafuDigitToChar Minus = '-'
snafuDigitToChar Zero = '0'
snafuDigitToChar One = '1'
snafuDigitToChar Two = '2'

snafuDigitToInteger :: SnafuDigit -> Integer
snafuDigitToInteger DoubleMinus = -2
snafuDigitToInteger Minus = -1
snafuDigitToInteger Zero = 0
snafuDigitToInteger One = 1
snafuDigitToInteger Two = 2

type SnafuNumber = [SnafuDigit]

base5Places :: [Integer]
base5Places = iterate (* 5) (1 :: Integer)

snafuNumberToInteger :: SnafuNumber -> Integer
snafuNumberToInteger sn = sum $ zipWith (*) base5Places (map snafuDigitToInteger $ reverse sn)

integerToSnafuNumber :: Integer -> SnafuNumber
integerToSnafuNumber 0 = []
integerToSnafuNumber n = rest ++ [sd]
  where
    remainder = n `mod` 5
    sd = [Zero, One, Two, DoubleMinus, Minus] !! fromInteger remainder

    rest = integerToSnafuNumber $ (n + 2) `div` 5

snafuDigitParser :: Parser SnafuDigit
snafuDigitParser =
  choice
    [ DoubleMinus <$ C.char '=',
      Minus <$ C.char '-',
      Zero <$ C.char '0',
      One <$ C.char '1',
      Two <$ C.char '2'
    ]

snafuNumberParser :: Parser SnafuNumber
snafuNumberParser = lexeme $ some snafuDigitParser

inputParser :: Parser [SnafuNumber]
inputParser = sepEndBy1 snafuNumberParser C.newline <* eof

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile inputParser inputPath

  let result = sum $ map snafuNumberToInteger input

  putStrLn $ map snafuDigitToChar $ integerToSnafuNumber result

part2 :: FilePath -> IO ()
part2 _ = do
  putStrLn "Start the blender!"
