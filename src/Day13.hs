{-# LANGUAGE InstanceSigs #-}

module Day13 where

import Common (Parser, lexeme, parseFromFile)
import Data.List (intercalate, sort)
import Optics (each, toListOf)
import Text.Megaparsec (MonadParsec (eof), between, choice, sepBy, sepEndBy1, try)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

data Packet = PInteger Integer | PList [Packet] deriving (Eq)

instance Show Packet where
  show :: Packet -> String
  show (PInteger n) = show n
  show (PList ps) = "[" ++ intercalate "," (map show ps) ++ "]"

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (PInteger n) (PInteger m) = compare n m
  compare i@(PInteger _) p = compare (PList [i]) p
  compare p i@(PInteger _) = compare p (PList [i])
  -- The puzzle describes lexicographically sorting lists
  -- which, incidentally, is exactly how the Ord instance for lists works
  compare (PList ps) (PList qs) = compare ps qs

packetParser :: Parser Packet
packetParser =
  choice
    [ try (PList <$> between (lexeme $ char '[') (lexeme $ char ']') (sepBy (lexeme packetParser) (lexeme $ char ','))),
      PInteger <$> lexeme decimal
    ]

packetPairParser :: Parser (Packet, Packet)
packetPairParser = do
  a <- packetParser <* newline
  b <- packetParser <* newline
  return (a, b)

inputParser :: Parser [(Packet, Packet)]
inputParser = sepEndBy1 packetPairParser newline

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  print $ sum $ map fst $ filter ((== LT) . snd) $ zip [1 :: Integer ..] $ map (uncurry compare) input

dividerPackets :: [Packet]
dividerPackets =
  [ PList [PList [PInteger 2]],
    PList [PList [PInteger 6]]
  ]

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let packets = sort $ concatMap (toListOf each) input ++ dividerPackets

  print $ product $ map fst $ filter ((`elem` dividerPackets) . snd) $ zip [1 :: Integer ..] packets
