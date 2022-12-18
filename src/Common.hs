{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}

module Common where

import Control.Monad (guard)
import Data.Char (isLetter)
import Data.Foldable (toList)
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Data.Void (Void)
import Linear (V2 (V2), V3 (V3))
import System.Exit (exitFailure)
import Text.Megaparsec
  ( Parsec,
    ShowErrorComponent,
    errorBundlePretty,
    many,
    parse,
    takeWhile1P,
    try,
  )
import qualified Text.Megaparsec.Byte.Lexer as L
import Text.Megaparsec.Char (hspace)
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as MCL

type Parser = Parsec Void LT.Text

parseFromFile :: ShowErrorComponent e => Parsec e LT.Text a -> FilePath -> IO a
parseFromFile p file = do
  res <- parse p file <$> LT.readFile file
  case res of
    Left peb -> do
      putStrLn $ errorBundlePretty peb
      exitFailure
    Right a -> return a

letters1 :: Parser LT.Text
letters1 = takeWhile1P (Just "letters") isLetter

sepBy1NonGreedy :: Parser b -> Parser a -> Parser [b]
sepBy1NonGreedy p sep = do
  x <- p
  (x :) <$> many (try (sep *> p))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

char :: Char -> Parser Char
char = lexeme . MC.char

string :: LT.Text -> Parser LT.Text
string = lexeme . MC.string

decimal :: Num a => Parser a
decimal = lexeme MCL.decimal

the :: (Foldable f, Ord a) => f a -> Maybe a
the xs = do
  let xs' = S.fromList $ toList xs
  guard $ S.size xs' == 1
  return $ S.findMin xs'

unpackJust :: MonadFail m => m (Maybe a) -> m a
unpackJust mma = do
  ma <- mma
  case ma of
    Nothing -> fail "received Nothing, expected Just"
    (Just a) -> pure a

count :: Foldable f => (a -> Bool) -> f a -> Int
count p = length . filter p . toList

class Tuplifiable f t | f -> t, t -> f where
  tuplify :: f -> t
  untuplify :: t -> f

instance Tuplifiable (V2 i) (i, i) where
  tuplify :: V2 i -> (i, i)
  tuplify (V2 x y) = (x, y)
  untuplify :: (i, i) -> V2 i
  untuplify (x, y) = V2 x y

instance Tuplifiable (V3 i) (i, i, i) where
  tuplify :: V3 i -> (i, i, i)
  tuplify (V3 x y z) = (x, y, z)
  untuplify :: (i, i, i) -> V3 i
  untuplify (x, y, z) = V3 x y z
