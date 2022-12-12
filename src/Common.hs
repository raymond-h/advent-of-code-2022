module Common where

import Control.Monad (guard)
import Data.Char (isLetter)
import Data.Foldable (toList)
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Data.Void (Void)
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
