module Common where

import Data.Char (isLetter)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec
  ( Parsec,
    ShowErrorComponent,
    errorBundlePretty,
    parse,
    takeWhile1P,
  )

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
