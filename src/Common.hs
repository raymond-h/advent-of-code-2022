module Common where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec
  ( Parsec,
    ShowErrorComponent,
    errorBundlePretty,
    parse,
    some,
  )
import Text.Megaparsec.Char (digitChar)

type Parser = Parsec Void LT.Text

parseFromFile :: ShowErrorComponent e => Parsec e LT.Text a -> FilePath -> IO a
parseFromFile p file = do
  res <- parse p file <$> LT.readFile file
  case res of
    Left peb -> do
      putStrLn $ errorBundlePretty peb
      exitFailure
    Right a -> return a

integer :: Parser Integer
integer = read <$> some digitChar
