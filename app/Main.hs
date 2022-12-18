module Main (main) where

import Lib (Options (Options), doThing)
import Options.Applicative (Parser, ParserInfo, auto, execParser, help, helper, info, long, metavar, option, optional, short, strArgument, (<**>))

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strArgument (metavar "INPUT-FILE")
    <*> optional (option auto (long "day" <> short 'd' <> metavar "DAY-NUM" <> help "Which day to run (defaults to current day)"))

opts :: ParserInfo Options
opts = info (optionsParser <**> helper) mempty

main :: IO ()
main = do
  options <- execParser opts
  doThing options
