module Lib where

import qualified Data.Map.Strict as M
import qualified Data.Time as T
import Solutions (solutions)

data Options = Options
  { optInputFile :: FilePath,
    optDay :: Maybe Integer
  }
  deriving (Eq, Show)

todayDayNumber :: IO Integer
todayDayNumber = do
  (_, _, day) <- T.toGregorian . T.localDay . T.zonedTimeToLocalTime <$> T.getZonedTime
  return $ toInteger day

doThing :: Options -> IO ()
doThing options = do
  day <- maybe todayDayNumber return $ optDay options

  case M.lookup day solutions of
    Nothing -> fail $ "No solution found for day " ++ show day
    Just (part1, part2) -> do
      part1 $ optInputFile options
      part2 $ optInputFile options
