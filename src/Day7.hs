{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Day7 where

import Common (Parser, letters1, parseFromFile)
import Data.Char (isAlphaNum)
import Data.Functor (void)
import Data.Functor.Foldable (Base, Recursive (..), cata)
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as LT
import Text.Megaparsec (choice, eof, lookAhead, sepBy, sepEndBy1, takeWhile1P, try)
import Text.Megaparsec.Char (char, hspace, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Text.Megaparsec.Char.Lexer as L

data ChangeDirectoryParameter = ToRoot | Out | In LT.Text deriving (Eq, Show)

data ListEntry
  = Directory LT.Text
  | File Integer LT.Text
  deriving (Eq, Show)

data CommandUsage = ChangeDirectory ChangeDirectoryParameter | List [ListEntry] deriving (Eq, Show)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

fileNameParser :: Parser LT.Text
fileNameParser = takeWhile1P (Just "filename-legal characters") $ \c -> c == '.' || isAlphaNum c

listEntryParser :: Parser ListEntry
listEntryParser =
  choice
    [ Directory <$> (lexeme (string "dir") *> lexeme fileNameParser),
      File <$> lexeme decimal <*> lexeme fileNameParser
    ]

listCommandUsageParser :: Parser CommandUsage
listCommandUsageParser = do
  void $ lexeme $ string "ls"
  void newline

  es <- sepBy listEntryParser (try (newline *> lookAhead listEntryParser))

  return $ List es

commandUsageParser :: Parser CommandUsage
commandUsageParser = lexeme (char '$') *> choice [try cd, listCommandUsageParser]
  where
    cd = ChangeDirectory <$> (lexeme (string "cd") *> cdParam)

    cdParam =
      lexeme $
        choice
          [ ToRoot <$ string "/",
            Out <$ string "..",
            In <$> letters1
          ]

inputParser :: Parser [CommandUsage]
inputParser = sepEndBy1 commandUsageParser newline

data DirectoryTree = DTDirectory (M.Map LT.Text DirectoryTree) | DTFile Integer deriving (Eq, Show)

isDirectory :: DirectoryTree -> Bool
isDirectory (DTDirectory _) = True
isDirectory _ = False

type FilesystemState = ([LT.Text], DirectoryTree)

initialFilesystemState :: FilesystemState
initialFilesystemState = ([], DTDirectory M.empty)

unsnoc :: [a] -> Maybe (a, [a])
unsnoc [] = Nothing
unsnoc xs = Just (last xs, init xs)

modifyDirectoryTreeAt :: [LT.Text] -> (DirectoryTree -> DirectoryTree) -> DirectoryTree -> DirectoryTree
modifyDirectoryTreeAt (unsnoc -> Nothing) f x = f x
modifyDirectoryTreeAt _ _ (DTFile _) = error "cannot navigate into file"
modifyDirectoryTreeAt (unsnoc -> Just (curDir, rest)) f (DTDirectory m) = DTDirectory $ M.adjust (modifyDirectoryTreeAt rest f) curDir m

applyCommandToFilesystemState :: FilesystemState -> CommandUsage -> FilesystemState
applyCommandToFilesystemState (_, dt) (ChangeDirectory ToRoot) = ([], dt)
applyCommandToFilesystemState (cwd, dt) (ChangeDirectory Out) = (tail cwd, dt)
applyCommandToFilesystemState (cwd, dt) (ChangeDirectory (In newDir)) = (newDir : cwd, dt)
applyCommandToFilesystemState (cwd, dt) (List es) = (cwd, modifyDirectoryTreeAt cwd insertEntries dt)
  where
    insertEntries dt' = foldl (flip insertEntry) dt' es

    insertEntry :: ListEntry -> DirectoryTree -> DirectoryTree
    insertEntry _ (DTFile _) = error "cannot insert entry into a file"
    insertEntry (Directory name) (DTDirectory m) = DTDirectory $ M.insert name (DTDirectory M.empty) m
    insertEntry (File size name) (DTDirectory m) = DTDirectory $ M.insert name (DTFile size) m

data DirectoryTreeF a = DTDirectoryF (M.Map LT.Text a) | DTFileF Integer deriving (Eq, Show, Functor)

type instance Base DirectoryTree = DirectoryTreeF

instance Recursive DirectoryTree where
  project :: DirectoryTree -> DirectoryTreeF DirectoryTree
  project (DTDirectory m) = DTDirectoryF m
  project (DTFile size) = DTFileF size

totalSize :: DirectoryTree -> Integer
totalSize = cata go
  where
    go :: DirectoryTreeF Integer -> Integer
    go (DTFileF size) = size
    go (DTDirectoryF m) = sum m

type DirectoryUsageEntry = (Maybe LT.Text, Bool, Integer)

directoryUsage :: DirectoryTree -> [DirectoryUsageEntry]
directoryUsage rootDt = (Nothing, True, totalSize rootDt) : para go rootDt
  where
    go :: DirectoryTreeF (DirectoryTree, [DirectoryUsageEntry]) -> [DirectoryUsageEntry]
    go (DTFileF _) = []
    go (DTDirectoryF (M.toList -> es)) = concatMap go' es

    go' :: (LT.Text, (DirectoryTree, [DirectoryUsageEntry])) -> [DirectoryUsageEntry]
    go' (dirName, (dt, xs)) = (Just dirName, isDirectory dt, totalSize dt) : xs

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let (_, dt) = foldl applyCommandToFilesystemState initialFilesystemState input

  print $ sum $ map (\(_, _, size) -> size) $ filter (\(_, isDir, size) -> isDir && size <= 100_000) $ directoryUsage dt

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let (_, dt) = foldl applyCommandToFilesystemState initialFilesystemState input

      maxSize :: Integer
      maxSize = 70_000_000

      availableSize = maxSize - totalSize dt

      neededSizeForUpdate :: Integer
      neededSizeForUpdate = 30_000_000

      sizeNeededToBeCleared = neededSizeForUpdate - availableSize

      possibleDirectoriesToDelete = filter (\(_, isDir, size) -> isDir && size >= sizeNeededToBeCleared) $ directoryUsage dt

  print $ minimum $ map (\(_, _, size) -> size) possibleDirectoriesToDelete
