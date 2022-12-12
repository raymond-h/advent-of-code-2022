{-# LANGUAGE TupleSections #-}

module Day12 where

import Common (Parser, letters1, parseFromFile, unpackJust)
import Common.Array (mk2dArray)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.ST (ST)
import Control.Monad.ST.Strict (runST)
import Data.Array (Array, Ix, array, assocs, bounds, inRange, range)
import qualified Data.Array as A
import Data.Char (ord)
import Data.Foldable (find)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.STRef (STRef)
import Data.STRef.Strict (modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy as LT
import Linear (V2 (V2))
import Text.Megaparsec (MonadParsec (eof), sepEndBy1)
import Text.Megaparsec.Char (newline)

type Coord = V2 Integer

type Heightmap = Array Coord Integer

charToHeight :: Char -> Int
charToHeight 'S' = ord 'a' - ord 'a'
charToHeight 'E' = ord 'z' - ord 'a'
charToHeight c = ord c - ord 'a'

findIndex :: Ix i => (a -> Bool) -> Array i a -> Maybe i
findIndex p as = fst <$> find (p . snd) (assocs as)

elemIndex :: (Ix i, Eq a) => a -> Array i a -> Maybe i
elemIndex e = findIndex (== e)

inputParser :: Parser (Coord, Coord, Heightmap)
inputParser = do
  charArr <- unpackJust $ mk2dArray . map LT.unpack <$> sepEndBy1 letters1 newline

  let heightmap :: Heightmap
      heightmap = toInteger . charToHeight <$> charArr

  startPos <- unpackJust $ pure $ elemIndex 'S' charArr
  endPos <- unpackJust $ pure $ elemIndex 'E' charArr

  return (startPos, endPos, heightmap)

whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ mCond mBody = do
  cond <- mCond
  when cond $ do
    void mBody
    whileM_ mCond mBody

whileJustM_ :: Monad m => m (Maybe b) -> (b -> m a) -> m ()
whileJustM_ mCond mBody = do
  cond <- mCond
  case cond of
    Nothing -> return ()
    Just b -> do
      void $ mBody b
      whileJustM_ mCond mBody

modifySTRefWithValue :: STRef s a -> (a -> (b, a)) -> ST s b
modifySTRefWithValue ref f = do
  v <- readSTRef ref
  let (x, v') = f v
  writeSTRef ref v'
  return x

unsnoc :: Seq.Seq a -> Maybe (Seq.Seq a, a)
unsnoc Seq.Empty = Nothing
unsnoc (as Seq.:|> a) = Just (as, a)

calculateCameFromMap :: Ord p => (p -> [p]) -> p -> M.Map p (Maybe p)
calculateCameFromMap neighborsOf start = runST $ do
  frontierRef <- newSTRef $ Seq.singleton start
  cameFromRef <- newSTRef $ M.singleton start Nothing

  whileJustM_ (unsnoc <$> readSTRef frontierRef) $ \(as, current) -> do
    writeSTRef frontierRef as

    forM_ (neighborsOf current) $ \next -> do
      m <- readSTRef cameFromRef
      unless (M.member next m) $ do
        modifySTRef' frontierRef (next Seq.:<|)
        writeSTRef cameFromRef $ M.insert next (Just current) m

  readSTRef cameFromRef

pathFromPoint :: Ord p => p -> M.Map p (Maybe p) -> Maybe [p]
pathFromPoint p m = case M.lookup p m of
  Nothing -> Nothing
  Just m_p -> case m_p of
    Nothing -> Just [p]
    (Just p') -> (p :) <$> pathFromPoint p' m

defaultArray :: Ix i => (i, i) -> e -> Array i e
defaultArray bounds' e = array bounds' (map (,e) (range bounds'))

cameFromMapVisualizer :: (Coord, Coord) -> M.Map Coord (Maybe Coord) -> Array Coord Char
cameFromMapVisualizer bounds' m = defaultArray bounds' '.' A.// map (\p -> (fst p, f p)) (M.assocs m)
  where
    f :: (Coord, Maybe Coord) -> Char
    f (_, Nothing) = '*'
    f (current, Just next) = g (next - current)

    g :: V2 Integer -> Char
    g (V2 1 0) = '>'
    g (V2 (-1) 0) = '<'
    g (V2 0 1) = 'V'
    g (V2 0 (-1)) = '^'
    g v = error $ "unknown relative position: " ++ show v

allDirections :: [V2 Integer]
allDirections = [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]

heightmapNeighbors :: Heightmap -> Coord -> [Coord]
heightmapNeighbors heightmap p = filter validNeighbor $ map (+ p) allDirections
  where
    validNeighbor :: Coord -> Bool
    validNeighbor p' = inRange (bounds heightmap) p' && heightmap A.! p' + 1 >= heightmap A.! p

part1 :: FilePath -> IO ()
part1 inputPath = do
  (startPos, endPos, heightmap) <- parseFromFile (inputParser <* eof) inputPath

  let cameFromMap = calculateCameFromMap (heightmapNeighbors heightmap) endPos

  print $ subtract 1 . length <$> pathFromPoint startPos cameFromMap

part2 :: FilePath -> IO ()
part2 inputPath = do
  (_, endPos, heightmap) <- parseFromFile (inputParser <* eof) inputPath

  let cameFromMap = calculateCameFromMap (heightmapNeighbors heightmap) endPos

      allElevationACoords :: [Coord]
      allElevationACoords = map fst $ filter ((== 0) . snd) $ assocs heightmap

      x = minimum $ map (subtract 1 . length) $ mapMaybe (`pathFromPoint` cameFromMap) allElevationACoords

  print x
