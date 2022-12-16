{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day16 (part1, part2, graphVisualization) where

import Common (Parser, char, decimal, lexeme, parseFromFile, sepBy1NonGreedy, string)
import Control.Monad (forM_, guard, void)
import Data.Array (Array)
import qualified Data.Array as A
import Data.Array.ST (MArray (newArray), readArray, runSTArray, writeArray)
import Data.Bifunctor (bimap, second)
import Data.Foldable (fold, maximumBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Optics (at, mapped, over, preview, (%), _2, _Just)
import Text.Megaparsec (MonadParsec (eof), choice, count, sepEndBy1)
import qualified Text.Megaparsec.Char as MC

type ValveName = LT.Text

data ValveAndTunnels = ValveAndTunnels
  { valveName :: ValveName,
    flowRate :: Integer,
    tunnelsTo :: [(Integer, ValveName)]
  }
  deriving (Eq, Show, Generic)

valveNameParser :: Parser ValveName
valveNameParser = LT.pack <$> lexeme (count 2 MC.letterChar)

valveAndTunnelsParser :: Parser ValveAndTunnels
valveAndTunnelsParser = do
  void $ string "Valve"
  valveName' <- valveNameParser
  mapM_ string ["has", "flow", "rate", "="]
  flowRate' <- decimal
  void $ char ';'
  choice
    [ mapM_ string ["tunnels", "lead", "to", "valves"],
      mapM_ string ["tunnel", "leads", "to", "valve"]
    ]
  tunnelsTo' <- map (1,) <$> sepBy1NonGreedy valveNameParser (char ',')
  return $ ValveAndTunnels {valveName = valveName', flowRate = flowRate', tunnelsTo = tunnelsTo'}

inputParser :: Parser [ValveAndTunnels]
inputParser = sepEndBy1 valveAndTunnelsParser MC.newline

graphVisualization :: [ValveAndTunnels] -> LT.Text
graphVisualization vats = "strict graph {\n" <> foldMap (fold . showVaT) vats <> "}\n"
  where
    showVaT :: ValveAndTunnels -> [LT.Text]
    showVaT orig@ValveAndTunnels {..} = "  " <> valveName <> " [label=\"" <> vaTLabel orig <> "\"]\n" : map (showVaTEdge valveName) tunnelsTo

    vaTLabel ValveAndTunnels {..} = valveName <> " (flow rate=" <> LT.pack (show flowRate) <> ")"

    showVaTEdge :: ValveName -> (Integer, ValveName) -> LT.Text
    showVaTEdge valveName (cost, otherValveName) =
      "  " <> valveName <> " -- " <> otherValveName <> " [label=" <> LT.pack (show cost) <> "]\n"

valveAndTunnelsAsMap :: [ValveAndTunnels] -> M.Map LT.Text ValveAndTunnels
valveAndTunnelsAsMap vats = M.fromList $ map (\vat -> (valveName vat, vat)) vats

type CostFunction = ValveName -> ValveName -> Integer

type FlowRateFunction = ValveName -> Integer

enumerateOptions :: S.Set ValveName -> FlowRateFunction -> CostFunction -> Integer -> ValveName -> [(Integer, S.Set ValveName)]
enumerateOptions allVns flowRateFn costFn timeLeft startingValve' =
  map (second (S.insert startingValve')) $
    inner timeLeft startingValve' (S.delete startingValve' allVns)
  where
    keepHighest :: [(Integer, S.Set ValveName)] -> [(Integer, S.Set ValveName)]
    keepHighest rs = map swap $ M.assocs $ M.fromListWith max $ map swap rs

    inner :: Integer -> ValveName -> S.Set ValveName -> [(Integer, S.Set ValveName)]
    inner _ _ s | S.null s = return (0, S.empty)
    inner timeLeftInner prevVn vats = do
      vn <- S.toList vats

      let timeLeftInner' = timeLeftInner - costFn prevVn vn - 1
          vats' = S.delete vn vats
          releasedPressure = timeLeftInner' * flowRateFn vn

      if timeLeftInner' > 0
        then do
          x <- (0, S.empty) : keepHighest (inner timeLeftInner' vn vats')

          return $ bimap (+ releasedPressure) (S.insert vn) x
        else return (0, S.empty)

pairings :: (Eq b) => [b] -> [(b, b)]
pairings as = do
  a <- as
  b <- as
  guard $ a /= b
  return (a, b)

addMaybe :: Maybe Integer -> Maybe Integer -> Maybe Integer
addMaybe mA mB = (+) <$> mA <*> mB

minMaybe :: Maybe Integer -> Maybe Integer -> Maybe Integer
minMaybe Nothing a = a
minMaybe a Nothing = a
minMaybe (Just a) (Just b) = Just $ min a b

floydWarshall :: Int -> M.Map (Int, Int) Integer -> Array (Int, Int) Integer
floydWarshall labelCount ws = fmap fromJust $ runSTArray $ do
  arr <- newArray ((1, 1), (labelCount, labelCount)) Nothing

  forM_ (M.assocs ws) $ \((u, v), w') -> do
    writeArray arr (u, v) $ Just w'

  forM_ [1 .. labelCount] $ \v -> do
    writeArray arr (v, v) $ Just 0

  forM_ [1 .. labelCount] $ \k -> do
    forM_ [1 .. labelCount] $ \i -> do
      forM_ [1 .. labelCount] $ \j -> do
        w' <- readArray arr (i, j)
        w'' <- addMaybe <$> readArray arr (i, k) <*> readArray arr (k, j)
        writeArray arr (i, j) $ minMaybe w' w''

  return arr

makeCostFunction :: [ValveAndTunnels] -> CostFunction
makeCostFunction vats = costFn
  where
    labelIndexMap = M.fromList $ zip (map valveName vats) [1 :: Int ..]
    labelCount = length vats

    go :: ValveName -> (Integer, ValveName) -> ((Int, Int), Integer)
    go vn (cost, vn') = ((labelIndexMap M.! vn, labelIndexMap M.! vn'), cost)

    ws :: M.Map (Int, Int) Integer
    ws = M.fromList $ concatMap (\vat -> map (go $ valveName vat) $ tunnelsTo vat) vats

    floydWarshallArray = floydWarshall labelCount ws

    costFn :: CostFunction
    costFn prevVn vn = floydWarshallArray A.! (labelIndexMap M.! prevVn, labelIndexMap M.! vn)

makeFlowRateFunction :: M.Map ValveName ValveAndTunnels -> ValveName -> Integer
makeFlowRateFunction m vn =
  fromMaybe (error $ "unknown valve name " ++ LT.unpack vn) $
    preview (at vn % _Just % #flowRate) m

startingValve :: LT.Text
startingValve = "AA"

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let nonZeroFlowRateValves = S.fromList $ map valveName $ filter ((> 0) . flowRate) input
      flowRateFn = makeFlowRateFunction (valveAndTunnelsAsMap input)
      costFn = makeCostFunction input

  let xs = enumerateOptions nonZeroFlowRateValves flowRateFn costFn 30 startingValve

  print $ fst $ maximumBy (comparing fst) xs

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let nonZeroFlowRateValves = S.fromList $ map valveName $ filter ((> 0) . flowRate) input
      flowRateFn = makeFlowRateFunction (valveAndTunnelsAsMap input)
      costFn = makeCostFunction input

  let xs = enumerateOptions nonZeroFlowRateValves flowRateFn costFn 26 startingValve
      ys =
        filter (\((_, a), (_, b)) -> S.disjoint a b) $
          pairings $
            over (mapped % _2) (S.delete startingValve) xs

      ys' = map (\((aP, a), (bP, b)) -> (aP + bP, (a, b))) ys

  print $ fst $ maximumBy (comparing fst) ys'
