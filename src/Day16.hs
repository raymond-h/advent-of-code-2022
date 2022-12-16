{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day16 (part1, part2, graphVisualization) where

import Common (Parser, char, decimal, lexeme, parseFromFile, sepBy1NonGreedy, string)
import Control.Monad (guard, void)
import Data.Bifunctor (bimap, second)
import Data.Foldable (fold, maximumBy, minimumBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Optics (at, folded, mapped, over, preview, toListOf, (%), _1, _2, _Just)
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

startingValve :: LT.Text
startingValve = "AA"

valveAndTunnelsAsMap :: [ValveAndTunnels] -> M.Map LT.Text ValveAndTunnels
valveAndTunnelsAsMap vats = M.fromList $ map (\vat -> (valveName vat, vat)) vats

simplifyInput :: M.Map LT.Text ValveAndTunnels -> M.Map LT.Text ValveAndTunnels
simplifyInput m
  | m == m' = M.filter (\vat -> valveName vat == startingValve || flowRate vat /= 0) m
  | otherwise = simplifyInput m'
  where
    m' = M.mapWithKey simplifyEntry m

    simplifyEntry :: ValveName -> ValveAndTunnels -> ValveAndTunnels
    simplifyEntry vn = over #tunnelsTo (concatMap (replacementTunnels vn))

    replacementTunnels :: ValveName -> (Integer, ValveName) -> [(Integer, ValveName)]
    replacementTunnels vn orig@(cost, vn')
      | shouldFilterOut vn' =
          filter ((/= vn) . snd) $
            map (over _1 (+ cost)) $
              toListOf (at vn' % _Just % #tunnelsTo % folded) m
      | otherwise = [orig]

    shouldFilterOut vn = vn /= startingValve && preview (at vn % _Just % #flowRate) m == Just 0

pathsBetween :: M.Map LT.Text ValveAndTunnels -> ValveName -> ValveName -> [[(Integer, ValveName)]]
pathsBetween m vn vn'
  | vn == vn' = return [(0, vn)]
  | otherwise = map ((0, vn) :) $ inner vn (S.singleton vn) $ neighbors vn
  where
    neighbors :: ValveName -> [(Integer, ValveName)]
    neighbors vn'' = toListOf (at vn'' % _Just % #tunnelsTo % folded) m

    inner :: ValveName -> S.Set ValveName -> [(Integer, ValveName)] -> [[(Integer, ValveName)]]
    inner _ _ [] = []
    inner prev visited candidates = do
      orig@(_, c) <- candidates

      if c == vn'
        then return [orig]
        else do
          let candidates' = filter (flip S.notMember visited . snd) $ filter ((/= prev) . snd) $ neighbors c

          p <- inner c (S.insert c visited) candidates'

          return $ orig : p

shortestPathBetween :: M.Map LT.Text ValveAndTunnels -> ValveName -> ValveName -> [(Integer, ValveName)]
shortestPathBetween m vn vn' = minimumBy (comparing (sum . map fst)) $ pathsBetween m vn vn'

type CostFunction = ValveName -> ValveName -> Integer

flowRateOf :: M.Map LT.Text ValveAndTunnels -> ValveName -> Integer
flowRateOf m vn =
  fromMaybe (error $ "unknown valve name " ++ LT.unpack vn) $
    preview (at vn % _Just % #flowRate) m

enumerateOptions :: M.Map LT.Text ValveAndTunnels -> CostFunction -> Integer -> ValveName -> [(Integer, S.Set ValveName)]
enumerateOptions m costFn timeLeft startingValve' =
  map (second (S.insert startingValve')) $
    inner timeLeft startingValve' (S.delete startingValve' $ M.keysSet m)
  where
    flowRateOf' = flowRateOf m

    keepHighest :: [(Integer, S.Set ValveName)] -> [(Integer, S.Set ValveName)]
    keepHighest rs = map swap $ M.assocs $ M.fromListWith max $ map swap rs

    inner :: Integer -> ValveName -> S.Set ValveName -> [(Integer, S.Set ValveName)]
    inner _ _ s | S.null s = return (0, S.empty)
    inner timeLeftInner prevVn vats = do
      vn <- S.toList vats

      let timeLeftInner' = timeLeftInner - costFn prevVn vn - 1
          vats' = S.delete vn vats
          releasedPressure = timeLeftInner' * flowRateOf' vn

      if timeLeftInner' > 0
        then do
          x <- (0, S.empty) : keepHighest (inner timeLeftInner' vn vats')
          -- let x = maximumBy (comparing fst) $ inner timeLeftInner' vn vats'

          return $ bimap (+ releasedPressure) (S.insert vn) x
        else return (0, S.empty)

pairings :: (Eq b) => [b] -> [(b, b)]
pairings as = do
  a <- as
  b <- as
  guard $ a /= b
  return (a, b)

part1 :: FilePath -> IO ()
part1 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let inputMap = valveAndTunnelsAsMap input
      inputMap' = simplifyInput inputMap

      costMap =
        M.fromList $
          map (\x -> (x, sum $ map fst $ uncurry (shortestPathBetween inputMap') x)) $
            pairings $
              M.keys inputMap'

      costFn :: CostFunction
      costFn prevVn vn = costMap M.! (prevVn, vn)

  let xs = enumerateOptions inputMap' costFn 30 startingValve

  print $ fst $ maximumBy (comparing fst) xs

part2 :: FilePath -> IO ()
part2 inputPath = do
  input <- parseFromFile (inputParser <* eof) inputPath

  let inputMap = valveAndTunnelsAsMap input
      inputMap' = simplifyInput inputMap

      costMap =
        M.fromList $
          map (\x -> (x, sum $ map fst $ uncurry (shortestPathBetween inputMap') x)) $
            pairings $
              M.keys inputMap'

      costFn :: CostFunction
      costFn prevVn vn = costMap M.! (prevVn, vn)

  let xs = enumerateOptions inputMap' costFn 26 startingValve
      ys =
        filter (\((_, a), (_, b)) -> S.disjoint a b) $
          pairings $
            over (mapped % _2) (S.delete startingValve) xs

      ys' = map (\((aP, a), (bP, b)) -> (aP + bP, (a, b))) ys

  print $ fst $ maximumBy (comparing fst) ys'
