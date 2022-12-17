{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Common.Array where

import Common (the)
import Control.Monad (guard)
import Data.Array (Array, Ix, array, bounds, range)
import qualified Data.Array as A
import Data.List (intercalate)
import Linear (V2)
import Linear.V2 (V2 (V2))

mk2dArray :: forall i a. (Ix i, Integral i) => [[a]] -> Maybe (Array (V2 i) a)
mk2dArray = mk2dArrayWithBasis (pure 1)

mk2dArrayWithBasis :: forall i a. (Ix i, Integral i) => V2 i -> [[a]] -> Maybe (Array (V2 i) a)
mk2dArrayWithBasis basis@(V2 basisX basisY) xss = do
  let h = length xss
  w <- the $ map length xss
  let bounds' :: (V2 i, V2 i)
      bounds' = (basis, (fromInteger . toInteger <$> V2 w h) - pure 1 + basis)

      elems :: [(V2 i, a)]
      elems = concatMap go $ zip [toInteger basisX ..] xss

      go :: (Integer, [a]) -> [(V2 i, a)]
      go (y, as) = zipWith (\x a -> (fromInteger <$> V2 x y, a)) [toInteger basisY ..] as
  guard $ length elems == w * h
  return $ array bounds' elems

showArray :: forall i a. (Ix i) => (a -> Char) -> Array (V2 i) a -> String
showArray f arr = intercalate "\n" (map showLine ys)
  where
    (V2 xMin yMin, V2 xMax yMax) = bounds arr
    xs = range (xMin, xMax)
    ys = range (yMin, yMax)

    showLine :: i -> String
    showLine y = [f $ arr A.! V2 x y | x <- xs]

showArrayBasic :: (Ix i, Show a) => Array (V2 i) a -> String
showArrayBasic = showArray (head . show)

defaultArray :: Ix i => (i, i) -> e -> Array i e
defaultArray bounds' e = array bounds' (map (,e) (range bounds'))
