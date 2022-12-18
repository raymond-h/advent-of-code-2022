{-# LANGUAGE ViewPatterns #-}

module MagicaVoxelXrawWriter where

import Common (tuplify)
import qualified Data.Array as A
import Data.Binary (Put, Word32, Word8)
import Data.Binary.Put (putByteString, putWord32le, putWord8, runPut)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S
import Linear (V3 (V3))

type Color = (Word8, Word8, Word8, Word8)

defaultColor :: Color
defaultColor = (0, 0, 0, 0)

type DataArray = A.Array (V3 Word32) (Maybe Color)

type IndexArray = A.Array (V3 Word32) Word8

dataToIndicesAndPalette :: DataArray -> (IndexArray, [Color])
dataToIndicesAndPalette cArr =
  let palette :: [Color]
      palette = S.toList $ S.fromList $ catMaybes $ toList cArr

      colorToIndexMap :: M.Map Color Word8
      colorToIndexMap = M.fromList $ zip palette [1 ..]

      iArr :: IndexArray
      iArr = fromMaybe 0 . (>>= flip M.lookup colorToIndexMap) <$> cArr
   in (iArr, palette)

padToMinimum :: Int -> a -> [a] -> [a]
padToMinimum n x xs
  | padLength < 0 = xs
  | otherwise = xs ++ padding
  where
    padLength = n - length xs
    padding = replicate padLength x

putXrawHeader :: V3 Word32 -> Put
putXrawHeader (tuplify -> (w, h, d)) = do
  putByteString "XRAW"
  putWord8 0 -- color channel data type - unsigned integer
  putWord8 4 -- num of color channels - RGBA
  putWord8 8 -- bits per channel
  putWord8 8 -- bits per index - 256 colors
  putWord32le w
  putWord32le h
  putWord32le d
  putWord32le 256 -- num of palette colors - 256

v3x :: V3 a -> a
v3x (V3 x _ _) = x

v3y :: V3 a -> a
v3y (V3 _ y _) = y

v3z :: V3 a -> a
v3z (V3 _ _ z) = z

arrayElementsInXYZOrder :: A.Array (V3 Word32) a -> [a]
arrayElementsInXYZOrder iArr = map snd $ sortBy (comparing (v3z . fst) <> comparing (v3y . fst) <> comparing (v3x . fst)) $ A.assocs iArr

putXrawIndexArray :: IndexArray -> Put
putXrawIndexArray = mapM_ putWord8 . arrayElementsInXYZOrder

putXrawPalette :: [Color] -> Put
putXrawPalette = mapM_ putColor
  where
    putColor (r, g, b, a) = mapM_ putWord8 [r, g, b, a]

putArrayAsXraw :: DataArray -> Put
putArrayAsXraw arr = putXrawHeader size >> putXrawIndexArray iArr >> putXrawPalette palette'
  where
    (minB, maxB) = A.bounds arr
    size = maxB - minB + pure 1

    (iArr, palette) = dataToIndicesAndPalette arr
    palette' = padToMinimum 256 defaultColor (defaultColor : palette)

toXrawBytestring :: DataArray -> LBS.ByteString
toXrawBytestring arr = runPut $ putArrayAsXraw arr
