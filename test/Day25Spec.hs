module Day25Spec where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as LT
import qualified Day25
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Text.Megaparsec (eof, parseMaybe)

spec :: SpecWith ()
spec = do
  let snafuNumberTestCases =
        [ (1, "1"),
          (2, "2"),
          (3, "1="),
          (4, "1-"),
          (5, "10"),
          (6, "11"),
          (7, "12"),
          (8, "2="),
          (9, "2-"),
          (10, "20"),
          (15, "1=0"),
          (20, "1-0"),
          (2022, "1=11-2"),
          (12345, "1-0---0"),
          (314159265, "1121-1110-1=0")
        ]

  describe "snafuNumberToInteger" $ do
    forM_ snafuNumberTestCases $ \(expected, snafuNumStr) -> do
      it ("converts " ++ snafuNumStr ++ " to " ++ show expected) $ do
        let snafuNum :: Day25.SnafuNumber
            snafuNum = fromJust $ parseMaybe (Day25.snafuNumberParser <* eof) $ LT.pack snafuNumStr

        Day25.snafuNumberToInteger snafuNum `shouldBe` expected

  describe "integerToSnafuNumber" $ do
    forM_ snafuNumberTestCases $ \(n, expectedSnafuNumStr) -> do
      it ("converts " ++ show n ++ " to " ++ expectedSnafuNumStr) $ do
        let snafuNum :: Day25.SnafuNumber
            snafuNum = fromJust $ parseMaybe (Day25.snafuNumberParser <* eof) $ LT.pack expectedSnafuNumStr

        Day25.integerToSnafuNumber n `shouldBe` snafuNum
