module LibSpec where

import Lib (addNumbers)
import Test.Hspec (SpecWith, describe, it, shouldBe)

spec :: SpecWith ()
spec = do
  describe "addNumbers" $ do
    it "works" $ do
      addNumbers (2 :: Integer) 3 `shouldBe` 5
