module Data.Excel.FormPopulateSpec (main, spec) where

import Test.Hspec
import Data.Excel.FormPopulate


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "selectListIncremental" $ do
    it "should get data at an incremental rate" $ do
          rslt <- selectListIncremental 100 [OnpingTagHistoryPid ==. (Just 299)] []
          (count rslt) `shouldBe` 100

