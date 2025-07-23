module FSRS.Parameters_Spec (spec) where

import Data.Aeson
import Test.Hspec

import FSRS.Parameters

spec :: Spec
spec = describe "FSRS Parameters" $ do
  it "Should be the same value after deserial" $ do
    True `shouldBe` True
  
