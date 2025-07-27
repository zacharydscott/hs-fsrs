{-# LANGUAGE OverloadedStrings #-}

module FSRS.Parameters_Spec (spec) where

import Data.Aeson
import Test.Hspec

import FSRS.Parameters
import Data.ByteString.Lazy (ByteString)
import Data.Either (isLeft)

-- May change this in the furute, it's very hardcoded
-- Note this is out of the default params constructor order
expectedDefaultParametersJSONString :: ByteString
expectedDefaultParametersJSONString =
  "[0.2172,1.1771,3.2602,16.1507,7.0114,0.57,2.0966,6.9e-3,1.5261,0.112,1.0178,\
  \1.849,0.1133,0.3127,2.2934,0.2191,3.0004,0.7536,0.3332,0.1437,0.2]"

spec :: Spec
spec = do
  describe "FSRS Parameters" $ do
    it "Should be the same value going to and from JSON" $ do
      Success defaultParameters `shouldBe` fromJSON (toJSON defaultParameters)
    it "Should have the expected JSON string form and parameter values" $ do
      encode defaultParameters `shouldBe` expectedDefaultParametersJSONString
    it "Should deserialize with the correct argument order from a json string" $ do 
      eitherDecode expectedDefaultParametersJSONString `shouldBe` Right defaultParameters
    it "fails to decode when the JSON array has the wrong length" $ do
         let tooShort = "[1,2,3]"
             tooLong  = "[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]"
         (eitherDecode tooShort :: Either String Parameters) `shouldSatisfy` isLeft
         (eitherDecode tooLong :: Either String Parameters) `shouldSatisfy` isLeft
