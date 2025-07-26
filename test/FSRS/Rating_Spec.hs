{-# LANGUAGE OverloadedStrings #-}

module FSRS.Rating_Spec(spec) where

import Data.Aeson
import Test.Hspec
import FSRS.Rating

spec :: Spec
spec = do
  describe "fromRating" $ do
    it "maps Again → 0"  $ fromRating Again  `shouldBe` (0 :: Int)
    it "maps Hard  → 1"  $ fromRating Hard   `shouldBe` (1 :: Int)
    it "maps Good  → 2"  $ fromRating Good   `shouldBe` (2 :: Int)
    it "maps Easy  → 3"  $ fromRating Easy   `shouldBe` (3 :: Int)

  describe "toRating" $ do
    it "maps 0 → Just Again" $ toRating (0 :: Int) `shouldBe` Just Again
    it "maps 1 → Just Hard"  $ toRating (1 :: Int) `shouldBe` Just Hard
    it "maps 2 → Just Good"  $ toRating (2 :: Int) `shouldBe` Just Good
    it "maps 3 → Just Easy"  $ toRating (3 :: Int) `shouldBe` Just Easy
    it "maps >3 → Nothing"   $ toRating (42 :: Int) `shouldBe` Nothing
    it "maps negative → Nothing" $ toRating (-1 :: Int) `shouldBe` Nothing

  describe "ToJSON instance" $ do
    it "encodes Again as Number 0" $
      encode Again `shouldBe` "0"
    it "encodes Hard  as Number 1" $
      encode Hard  `shouldBe` "1"
    it "encodes Good  as Number 2" $
      encode Good  `shouldBe` "2"
    it "encodes Easy  as Number 3" $
      encode Easy  `shouldBe` "3"

  describe "FromJSON instance" $ do
    it "decodes Number 0 to Again" $
      eitherDecode "0" `shouldBe` Right Again
    it "decodes Number 1 to Hard" $
      eitherDecode "1" `shouldBe` Right Hard
    it "decodes Number 2 to Good" $
      eitherDecode "2" `shouldBe` Right Good
    it "decodes Number 3 to Easy" $
      eitherDecode "3" `shouldBe` Right Easy

    it "fails to decode out‐of‐range numbers" $ do
      let err = eitherDecode "7" :: Either String Rating
      case err of
        Left msg -> msg `shouldContain` "Invalid Review Rating value"
        Right _  -> expectationFailure "Expected failure, but got Right"

    it "fails to decode non‐numeric JSON" $ do
      let err = eitherDecode "\"foo\"" :: Either String Rating
      case err of
        Left msg -> msg `shouldContain` "parsing FSRS Review Rating"
        Right _  -> expectationFailure "Expected failure, but got Right"
