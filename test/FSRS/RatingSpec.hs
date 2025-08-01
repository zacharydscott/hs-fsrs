{-# LANGUAGE OverloadedStrings #-}

module FSRS.RatingSpec (spec) where

import Data.Aeson
import FSRS.Rating
import Test.Hspec

spec :: Spec
spec = do
  describe "fromRating" $ do
    it "maps Again → 1" $ fromRating Again `shouldBe` (1 :: Int)
    it "maps Hard  → 2" $ fromRating Hard `shouldBe` (2 :: Int)
    it "maps Good  → 3" $ fromRating Good `shouldBe` (3 :: Int)
    it "maps Easy  → 4" $ fromRating Easy `shouldBe` (4 :: Int)

  describe "toRating" $ do
    it "maps 1 → Just Again" $ toRating (1 :: Int) `shouldBe` Just Again
    it "maps 2 → Just Hard" $ toRating (2 :: Int) `shouldBe` Just Hard
    it "maps 3 → Just Good" $ toRating (3 :: Int) `shouldBe` Just Good
    it "maps 4 → Just Easy" $ toRating (4 :: Int) `shouldBe` Just Easy
    it "maps >4 → Nothing" $ toRating (43 :: Int) `shouldBe` Nothing
    it "maps negative → Nothing" $ toRating (-2 :: Int) `shouldBe` Nothing

  describe "ToJSON instance" $ do
    it "encodes Again as Number 1" $
      encode Again `shouldBe` "1"
    it "encodes Hard  as Number 2" $
      encode Hard `shouldBe` "2"
    it "encodes Good  as Number 3" $
      encode Good `shouldBe` "3"
    it "encodes Easy  as Number 4" $
      encode Easy `shouldBe` "4"

  describe "FromJSON instance" $ do
    it "decodes Number 1 to Again" $
      eitherDecode "1" `shouldBe` Right Again
    it "decodes Number 2 to Hard" $
      eitherDecode "2" `shouldBe` Right Hard
    it "decodes Number 3 to Good" $
      eitherDecode "3" `shouldBe` Right Good
    it "decodes Number 4 to Easy" $
      eitherDecode "4" `shouldBe` Right Easy

    it "fails to decode out‐of‐range numbers" $ do
      let err = eitherDecode "7" :: Either String Rating
      case err of
        Left msg -> msg `shouldContain` "Invalid Review Rating value"
        Right _ -> expectationFailure "Expected failure, but got Right"

    it "fails to decode non‐numeric JSON" $ do
      let err = eitherDecode "\"foo\"" :: Either String Rating
      case err of
        Left msg -> msg `shouldContain` "parsing FSRS Review Rating"
        Right _ -> expectationFailure "Expected failure, but got Right"
