{-# LANGUAGE OverloadedStrings #-}

module FSRS.Card_Spec (spec) where

import Data.Aeson
import Data.Time
import Data.Time.Format.ISO8601
import Data.ByteString.Lazy
import Test.Hspec

import FSRS.Card

exampleTime1 :: UTCTime
exampleTime1 = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

exampleTime2 :: UTCTime
exampleTime2 = UTCTime (fromGregorian 1970 1 2) (secondsToDiffTime 0)

testCard :: Card
testCard = Card
  { cardState = New
  , cardDue = exampleTime2
  , cardLastReview = exampleTime1
  , cardElapsedDays = 0
  , cardLapses = 1
  , cardStep = 2
  , cardRepetitions = 3
  , cardStability = 0.5
  , cardDifficulty = 0.6
  }
expectedObject :: Value
expectedObject = object
  [ "state" .= Number 0
  , "due" .= iso8601Show exampleTime2
  , "lastReview" .= iso8601Show exampleTime1
  , "elapsedDays" .= Number 0
  , "lapses" .= Number 1
  , "step" .= Number 2
  , "repetitions" .= Number 3
  , "stability" .= Number 0.5
  , "difficulty" .= Number 0.6
  ]

spec :: Spec
spec = do
  describe "FSRS Card State" $ do
    it "should serialize each CardState to the correct number in JSON" $ do
      toJSON New `shouldBe` Number 0
      toJSON Learning `shouldBe` Number 1
      toJSON Reviewing `shouldBe` Number 2
      toJSON Relearning `shouldBe` Number 3
    it "should deserialize numbers from JSON to the correct CardStat" $ do
      eitherDecode "0" `shouldBe` Right New
      eitherDecode "1" `shouldBe` Right Learning
      eitherDecode "2" `shouldBe` Right Reviewing
      eitherDecode "3" `shouldBe` Right Relearning
  describe "FSRS Card" $ do
    it "should serialize Card data to JSON correctly" $ do
      toJSON testCard `shouldBe` expectedObject
      fromJSON expectedObject `shouldBe` Success testCard
    it "Should be the same after serializing and deserializing" $ do
      let encodedTestCard = encode testCard
      eitherDecode encodedTestCard `shouldBe` Right testCard
