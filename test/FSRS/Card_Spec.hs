{-# LANGUAGE OverloadedStrings #-}

module FSRS.Card_Spec (spec) where

import Data.Aeson
import Data.Time
import Data.Time.Format.ISO8601
import Test.Hspec

import FSRS.Card
import Data.Either (isLeft)

exampleTime1 :: Maybe UTCTime
exampleTime1 = Just $ UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

exampleTime2 :: UTCTime
exampleTime2 = UTCTime (fromGregorian 1970 1 2) (secondsToDiffTime 0)

testCard :: Card
testCard = Card
  { cardId = 11
  , cardState = New
  , cardDue = exampleTime2
  , cardLastReview = exampleTime1
  , cardLapses = 1
  , cardStep = 2
  , cardRepetitions = 3
  , cardStability = 0.5
  , cardDifficulty = 0.6
  }

expectedObject :: Value
expectedObject = object
  [ "id" .= Number 11
  , "state" .= Number 0
  , "due" .= iso8601Show exampleTime2
  , "lastReview" .= maybe "" iso8601Show exampleTime1
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
    it "should fail to deserialize out‑of‑range numbers" $ do
      (eitherDecode "4" :: Either String CardState) `shouldSatisfy` isLeft
      (eitherDecode "-1" :: Either String CardState) `shouldSatisfy` isLeft

  describe "FSRS Card" $ do
    it "should serialize Card data to JSON correctly" $ do
      toJSON testCard `shouldBe` expectedObject
      fromJSON expectedObject `shouldBe` Success testCard
    it "Should be the same after serializing and deserializing" $ do
      let encodedTestCard = encode testCard
      eitherDecode encodedTestCard `shouldBe` Right testCard
    it "should fail to parse a JSON with wrong field types" $ do
      -- put a string where a number is expected
      let bad2 = object
            [ "id"        .= String "eleven"
            , "state"     .= Number 0
            , "due"       .= iso8601Show exampleTime2
            , "lastReview".= maybe "" iso8601Show exampleTime1
            , "lapses"    .= Number 1
            , "step"      .= Number 2
            , "repetitions".= Number 3
            , "stability" .= Number 0.5
            , "difficulty".= Number 0.6
            ]
      case (fromJSON bad2 :: Result Card) of
        Error _ -> pure ()
        Success _ -> expectationFailure "Expected failure on wrong field type"
