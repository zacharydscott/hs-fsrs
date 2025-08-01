{-# LANGUAGE OverloadedStrings #-}

module FSRS.CardSpec (spec) where

import Data.Aeson
import Data.Either (isLeft)
import Data.Time
import Data.Time.Format.ISO8601
import FSRS.Card
import Test.Hspec

exampleTime1 :: UTCTime
exampleTime1 = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

exampleTime2 :: UTCTime
exampleTime2 = UTCTime (fromGregorian 1970 1 2) (secondsToDiffTime 0)

testReveiwingCard :: Card
testReveiwingCard =
  ActiveCard $
    CardDetails
      { cardId = 11,
        cardState = Reviewing,
        cardDue = exampleTime2,
        cardLastReview = exampleTime1,
        cardLapses = 1,
        cardStep = 2,
        cardRepetitions = 3,
        cardStability = 0.5,
        cardDifficulty = 0.6
      }

expectedReviewingCardJSONObject :: Value
expectedReviewingCardJSONObject =
  object
    [ "card_id" .= Number 11,
      "state" .= Number 2,
      "due" .= iso8601Show exampleTime2,
      "last_review" .= iso8601Show exampleTime1,
      "lapses" .= Number 1,
      "step" .= Number 2,
      "repetitions" .= Number 3,
      "stability" .= Number 0.5,
      "difficulty" .= Number 0.6
    ]

testNewCard :: Card
testNewCard = NewCard 22

blankDate :: UTCTime
blankDate = UTCTime (fromGregorian 1970 1 1) 0

expectedNewCardJSONObject :: Value
expectedNewCardJSONObject =
  object
    [ "card_id" .= Number 22,
      "state" .= Number 1,
      "due" .= iso8601Show blankDate,
      "step" .= Null,
      "last_review" .= Null,
      "stability" .= Null,
      "difficulty" .= Null
    ]

spec :: Spec
spec = do
  describe "FSRS Card State" $ do
    it "serialize each CardState to the correct number in JSON" $ do
      toJSON Learning `shouldBe` Number 1
      toJSON Reviewing `shouldBe` Number 2
      toJSON Relearning `shouldBe` Number 3
    it "deserialize numbers from JSON to the correct CardStat" $ do
      eitherDecode "1" `shouldBe` Right Learning
      eitherDecode "2" `shouldBe` Right Reviewing
    it "fail to deserialize out‑of‑range numbers" $ do
      (eitherDecode "4" :: Either String CardState) `shouldSatisfy` isLeft
      (eitherDecode "-1" :: Either String CardState) `shouldSatisfy` isLeft

  describe "FSRS Card" $ do
    it "serialize/deserialize Card data to JSON correctly when last review exists" $ do
      toJSON testReveiwingCard `shouldBe` expectedReviewingCardJSONObject
      fromJSON expectedReviewingCardJSONObject `shouldBe` Success testReveiwingCard
    it "is the same after serializing and deserializing when last review exists" $ do
      let encodedTestReveiwingCard = encode testReveiwingCard
      eitherDecode encodedTestReveiwingCard `shouldBe` Right testReveiwingCard
    it "serialize/deserialize Card data to JSON correctly when last review doesn't exists" $ do
      toJSON testNewCard `shouldBe` expectedNewCardJSONObject
      fromJSON expectedNewCardJSONObject `shouldBe` Success testNewCard
    it "be the same after serializing and deserializing when last review doesn't exists" $ do
      let encodedTestNewCard = encode testNewCard
      eitherDecode encodedTestNewCard `shouldBe` Right testNewCard
    it "fail to parse a JSON with wrong field types" $ do
      -- put a string where a number is expected
      let bad2 =
            object
              [ "card_id" .= String "eleven",
                "state" .= Number 0,
                "due" .= iso8601Show exampleTime2,
                "last_review" .= iso8601Show exampleTime1,
                "lapses" .= Number 1,
                "step" .= Number 2,
                "repetitions" .= Number 3,
                "stability" .= Number 0.5,
                "difficulty" .= Number 0.6
              ]
      case (fromJSON bad2 :: Result Card) of
        Error _ -> pure ()
        Success _ -> expectationFailure "Expected failure on wrong field type"
