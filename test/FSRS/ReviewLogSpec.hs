{-# LANGUAGE OverloadedStrings #-}

module FSRS.ReviewLogSpec (spec) where

import Data.Aeson
import Data.Time
import Data.Time.Format.ISO8601
import FSRS.Rating (Rating (..))
import FSRS.ReviewLog
import Test.Hspec

exampleTime :: UTCTime
exampleTime = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

exampleDuration :: NominalDiffTime
exampleDuration = realToFrac (5 :: Double)

testReviewLog :: ReviewLog
testReviewLog =
  ReviewLog
    { revLogCardId = 9,
      revLogRating = Good,
      revLogReviewDatetime = exampleTime,
      revLogReviewDuration = exampleDuration
    }

expectedReviewLogJSON :: Value
expectedReviewLogJSON =
  object
    [ "card_id" .= Number 9,
      "rating" .= Number 3,
      "review_datetime" .= iso8601Show exampleTime,
      "review_duration" .= Number 5.0
    ]

spec :: Spec
spec = do
  describe "FSRS.ReviewLog JSON instances" $ do
    it "serializes ReviewLog to JSON correctly" $ do
      toJSON testReviewLog `shouldBe` expectedReviewLogJSON

    it "deserializes JSON to ReviewLog correctly" $ do
      fromJSON expectedReviewLogJSON `shouldBe` Success testReviewLog

    it "is the same after serializing and deserializing" $ do
      let encoded = encode testReviewLog
      eitherDecode encoded `shouldBe` Right testReviewLog

    it "fails to parse when a field has the wrong type" $ do
      let bad =
            object
              [ "card_id" .= String "nine",
                "rating" .= Number 3,
                "review_datetime" .= iso8601Show exampleTime,
                "review_duration" .= Number 5.0
              ]
      case (fromJSON bad :: Result ReviewLog) of
        Error _ -> pure ()
        Success _ -> expectationFailure "Expected failure on wrong field type"
