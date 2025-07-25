{-# LANGUAGE OverloadedStrings #-}

module FSRS.Schedule_Spec (spec) where

import Test.Hspec
import Data.Time.Clock (UTCTime(..), secondsToDiffTime, NominalDiffTime, nominalDay)
import Data.Time.Calendar (fromGregorian)
import FSRS.Schedule
import FSRS.Card (Card(..), CardState(..))
import FSRS.Parameters (defaultParameters, difficultyMin, difficultyMax, stabilityMin)
import FSRS.Rating (Rating(..))

-- helper UTCTime: 2025-01-01T00:00:00Z
baseTime :: UTCTime
baseTime = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)

-- A minimal config for testing
testConfig :: SchedulingConfig
testConfig = SchedulingConfig
  { scParameters       = defaultParameters
  , scDesiredRetention = 0.9
  , scLearningSteps    = [60, 600]  -- 1 min, 10 min
  , scRelearningSteps  = [600]      -- 10 min
  , scMaximumInterval  = 365
  }

-- A fresh “new” card
newCard :: Card
newCard = Card
  { cardId         = 1
  , cardState      = New
  , cardDue        = baseTime
  , cardLastReview = Nothing
  , cardStep       = 0
  , cardLapses     = 0
  , cardRepetitions= 0
  , cardStability  = 0
  , cardDifficulty = 0
  }

spec :: Spec
spec = do
  describe "reviewCard" $ do
    it "Should be implemented" $ do
       True `shouldBe` True
