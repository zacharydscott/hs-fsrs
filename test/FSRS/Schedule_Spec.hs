{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module FSRS.Schedule_Spec (spec) where

import Test.Hspec
import Data.Time.Calendar (fromGregorian)
import FSRS.Schedule
import FSRS.Card (Card(..), CardState(..), mkCard)
import FSRS.Rating (Rating(..))
import FSRS.Parameters
import Data.Aeson
import Data.Time (UTCTime(..))
import FSRS.ReviewLog

spec :: Spec
spec = do
  describe "FSRS Scheduler JSON serialization" $ do
    it "should serialize/deserialize to JSON correctly when last review exists" $ do
      let parameters = Parameters 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
      let scheduler = Scheduler parameters 0.8 [60, 120] [100] 50
          schedulerExpectedJSON = object
            [ "parameters" .= [ Number 0, Number 1, Number 2, Number 3, Number 4, Number 5, Number 6,
              Number 7, Number 8, Number 9, Number 10, Number 11, Number 12, Number 13, Number 14,
              Number 15, Number 16, Number 17, Number 18, Number 19, Number 20]
            , "desiredRetention" .= Number 0.8
            , "learningSteps" .= [Number 60, Number 120]
            , "relearningSteps" .= [Number 100]
            , "maximumInterval" .= Number 50
            ]
      toJSON scheduler `shouldBe` schedulerExpectedJSON
      fromJSON schedulerExpectedJSON `shouldBe` Success scheduler
      let encodedScheduler = encode scheduler
      eitherDecode encodedScheduler `shouldBe` Right scheduler
    it "fails to deserialize if `parameters` list has wrong length" $ do
      let badScheduleJSON = object
            [ "parameters"       .= [Number 0, Number 1, Number 2]  -- too short
            , "desiredRetention" .= (0.5 :: Double)
            , "learningSteps"    .= [60 :: Int]
            , "relearningSteps"  .= [30 :: Int]
            , "maximumInterval"  .= (10 :: Int)
            ]
      (fromJSON badScheduleJSON :: Result Scheduler) `shouldSatisfy` \r -> case r of
            Error _ -> True
            Success _ -> False
  describe "FSRS Scheduler reviews (hard coded values compared to Python implementation output)" $ do

    describe "New Card Updates" $ do
      let newCard' = mkCard 1 New -- apostrophe since `newCard` is a function
          reviewTime = UTCTime (fromGregorian 2022 11 29) 0
          review = reviewCardAtTime defaultScheduler 0

      it "should update new cards reviewed with again" $ do
        let (newCardAgain, newAgLogain) = review newCard' Again reviewTime
        newCardAgain `shouldBe` Card 
          { cardId = 1
          , cardState = Learning
          , cardDue = UTCTime (fromGregorian 2022 11 29) (1*60)
          , cardLastReview = Just reviewTime
          , cardStep = 0
          , cardLapses = 0
          , cardRepetitions = 1
          , cardStability = 0.2172
          , cardDifficulty = 7.0114
          }
        newAgLogain `shouldBe` ReviewLog 1 Again reviewTime 0

      it "should update new cards reviewed with Hard" $ do
        let (newCardHard, newLogHard) = review newCard' Hard reviewTime
        newCardHard `shouldBe` Card 
          { cardId = 1
          , cardState = Learning
          , cardDue = UTCTime (fromGregorian 2022 11 29) (5*60 + 30)
          , cardLastReview = Just reviewTime
          , cardStep = 0
          , cardLapses = 0
          , cardRepetitions = 1
          , cardStability = 1.1771
          , cardDifficulty = 6.243132948566265
          }
        newLogHard `shouldBe` ReviewLog 1 Hard reviewTime 0

      it "should update new cards reviewed with Good" $ do
        let (newCardGood, newLogGood) = review newCard' Good reviewTime
        newCardGood `shouldBe` Card 
          { cardId = 1
          , cardState = Learning
          , cardDue = UTCTime (fromGregorian 2022 11 29) (10*60)
          , cardLastReview = Just reviewTime
          , cardStep = 1
          , cardLapses = 0
          , cardRepetitions = 1
          , cardStability = 3.2602
          , cardDifficulty = 4.884631634813845
          }
        newLogGood `shouldBe` ReviewLog 1 Good reviewTime 0

      it "should update new cards reviewed with Easy" $ do
        let (newCardEasy, newLogEasy) = review newCard' Easy reviewTime
        newCardEasy `shouldBe` Card 
          { cardId = 1
          , cardState = Reviewing
          , cardDue = UTCTime (fromGregorian 2022 12 15) 0
          , cardLastReview = Just reviewTime
          , cardStep = 0
          , cardLapses = 0
          , cardRepetitions = 1
          , cardStability = 16.1507
          , cardDifficulty = 2.482438522375996
          }
        newLogEasy `shouldBe` ReviewLog 1 Easy reviewTime 0

    describe "Learning Card Updates" $ do
      let reviewTime = UTCTime (fromGregorian 2022 11 29) (10*60)
          reviewtimePlus1day = UTCTime (fromGregorian 2022 11 30) (10*60)
          reviewTimeMinus5min = UTCTime (fromGregorian 2022 11 29) (5*60)
          learningCard = Card
            { cardId = 2
            , cardState = Learning
            , cardDue = reviewTime
            , cardLastReview = Just $ UTCTime (fromGregorian 2022 11 29) 0
            , cardStep = 1
            , cardLapses = 0
            , cardRepetitions = 1
            , cardStability = 3.2602
            , cardDifficulty = 4.884631634813845
            }
          review = reviewCardAtTime defaultScheduler 0

      it "should update new cards reviewed with again" $ do
        let (learningCardAgain, learningLogAgain) = review learningCard Again reviewTime
        learningCardAgain `shouldBe` Card 
          { cardId = 2
          , cardState = Learning
          , cardDue = UTCTime (fromGregorian 2022 11 29) (11*60)
          , cardLastReview = Just reviewTime
          , cardStep = 0
          , cardLapses = 0
          , cardRepetitions = 2
          , cardStability = 0.7833819093752047
          , cardDifficulty = 7.234918643089044
          }
        learningLogAgain `shouldBe` ReviewLog 2 Again reviewTime 0

      it "should update new cards reviewed with Hard" $ do
        let (learningCardHard, learningLogHard) = review learningCard Hard reviewTime
        learningCardHard `shouldBe` Card 
          { cardId = 2
          , cardState = Learning
          , cardDue = UTCTime (fromGregorian 2022 11 29) (20*60)
          , cardLastReview = Just reviewTime
          , cardStep = 1
          , cardLapses = 0
          , cardRepetitions = 2
          , cardStability = 1.6644005848819015
          , cardDifficulty = 6.051487572713533
          }
        learningLogHard `shouldBe` ReviewLog 2 Hard reviewTime 0

      it "should update learning cards reviewed with Good" $ do
        let (learningCardGood, learningLogGood) = review learningCard Good reviewTime
        learningCardGood `shouldBe` Card
          { cardId          = 2
          , cardState       = Reviewing
          , cardDue         = UTCTime (fromGregorian 2022 12 3) (10*60)
          , cardLastReview  = Just reviewTime
          , cardStep        = 0
          , cardLapses      = 0
          , cardRepetitions = 2
          , cardStability   = 3.536243655619573
          , cardDifficulty  = 4.868056502338024
          }
        learningLogGood `shouldBe` ReviewLog 2 Good reviewTime 0

      it "should update learning cards reviewed with Easy" $ do
        let (learningCardEasy, learningLogEasy) = review learningCard Easy reviewTime
        learningCardEasy `shouldBe` Card
          { cardId          = 2
          , cardState       = Reviewing
          , cardDue         = UTCTime (fromGregorian 2022 12 7) (10*60)
          , cardLastReview  = Just reviewTime
          , cardStep        = 0
          , cardLapses      = 0
          , cardRepetitions = 2
          , cardStability   = 7.513226867074781
          , cardDifficulty  = 3.6846254319625134
          }
        learningLogEasy `shouldBe` ReviewLog 2 Easy reviewTime 0

      it "should update learning cards reviewed with Easy, 5 minutes before due" $ do
        let (learningCardEasy, learningLogEasy) = review learningCard Easy reviewTimeMinus5min
        learningCardEasy `shouldBe` Card
          { cardId          = 2
          , cardState       = Reviewing
          , cardDue         = UTCTime (fromGregorian 2022 12 7) (5*60)
          , cardLastReview  = Just reviewTimeMinus5min
          , cardStep        = 0
          , cardLapses      = 0
          , cardRepetitions = 2
          , cardStability   = 7.513226867074781
          , cardDifficulty  = 3.6846254319625134
          }
        learningLogEasy `shouldBe` ReviewLog 2 Easy reviewTimeMinus5min 0

      it "should update learning cards reviewed with Easy, 1 day after due" $ do
        let (learningCardEasy, learningLogEasy) = review learningCard Easy reviewtimePlus1day
        learningCardEasy `shouldBe` Card
          { cardId          = 2
          , cardState       = Reviewing
          , cardDue         = UTCTime (fromGregorian 2022 12 13) (10*60)
          , cardLastReview  = Just reviewtimePlus1day
          , cardStep        = 0
          , cardLapses      = 0
          , cardRepetitions = 2
          , cardStability   = 12.72534030413839
          , cardDifficulty  = 3.6846254319625134
          }
        learningLogEasy `shouldBe` ReviewLog 2 Easy reviewtimePlus1day 0

  describe "Reviewing Card Updates" $ do
    let reviewTime      = UTCTime (fromGregorian 2022 12 7) (10*60)
        reviewingCard = Card
          { cardId          = 2
          , cardState       = Reviewing
          , cardDue         = reviewTime
          , cardLastReview  = Just $ UTCTime (fromGregorian 2022 11 29) (10*60)
          , cardStep        = 0
          , cardLapses      = 0
          , cardRepetitions = 2
          , cardStability   = 7.51322686707478
          , cardDifficulty  = 3.6846254319625134
          }
        review = reviewCardAtTime defaultScheduler 0

    it "should update reviewing cards reviewed with Again" $ do
      let (reviewingCardAgain, reviewingLogAgain) = review reviewingCard Again reviewTime
      reviewingCardAgain `shouldBe` Card
        { cardId          = 2
        , cardState       = Relearning
        , cardDue         = UTCTime (fromGregorian 2022 12  7) (20*60)
        , cardLastReview  = Just reviewTime
        , cardStep        = 0
        , cardLapses      = 1
        , cardRepetitions = 3
        , cardStability   = 1.9338559181758825
        , cardDifficulty  = 6.598430942407233
        }
      reviewingLogAgain `shouldBe` ReviewLog 2 Again reviewTime 0

    it "should update reviewing cards reviewed with Hard" $ do
      let (reviewingCardHard, reviewingLogHard) = review reviewingCard Hard reviewTime
      reviewingCardHard `shouldBe` Card
        { cardId          = 2
        , cardState       = Reviewing
        , cardDue         = UTCTime (fromGregorian 2022 12 19) (10*60)
        , cardLastReview  = Just reviewTime
        , cardStep        = 0
        , cardLapses      = 0
        , cardRepetitions = 3
        , cardStability   = 12.48314473820678
        , cardDifficulty  = 5.137380642346799
        }
      reviewingLogHard `shouldBe` ReviewLog 2 Hard reviewTime 0

    it "should update reviewing cards reviewed with Good" $ do
      let (reviewingCardGood, reviewingLogGood) = review reviewingCard Good reviewTime
      reviewingCardGood `shouldBe` Card
        { cardId          = 2
        , cardState       = Reviewing
        , cardDue         = UTCTime (fromGregorian 2023 1 6) (10*60)
        , cardLastReview  = Just reviewTime
        , cardStep        = 0
        , cardLapses      = 0
        , cardRepetitions = 3
        , cardStability   = 30.19655809086301
        , cardDifficulty  = 3.676330342286366
        }
      reviewingLogGood `shouldBe` ReviewLog 2 Good reviewTime 0

    it "should update reviewing cards reviewed with Easy" $ do
      let (reviewingCardEasy, reviewingLogEasy) = review reviewingCard Easy reviewTime
      reviewingCardEasy `shouldBe` Card
        { cardId          = 2
        , cardState       = Reviewing
        , cardDue         = UTCTime (fromGregorian 2023 2 21) (10*60)
        , cardLastReview  = Just reviewTime
        , cardStep        = 0
        , cardLapses      = 0
        , cardRepetitions = 3
        , cardStability   = 75.57229387092897
        , cardDifficulty  = 2.2152800422259333
        }
      reviewingLogEasy `shouldBe` ReviewLog 2 Easy reviewTime 0

  describe "Relearning Card Updates" $ do
    let reviewTime      = UTCTime (fromGregorian 2022 12  7) (20*60)
        relearningCard = Card
          { cardId          = 2
          , cardState       = Relearning
          , cardDue         = UTCTime (fromGregorian 2022 12 7) (20*60)
          , cardLastReview  = Just $ UTCTime (fromGregorian 2022 12 7) (10*60)
          , cardStep        = 0
          , cardLapses      = 1
          , cardRepetitions = 3
          , cardStability   = 1.9338559181758823
          , cardDifficulty  = 6.598430942407233
          }
        review = reviewCardAtTime defaultScheduler 0

    it "should update reviewing cards reviewed with Again" $ do
      let (relearningCardAgain, relearningLogAgain) = review relearningCard Again reviewTime
      relearningCardAgain `shouldBe` Card
        { cardId          = 2
        , cardState       = Relearning
        , cardDue         = UTCTime (fromGregorian 2022 12  7) (30*60)
        , cardLastReview  = Just reviewTime
        , cardStep        = 0
        , cardLapses      = 1
        , cardRepetitions = 4
        , cardStability   = 0.500895882313634
        , cardDifficulty  = 8.143924095001145
        }
      relearningLogAgain `shouldBe` ReviewLog 2 Again reviewTime 0

    it "should update reviewing cards reviewed with Hard" $ do
      let (relearningCardHard, relearningLogHard) = review relearningCard Hard reviewTime
      relearningCardHard `shouldBe` Card
        { cardId          = 2
        , cardState       = Relearning
        , cardDue         = UTCTime (fromGregorian 2022 12 7) (35*60)
        , cardLastReview  = Just reviewTime
        , cardStep        = 0
        , cardLapses      = 1
        , cardRepetitions = 4
        , cardStability   = 1.0642208985304098
        , cardDifficulty  = 7.35697734485508
        }
      relearningLogHard `shouldBe` ReviewLog 2 Hard reviewTime 0

    it "should update reviewing cards reviewed with Good" $ do
      let (relearningCardGood, relearningLogGood) = review relearningCard Good reviewTime
      relearningCardGood `shouldBe` Card
        { cardId          = 2
        , cardState       = Reviewing
        , cardDue         = UTCTime (fromGregorian 2022 12 9) (20*60)
        , cardLastReview  = Just reviewTime
        , cardStep        = 0
        , cardLapses      = 1
        , cardRepetitions = 4
        , cardStability   = 2.2610809169313972
        , cardDifficulty  = 6.570030594709017
        }
      relearningLogGood `shouldBe` ReviewLog 2 Good reviewTime 0

    it "should update reviewing cards reviewed with Easy" $ do
      let (relearningCardEasy, relearningLogEasy) = review relearningCard Easy reviewTime
      relearningCardEasy `shouldBe` Card
        { cardId          = 2
        , cardState       = Reviewing
        , cardDue         = UTCTime (fromGregorian 2022 12 12) (20*60)
        , cardLastReview  = Just reviewTime
        , cardStep        = 0
        , cardLapses      = 1
        , cardRepetitions = 4
        , cardStability   = 4.803971543850715
        , cardDifficulty  = 5.783083844562954
        }
      relearningLogEasy `shouldBe` ReviewLog 2 Easy reviewTime 0
