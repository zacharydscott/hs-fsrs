{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module FSRS.Schedule_QuickCheck_Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import FSRS.Schedule
import FSRS.Card   (CardState(..), mkCard, cardStability, cardDifficulty, cardDue, toCardState)
import FSRS.Rating (Rating(..))
import FSRS.Rating (toRating)
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))
import FSRS.Card
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import FSRS.ReviewLog
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck.Monadic (monadicIO, run, PropertyM)
import System.Process (readProcess, readCreateProcessWithExitCode, proc)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson (encode, eitherDecode, ToJSON, FromJSON)
import Data.Time.Format.ISO8601 (ISO8601(iso8601Format))
import Data.Time (defaultTimeLocale, formatTime)
import FSRS.Parameters (stabilityMin, difficultyMin, difficultyMax)
import Control.Monad (void)
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)

instance Arbitrary Rating where
  arbitrary = (choose (1, 4) :: Gen Int) <&> (fromMaybe Again . toRating)

instance Arbitrary CardState where
  arbitrary = (choose (0, 3) :: Gen Int) <&> (fromMaybe New . toCardState)

-- no need to randomly geneary the id, and some other properties and lastReviewTime is hard coded
-- This is only needed for this test suite
instance Arbitrary Card where
  arbitrary = do
    state <- arbitrary
    step <- if state == Learning
      then chooseInt (0,1)
      else pure 0
    stability <- max stabilityMin . abs <$> arbitrary
    difficulty <- choose (difficultyMin, difficultyMax)
    let lastReviewTime' = if state == New
        then Nothing
        else Just lastReviewTime
    pure $ Card
      { cardId          = 1
      , cardState       = state
      , cardDue         = lastReviewTime -- will get overwirtten
      , cardLastReview  = lastReviewTime'
      , cardStep        = step
      , cardLapses      = 0
      , cardRepetitions = 2
      , cardStability   = stability
      , cardDifficulty  = difficulty
      }

lastReviewTime :: UTCTime
lastReviewTime = UTCTime (fromGregorian 1970 0 0) 0
dueUpperBound :: UTCTime
dueUpperBound = UTCTime (fromGregorian 1972 0 0) 0
nextDayTime :: UTCTime
nextDayTime = UTCTime (fromGregorian 1970 0 1) 0
reviewUpperBound :: UTCTime
reviewUpperBound = UTCTime (fromGregorian 1973 0 0) 0

data ReviewOutput = ReviewOutput
  { out_card :: Card
  , out_revLog :: ReviewLog
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

spec :: Spec
spec = do
  describe "QuickCheck evaluation of review against Python Implementation" $ modifyMaxSuccess (const 5) $ do
    it "reviewCardAtTime should match the Python non-fuzzed value" $ property $ monadicIO $ do
      dueTime <- run $ generate $ chooseUTCTime lastReviewTime dueUpperBound
      reviewSameDay <- run $ generate arbitrary
      card <- run $ generate arbitrary
      let card' = card { cardDue = dueTime }
          pythonState = if cardState card == New then Learning else cardState card
          pythonCard = card' { cardState = pythonState }
      reviewTime <- if reviewSameDay
        then run $ generate $ chooseUTCTime lastReviewTime nextDayTime
        else run $ generate $ chooseUTCTime nextDayTime reviewUpperBound
      rating <- run $ generate arbitrary
      let (reviewedCard, revLog) = reviewCardAtTime defaultScheduler 5 card' rating reviewTime
      -- get Python value
          encCard = LBS.unpack $ encode pythonCard
          encReviewTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6Q+00:00" reviewTime
          encRating = LBS.unpack $ encode rating
      run $ putStrLn encReviewTime
      let process = proc "../venv/bin/python" ["extra/python-evaluation.py", encCard, encReviewTime, encRating]
      (exitCode, stdout, stderr) <- run $ readCreateProcessWithExitCode process ""
      run $ putStrLn stdout
      run $ putStrLn stderr
      run $ putStrLn $ show exitCode
      (pythonCard, pythonRevLog) <- case eitherDecode $ BL.pack stdout of
        Right revOut -> pure $ (out_card revOut, out_revLog revOut)
        Left err -> fail err
      let revLogPasses = revLogCardId revLog == 1
            && revLogReviewDatetime revLog == reviewTime
            && revLogRating revLog == rating
      -- check that card properties match
      void $ if
           cardId reviewedCard == cardId pythonCard
        && cardState reviewedCard == cardState pythonCard
        && cardDue reviewedCard == cardDue pythonCard
        && cardLastReview reviewedCard == cardLastReview pythonCard
        && cardStep reviewedCard == cardStep pythonCard
        && cardStability reviewedCard == cardStability pythonCard
        && cardDifficulty reviewedCard == cardDifficulty pythonCard
        then pure ()
        else do
          run $ putStrLn "Haskell review:"
          run $ print reviewedCard
          run $ putStrLn "Python review:"
          run $ print pythonCard
          fail "Card properties from python did not match Haskell implementation"
      pure revLogPasses

chooseUTCTime :: UTCTime -> UTCTime -> Gen UTCTime
chooseUTCTime lb ub = do
  let lbSec = floor $ utcTimeToPOSIXSeconds lb
      obSec = floor $ utcTimeToPOSIXSeconds ub
  timeSecond <- chooseInt (lbSec, obSec)
  pure $ posixSecondsToUTCTime $ fromIntegral timeSecond

