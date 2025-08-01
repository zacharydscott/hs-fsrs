{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module FSRS.Schedule_QuickCheck_Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), NominalDiffTime)
import FSRS.Schedule
import FSRS.Rating ( Rating(..), toRating )
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))
import FSRS.Card
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import FSRS.ReviewLog
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck.Monadic (monadicIO, run)
import System.Process (proc, CreateProcess (..), createProcess, StdStream (CreatePipe))
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Aeson (encode, eitherDecode, ToJSON, FromJSON)
import Data.Time (diffUTCTime, nominalDay, addUTCTime)
import FSRS.Parameters
    ( stabilityMin, difficultyMin, difficultyMax, Parameters(..) )
import Control.Monad (void)
import GHC.Generics (Generic)
import GHC.IO.Handle (Handle, hFlush)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (foldl')

instance Arbitrary Rating where
  arbitrary = (choose (1, 4) :: Gen Int) <&> fromMaybe Again . toRating

instance Arbitrary CardState where
  arbitrary = (choose (0, 3) :: Gen Int) <&> fromMaybe Learning . toCardState

-- no need to randomly geneary the id, and some other properties and lastReviewTime is hard coded
-- This is only needed for this test suite
instance Arbitrary Card where
  arbitrary = do
    makeNew <- arbitrary
    if makeNew
      then pure $ NewCard 1
      else do
        state <- arbitrary
        step <- if state == Learning || state == Relearning
          then chooseInt (0,3)
          else pure 0
        stability <- max stabilityMin . abs <$> arbitrary
        difficulty <- choose (difficultyMin, difficultyMax)
        pure $ ActiveCard $ CardDetails
          { cardId          = 1
          , cardState       = state
          , cardDue         = lastReviewTime -- unimportant, not used in calculations, just for from_dict
          , cardLastReview  = lastReviewTime
          , cardStep        = step
          , cardLapses      = 0
          , cardRepetitions = 0
          , cardStability   = stability
          , cardDifficulty  = difficulty
          }

lastReviewTime :: UTCTime
lastReviewTime = UTCTime (fromGregorian 1970 0 0) 5
nextDayTime :: UTCTime
nextDayTime = UTCTime (fromGregorian 1970 0 1) 5
reviewUpperBound :: UTCTime
reviewUpperBound = UTCTime (fromGregorian 1975 0 0) 5

data ReviewOutput = ReviewOutput
  { out_card :: Card
  , out_revLog :: ReviewLog
  , out_retrievability :: Double
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ReviewInput = ReviewInput
  { in_card :: Card
  , in_rating :: Rating
  , in_review_time :: UTCTime
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

approx :: (Fractional a, Ord a) => a -> a -> Bool
approx a b =
  let (h,l) = (max a b, min a b)
  in (h - l) / l < 0.001

infix 4 `approx`

data PyProc = PyProc
  { pyIn  :: Handle
  , pyOut :: Handle
  }

withPythonFSRS ::  IO PyProc
withPythonFSRS = do
          (Just hin, Just hout, _, _) <- createProcess
            (proc "../venv/bin/python3" ["extra/python-evaluation.py"])
            { std_in = CreatePipe, std_out = CreatePipe}
          pure $ PyProc hin hout

spec :: Spec
spec = beforeAll withPythonFSRS $ describe "QuickCheck evaluation of review against Python Implementation" $ modifyMaxSuccess (const 10000) $ do
  it "fuzzed values are within range and only for reviewing cards" $ \_ -> do
    property $ monadicIO $ do
      reviewSameDay <- run $ generate arbitrary
      card <- run $ generate arbitrary
      reviewTime <- if reviewSameDay
        then run $ generate $ chooseUTCTime lastReviewTime nextDayTime
        else run $ generate $ chooseUTCTime nextDayTime reviewUpperBound
      rating <- run $ generate arbitrary
      let (normalCard, normalRevLog) = reviewCardAtTime defaultScheduler 5 card rating reviewTime
      (fuzzedCard, fuzzedRevLog) <- run $ reviewCardFuzzAtTime defaultScheduler 5 card rating reviewTime
      void $ if normalRevLog == fuzzedRevLog then pure () else fail "Revlogs don't match"
      case fuzzedCard of
        NewCard _ -> pure $ fuzzedCard == normalCard
        ActiveCard fd -> do
          nd <- case normalCard of
            NewCard _ -> fail "New card from normal evaluation when fuzzed is active"
            ActiveCard det -> pure det
          void $ if cardId fd == cardId nd then pure () else fail "card ids don't match"
          void $ if cardState fd == cardState nd then pure () else fail "card states don't match"
          void $ if cardLastReview fd == cardLastReview nd then pure () else fail "last reviews don't match"
          void $ if cardLapses fd == cardLapses nd then pure () else fail "lapses don't match"
          void $ if cardRepetitions fd == cardRepetitions nd then pure () else fail "repetitions don't match"
          void $ if cardStability fd == cardStability nd then pure () else fail "stabilities Ids don't match"
          void $ if cardDifficulty fd == cardDifficulty nd then pure () else fail "difficulties Ids don't match"
          void $ if cardId fd == cardId nd then pure () else fail "card Ids don't match"
          void $ case cardState fd of
            Reviewing -> do
              let dueRange = getFuzzedRange (scMaximumInterval defaultScheduler) (cardDue nd `diffUTCTime` reviewTime)
                  (minTime, maxTime) = (
                      fst dueRange `addUTCTime` reviewTime
                    , snd dueRange `addUTCTime` reviewTime
                    )
                  fuzzDue = cardDue fd
              if fuzzDue >= minTime
                 && fuzzDue <= maxTime
                then pure ()
                else fail $ "fuzzed due date '" ++ show fuzzDue ++ "' was not in range ('"
                  ++ show minTime ++ "','" ++ show maxTime ++ "'), from non-fuzzed date: '"
                  ++ show (cardDue nd) ++ "'."
            _         -> if cardStability fd == cardStability nd then pure () else fail "card Ids don't match"
          pure True

  it "reviewCardAtTime matchs the Python Implementation non-fuzzed value" $ \py -> do
    property $ monadicIO $ do
      reviewSameDay <- run $ generate arbitrary
      card <- run $ generate arbitrary
      reviewTime <- if reviewSameDay
        then run $ generate $ chooseUTCTime lastReviewTime nextDayTime
        else run $ generate $ chooseUTCTime nextDayTime reviewUpperBound
      rating <- run $ generate arbitrary
      let (reviewedCard, revLog) = reviewCardAtTime defaultScheduler 5 card rating reviewTime
          pyFSRSInput = ReviewInput card rating reviewTime
      run $ do
        LBS8.hPutStrLn (pyIn py) (encode pyFSRSInput)
        hFlush (pyIn py)
      outLine <- run $ BS.hGetLine (pyOut py)
      (reviewedPythonCard, pythonRevLog, retrievability) <- case eitherDecode $ LBS.fromStrict outLine of
        Right revOut -> pure (out_card revOut, out_revLog revOut, out_retrievability revOut)
        Left err -> fail $ "unable to parse '" ++ show outLine ++ "': " ++ err
      void $ if
                 revLogCardId revLog == 1
              && revLogReviewDatetime revLog == reviewTime
              && revLogRating revLog == rating
            then pure ()
            else fail "rev log is invalid"
      void $ if revLog == pythonRevLog
        then pure ()
        else fail $ "Haskell revlog does not match the Python implementation. Haskell: "
          ++ show revLog ++ ". Python" ++ show pythonRevLog
      -- check that card properties match
      pythonDetails <- case reviewedPythonCard of
        NewCard _ -> fail "python reviewed card returned as new"
        (ActiveCard d) -> pure d
      haskellDetails <- case reviewedCard of
        NewCard _ -> fail "Haskell reviewed card returned as new"
        (ActiveCard d) -> pure d
      haskellPreDetails <- case card of
        NewCard _ -> pure haskellDetails
        (ActiveCard d) -> pure d
      let hsRetrievability = case card of
            NewCard _ -> 0
            ActiveCard d -> cardRetrievability ( wRetrievabilityDecay $ scParameters defaultScheduler) (cardStability d) ((reviewTime `diffUTCTime` lastReviewTime) / nominalDay)
      void $ if
           cardId haskellDetails == cardId pythonDetails
        && cardState haskellDetails == cardState pythonDetails
        && (cardDue haskellDetails `diffUTCTime` reviewTime)
           `approx` (cardDue pythonDetails `diffUTCTime` reviewTime)
        && cardLastReview haskellDetails == cardLastReview pythonDetails
        && cardStep haskellDetails == cardStep pythonDetails
        && cardStability haskellDetails `approx` cardStability pythonDetails
        && cardDifficulty haskellDetails `approx` cardDifficulty pythonDetails
        then pure ()
        else do
          fail $ "Card properties from python did not match Haskell implementation"
            ++ "\nHaskell review:\n" ++ show reviewedCard ++ "\nPython review:\n" ++ show reviewedPythonCard
            ++ "\nOriginal Card:\n" ++ show card
            ++ "\nReview Log:\n" ++ show revLog
            ++ "\nPython Retrievability:\n" ++ show retrievability
            ++ "\nHaskell Retrievability:\n" ++ show hsRetrievability
            ++ "\nHaskell days since:\n" ++ show (daysSinceLastReview haskellPreDetails reviewTime)
            ++ "\nHaskell diff time:\n" ++ show (reviewTime `diffUTCTime` cardLastReview haskellPreDetails)
      pure True

chooseUTCTime :: UTCTime -> UTCTime -> Gen UTCTime
chooseUTCTime lb ub = do
  let lbSec = floor $ utcTimeToPOSIXSeconds lb
      obSec = floor $ utcTimeToPOSIXSeconds ub
  timeSecond <- chooseInt (lbSec, obSec)
  pure $ posixSecondsToUTCTime $ fromIntegral timeSecond

daysSinceLastReview :: CardDetails -> UTCTime -> NominalDiffTime
daysSinceLastReview card reviewDate = max 0 $ (reviewDate `diffUTCTime` cardLastReview card) / nominalDay

-- replicating scheduler code, perhaps should be exported
getFuzzedRange :: Int -> NominalDiffTime -> (NominalDiffTime, NominalDiffTime)
getFuzzedRange maximumInterval interval = if interval / nominalDay < 2.5
  then (interval, interval)
  else let intervalDays = realToFrac (floor $ interval / nominalDay :: Integer)
           delta = getDeltaForInterval intervalDays
           maxIntr = min (round $ intervalDays + delta) maximumInterval
           minIntr = min maxIntr (max 2 (round $ intervalDays - delta))
       in (nominalDay * realToFrac minIntr, nominalDay *  realToFrac maxIntr)

getDeltaForInterval :: NominalDiffTime -> NominalDiffTime
getDeltaForInterval interval =
  foldl' (fuzzRangeStep interval) 1 fuzzRanges
  where factorCap intr maybeTime = case maybeTime of
          Nothing -> intr
          Just upperBound -> min upperBound intr
        fuzzRangeStep intr delta (start, end, factor) =
          delta + factor * max (factorCap intr end - start) 0

