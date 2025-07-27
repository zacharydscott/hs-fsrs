{-# LANGUAGE DeriveGeneric #-}
module FSRS.Schedule (
  Scheduler(..),
  defaultScheduler,
  reviewCard,
  reviewCardAtTime,
  reviewCardFuzz,
  reviewCardFuzzAtTime,
) where

import FSRS.Card (Card(..), CardState(..), Difficulty, Stability)
import FSRS.Parameters (Parameters(..), stabilityMin, difficultyMin, difficultyMax, defaultParameters)
import FSRS.Rating (Rating(..), fromRating)
import Data.Time (NominalDiffTime, diffUTCTime, nominalDay, addUTCTime, UTCTime, getCurrentTime)
import FSRS.Utils (bound, nominalMinute, genericParseOptionsWithPrefix)
import FSRS.ReviewLog (ReviewLog (..))
import Data.List (foldl')
import System.Random (randomRIO)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), genericParseJSON, genericToJSON)

data Scheduler = Scheduler
  { scParameters       :: Parameters
  , scDesiredRetention :: Double
  , scLearningSteps    :: [NominalDiffTime]
  , scRelearningSteps  :: [NominalDiffTime]
  , scMaximumInterval  :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON Scheduler where
  parseJSON = genericParseJSON $ genericParseOptionsWithPrefix "sc"

instance ToJSON Scheduler where
  toJSON = genericToJSON $ genericParseOptionsWithPrefix "sc"

defaultScheduler :: Scheduler
defaultScheduler = Scheduler
  { scParameters = defaultParameters
  , scDesiredRetention = 0.9
  , scLearningSteps = [1 * nominalMinute, 10 * nominalMinute]
  , scRelearningSteps = [10 * nominalMinute]
  , scMaximumInterval = 36500
  }

-- | This is an intermediate card update value which is for internal to scheduling module only.
-- | This is used to keep the actual update functions purely focused on changes to the card, so:
-- | 1) allowing the time values to be relative (actual dates handled by caller)
-- | 2) the updated interval is handed back, so fuzzing can occur. Fuzzing requires IO since it needs randomness
-- | 3) the review log doesn't need to be in the core calculation
-- | manipulated for fuzzing.
data SchedulingUpdate = SchedulingUpdate
  { suStability  :: Stability
  , suDifficulty :: Difficulty
  , suInterval   :: NominalDiffTime
  , suState      :: CardState
  , suStep       :: Int
  }

-- | reviews a card using the current time as the review date
reviewCard :: Scheduler -> NominalDiffTime -> Card -> Rating -> IO (Card, ReviewLog)
reviewCard s d c r = reviewCardAtTime s d c r <$> getCurrentTime

reviewCardFuzz :: Scheduler -> NominalDiffTime -> Card -> Rating -> IO (Card, ReviewLog)
reviewCardFuzz s d c r = getCurrentTime >>= reviewCardFuzzAtTime s d c r 

reviewCardAtTime :: Scheduler -> NominalDiffTime -> Card -> Rating -> UTCTime -> (Card, ReviewLog)
reviewCardAtTime conf reviewDuration card rating reviewDate =
  let mElapsedDays = daysSinceLastReview card reviewDate
      update = case cardState card of
        New -> updateNewCard conf card rating
        Learning -> updateLearningCard conf card rating mElapsedDays
        Reviewing -> updateReviewingCard conf card rating mElapsedDays
        Relearning -> updateRelearningCard conf card rating mElapsedDays
      didLapse = rating == Again && cardState card == Reviewing
      updatedCard = Card
        { cardId = cardId card
        , cardState = suState update
        , cardDue =  suInterval update `addUTCTime` reviewDate
        , cardLastReview = Just reviewDate
        , cardStep = suStep update
        , cardLapses = cardLapses card + if didLapse then 1 else 0
        , cardRepetitions = cardRepetitions card + 1
        , cardStability = suStability update
        , cardDifficulty = suDifficulty update
        }
      reviewLog = ReviewLog (cardId card) rating reviewDate reviewDuration
  in (updatedCard, reviewLog)

reviewCardFuzzAtTime ::
  Scheduler -> NominalDiffTime -> Card -> Rating -> UTCTime -> IO (Card, ReviewLog)
reviewCardFuzzAtTime conf reviewDuration card rating reviewDate = do
  let mElapsedDays = daysSinceLastReview card reviewDate
      update = case cardState card of
        New -> updateNewCard conf card rating
        Learning -> updateLearningCard conf card rating mElapsedDays
        Reviewing -> updateReviewingCard conf card rating mElapsedDays
        Relearning -> updateRelearningCard conf card rating mElapsedDays
      didLapse = rating == Again && cardState card == Reviewing
  fuzzedInterval <- getFuzzedInterval conf (suInterval update)
  let updatedCard = Card
        { cardId = cardId card
        , cardState = suState update
        , cardDue =  fuzzedInterval `addUTCTime` reviewDate
        , cardLastReview = Just reviewDate
        , cardStep = suStep update
        , cardLapses = cardLapses card + if didLapse then 1 else 0
        , cardRepetitions = cardRepetitions card + 1
        , cardStability = suStability update
        , cardDifficulty = suDifficulty update
        }
      reviewLog = ReviewLog (cardId card) rating reviewDate reviewDuration
  return (updatedCard, reviewLog)

updateNewCard :: Scheduler -> Card -> Rating -> SchedulingUpdate
updateNewCard conf card rating =
  let params = scParameters conf
      updatedStability = initialStability params rating
      updatedDifficulty = initialDifficulty params rating
      learningSteps = scLearningSteps conf
      step = cardStep card
      partialUpdate = SchedulingUpdate updatedStability updatedDifficulty -- currying to curb repetition
  in if null learningSteps || (step >= length learningSteps && rating /= Again)
    then partialUpdate (nextInterval conf updatedStability) Reviewing 0
    else case rating of
      Again -> partialUpdate (head learningSteps) Learning 0
      Hard -> let updatedInterval
                   | step == 0 && length learningSteps == 1
                   = head learningSteps * 1.5
                   | step == 0 -- omitting check for 2 or greater as it is implied
                   = let (s1:s2:_) = learningSteps in (s1 + s2) / 2
                   | otherwise
                   = learningSteps !! step
              in partialUpdate updatedInterval Learning step
      Good -> if step + 1 == length learningSteps
                then partialUpdate (nextInterval conf updatedStability) Reviewing 0
                else partialUpdate (learningSteps !! (step + 1)) Learning (step + 1)
      Easy -> partialUpdate (nextInterval conf updatedStability) Reviewing 0

updateLearningCard :: Scheduler -> Card -> Rating -> Maybe NominalDiffTime -> SchedulingUpdate
updateLearningCard conf card rating mElapsedDays =
  let updatedStability = shortOrLongTermStability conf card rating mElapsedDays
      updatedDifficulty = nextDifficulty conf (cardDifficulty card) rating
      learningSteps = scLearningSteps conf
      step = cardStep card
      partialUpdate = SchedulingUpdate updatedStability updatedDifficulty -- currying to curb repetition
  in if null learningSteps || (step >= length learningSteps && rating /= Again)
    then partialUpdate (nextInterval conf updatedStability) Reviewing 0
    else case rating of
      Again -> partialUpdate (head learningSteps) Learning 0
      Hard -> let updatedInterval
                   | step == 0 && length learningSteps == 1
                   = head learningSteps * 1.5
                   | step == 0 -- omitting check for 2 or greater as it is implied
                   = let (s1:s2:_) = learningSteps in (s1 + s2) / 2
                   | otherwise
                   = learningSteps !! step
              in partialUpdate updatedInterval Learning step
      Good -> if step + 1 == length learningSteps
                then partialUpdate (nextInterval conf updatedStability) Reviewing 0
                else partialUpdate (learningSteps !! (step + 1)) Learning (step + 1)
      Easy -> partialUpdate (nextInterval conf updatedStability) Reviewing 0

updateReviewingCard :: Scheduler -> Card -> Rating -> Maybe NominalDiffTime -> SchedulingUpdate
updateReviewingCard conf card rating mElapsedDays =
  let difficulty = cardDifficulty card
      updatedStability = shortOrLongTermStability conf card rating mElapsedDays
      updatedDifficulty = nextDifficulty conf difficulty rating
      updataedState = if rating == Again && not (null (scRelearningSteps conf))
        then Relearning
        else Reviewing
      updatedInterval = if updataedState == Relearning
        then head (scRelearningSteps conf)
        else nextInterval conf updatedStability
  in SchedulingUpdate updatedStability updatedDifficulty updatedInterval updataedState 0


updateRelearningCard :: Scheduler -> Card -> Rating -> Maybe NominalDiffTime -> SchedulingUpdate
updateRelearningCard conf card rating mElapsedDays =
  let difficulty = cardDifficulty card
      updatedStability = shortOrLongTermStability conf card rating mElapsedDays
      updatedDifficulty = nextDifficulty conf difficulty rating
      relearningSteps = scRelearningSteps conf
      step = cardStep card
      partialUpdate = SchedulingUpdate updatedStability updatedDifficulty -- currying to curb repetition
  in if null relearningSteps || (step >= length relearningSteps && rating /= Again)
    then partialUpdate (nextInterval conf updatedStability) Reviewing 0
    else case rating of
      Again -> partialUpdate (head relearningSteps) Relearning 0
      Hard -> let updatedInterval
                   | step == 0 && length relearningSteps == 1
                   = head relearningSteps * 1.5
                   | step == 0 -- omitting check for 2 or greater as it is implied
                   = let (s1:s2:_) = relearningSteps in (s1 + s2) / 2
                   | otherwise
                   = relearningSteps !! step
              in partialUpdate updatedInterval Relearning step
      Good -> if step + 1 == length relearningSteps
                then partialUpdate (nextInterval conf updatedStability) Reviewing 0
                else partialUpdate (relearningSteps !! (step + 1)) Reviewing (step + 1)
      Easy -> partialUpdate (nextInterval conf updatedStability) Reviewing 0

shortOrLongTermStability :: Scheduler -> Card -> Rating -> Maybe NominalDiffTime -> Stability
shortOrLongTermStability conf card rating mElapsedDays
  | Just d <- mElapsedDays, d < 1 = shortTerm
  | otherwise                     = longTerm
  where shortTerm = shortTermStability conf (cardStability card) rating
        longTerm = let retrievability = cardRetrievability conf card mElapsedDays
                   in nextStability conf (cardDifficulty card) retrievability rating (cardStability card)

nextStability :: Scheduler -> Difficulty -> Double -> Rating -> Stability -> Double
nextStability conf difficulty retrievability rating stability = clampStability $ case rating of
  Again -> nextForgetStability conf difficulty retrievability stability
  _ -> nextRecallStability conf difficulty retrievability rating stability

nextForgetStability :: Scheduler -> Difficulty -> Double -> Stability -> Double
nextForgetStability conf difficulty retrievability stability =
  let params = scParameters conf
      longTermBase = wForgetStabilityLongTermBase params
      diffExp = wForgetStabilityDifficultyExp params
      stabExp = wForgetStabilityStabilityExp params
      retCoef = wForgetStabilityRetrievabilityCoef params
      longTermVersion =
          longTermBase
        * (difficulty ** (-diffExp))
        * ((stability + 1) ** stabExp - 1)
        * exp (retCoef * (1 - retrievability))
      stStabExp = wShortTermStabilityStabilityExp params
      stRatingNorm = wShortTermStabilityRatingNormalization params
      shortTermVersion = stability / exp (stStabExp * stRatingNorm)
  in min longTermVersion shortTermVersion

nextRecallStability :: Scheduler -> Difficulty -> Double -> Rating -> Stability -> Double
nextRecallStability conf difficulty retrievability rating stability =
  let params = scParameters conf
      hardPenalty = if rating == Hard then wRecallStabilityHardPenalty params else 1
      easyBonus = if rating == Easy then wRecallStabilityEasyBonus params else 1
      growthRate = wRecallStabilityGrowthRate params
      damper = wRecallStabilityDamper params
      retCoef = wRecallStabilityRetievabilityCoef params
      stabilityMultiplier =
          exp growthRate
        * (11 - difficulty)
        * (stability ** (-damper))
        * (exp (retCoef * (1 - retrievability)) - 1)
        * hardPenalty
        * easyBonus
  in stability * (1 + stabilityMultiplier)

shortTermStability :: Scheduler -> Stability -> Rating -> Double
shortTermStability conf stability rating =
  let params = scParameters conf
      boost = wShortTermStabilityExpCoef params
      rn = wShortTermStabilityRatingNormalization params
      se = wShortTermStabilityStabilityExp params
      shortTermInc = exp (boost * (fromRating rating - 3 + rn)) * (stability ** (-se))
      shortTermInc' = if rating == Good || rating == Easy
        then max 1 shortTermInc
        else shortTermInc
  in clampStability $ stability * shortTermInc'

-- Fractional of the number of days
daysSinceLastReview :: Card  -> UTCTime -> Maybe NominalDiffTime
daysSinceLastReview card reviewDate = fmap (\lastReview -> max 0 $ reviewDate `diffUTCTime` lastReview / nominalDay) (cardLastReview card)

initialStability :: Parameters -> Rating -> Stability
initialStability params rating = clampStability $ case rating of
  Again -> wInitialStabilityAgain params
  Hard  -> wInitialStabilityHard  params
  Good  -> wInitialStabilityGood  params
  Easy  -> wInitialStabilityEasy  params

initialDifficulty :: Parameters -> Rating -> Difficulty
initialDifficulty params rating = clampDifficulty $
  let difficultyBase = wInitialDifficultyBaseline params
      ratingCoef = wInitialDifficultyRatingCoef params
  in difficultyBase + 1 - exp (ratingCoef * (fromRating rating - 1))

clampStability :: Stability -> Stability
clampStability stab = max stab stabilityMin

clampDifficulty :: Difficulty -> Difficulty
clampDifficulty = bound difficultyMin difficultyMax

decayFactor :: Double -> Double
decayFactor decay = 0.9 ** (1 / decay) - 1

nextInterval :: Scheduler -> Stability -> NominalDiffTime
nextInterval conf stability =
  let decay = -(wRetrievabilityDecay $ scParameters conf)
      ret = scDesiredRetention conf
      maxIntr = scMaximumInterval conf
      rawNextInterval = (stability / decayFactor decay) * (ret ** (1 / decay) - 1)
  in nominalDay * realToFrac (bound 1 maxIntr (round rawNextInterval))

nextDifficulty :: Scheduler -> Difficulty -> Rating -> Double
nextDifficulty conf difficulty rating =
  let params = scParameters conf
      dc = wDifficultyDeltaCoef params
      mr = wDifficultyMeanReversion params
      easyDifficulty = initialDifficulty params Easy
      difficultyDelta = -(dc * (fromRating rating - 3))
      dampedDifficulty = difficulty + (10 - difficulty) * difficultyDelta / 9
      meanReversionDifficulty = mr * easyDifficulty + (1 - mr) * dampedDifficulty
  in clampDifficulty meanReversionDifficulty

cardRetrievability :: Scheduler -> Card -> Maybe NominalDiffTime -> Double
cardRetrievability conf card mElapsedDays = case mElapsedDays of
  Nothing -> 0
  Just elapsedDays -> let decay = -(wRetrievabilityDecay $ scParameters conf)
                          roundedElapsed = fromIntegral (floor elapsedDays :: Int)
                      in (1 + decayFactor decay * roundedElapsed / cardStability card) ** decay

getFuzzedInterval :: Scheduler -> NominalDiffTime -> IO NominalDiffTime
getFuzzedInterval conf interval = if interval / nominalDay < 2.5
  then pure interval
  else do
    let intervalDays = realToFrac (floor $ interval / nominalDay :: Integer)
        delta = getDeltaForInterval intervalDays
        maxIntr = min (round $ intervalDays + delta) (scMaximumInterval conf)
        minIntr = min maxIntr (max 2 (round $ intervalDays - delta))
    fuzzedIntr <- randomRIO (minIntr, maxIntr)
    return $ nominalDay * realToFrac (min fuzzedIntr (scMaximumInterval conf))

fuzzRanges :: [(NominalDiffTime, Maybe NominalDiffTime, NominalDiffTime)]
fuzzRanges =
  [ (2.5, Just 7.0, 0.15)
  , (7.0, Just 20.0, 0.1)
  , (20.0, Nothing, 0.05)
  ]

getDeltaForInterval :: NominalDiffTime -> NominalDiffTime
getDeltaForInterval interval =
  foldl' (fuzzRangeStep interval) 1 fuzzRanges
  where factorCap intr maybeTime = case maybeTime of
          Nothing -> intr
          Just upperBound -> min upperBound intr
        fuzzRangeStep intr delta (start, end, factor) =
          delta + factor * max (factorCap intr end - start) 0

