{-# LANGUAGE DeriveGeneric #-}

module FSRS.Schedule
  ( Scheduler (..),
    defaultScheduler,
    reviewCard,
    reviewCardAtTime,
    reviewCardFuzz,
    reviewCardFuzzAtTime,
    cardRetrievability,
    fuzzRanges,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), genericParseJSON, genericToJSON)
import Data.List (foldl')
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDay)
import FSRS.Card (Card (..), CardDetails (..), CardState (..), Difficulty, Stability, getCardId, getLapses, getRepetitions)
import FSRS.Parameters (Parameters (..), defaultParameters, difficultyMax, difficultyMin, stabilityMin)
import FSRS.Rating (Rating (..), fromRating)
import FSRS.ReviewLog (ReviewLog (..))
import FSRS.Utils (bound, genericParseOptionsWithPrefix, nominalMinute)
import GHC.Generics (Generic)
import System.Random (randomRIO)

data Scheduler = Scheduler
  { scParameters :: Parameters,
    scDesiredRetention :: Double,
    scLearningSteps :: [NominalDiffTime],
    scRelearningSteps :: [NominalDiffTime],
    scMaximumInterval :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON Scheduler where
  parseJSON = genericParseJSON $ genericParseOptionsWithPrefix "sc"

instance ToJSON Scheduler where
  toJSON = genericToJSON $ genericParseOptionsWithPrefix "sc"

defaultScheduler :: Scheduler
defaultScheduler =
  Scheduler
    { scParameters = defaultParameters,
      scDesiredRetention = 0.9,
      scLearningSteps = [1 * nominalMinute, 10 * nominalMinute],
      scRelearningSteps = [10 * nominalMinute],
      scMaximumInterval = 36500
    }

-- | This is an intermediate card update value which is internal to the scheduling module only.
-- | This is used to keep the actual update functions purely focused on changes to the card. The core FSRS
-- | algorithm really cares about the time since the last review, and this allows the separation between
-- | relative time and the actual date scheduling and revlog generation.
data SchedulingUpdate = SchedulingUpdate
  { suStability :: Stability,
    suDifficulty :: Difficulty,
    suInterval :: NominalDiffTime,
    suState :: CardState,
    suStep :: Int
  }

-- | reviews a card using the current time as the review date
reviewCard :: Scheduler -> NominalDiffTime -> Card -> Rating -> IO (Card, ReviewLog)
reviewCard s d c r = reviewCardAtTime s d c r <$> getCurrentTime

-- | reviews a card with slight random alterations to the due date if in review state
reviewCardFuzz :: Scheduler -> NominalDiffTime -> Card -> Rating -> IO (Card, ReviewLog)
reviewCardFuzz s d c r = getCurrentTime >>= reviewCardFuzzAtTime s d c r

-- | reviews a card at specified time, the only non-IO scheduling function.
reviewCardAtTime :: Scheduler -> NominalDiffTime -> Card -> Rating -> UTCTime -> (Card, ReviewLog)
reviewCardAtTime conf reviewDuration card rating reviewDate =
  let (update, didLapse) = case card of
        NewCard _ -> (updateNewCard conf rating, False)
        ActiveCard details ->
          let elapsedDays = daysSinceLastReview details reviewDate
           in case cardState details of
                Learning -> (updateLearningCard conf details rating elapsedDays, False)
                Reviewing -> (updateReviewingCard conf details rating elapsedDays, rating == Again)
                Relearning -> (updateRelearningCard conf details rating elapsedDays, False)
      cid = getCardId card
      updatedCard =
        ActiveCard $
          CardDetails
            { cardId = cid,
              cardState = suState update,
              cardDue = suInterval update `addUTCTime` reviewDate,
              cardLastReview = reviewDate,
              cardStep = suStep update,
              cardLapses = getLapses card + if didLapse then 1 else 0,
              cardRepetitions = getRepetitions card + 1,
              cardStability = suStability update,
              cardDifficulty = suDifficulty update
            }
      reviewLog = ReviewLog cid rating reviewDate reviewDuration
   in (updatedCard, reviewLog)

-- reviews a card at the specified time with slight random alterations to the due date if in review state
reviewCardFuzzAtTime ::
  Scheduler -> NominalDiffTime -> Card -> Rating -> UTCTime -> IO (Card, ReviewLog)
reviewCardFuzzAtTime conf reviewDuration card rating reviewDate = do
  let (update, didLapse) = case card of
        NewCard _ -> (updateNewCard conf rating, False)
        ActiveCard details ->
          let elapsedDays = daysSinceLastReview details reviewDate
           in case cardState details of
                Learning -> (updateLearningCard conf details rating elapsedDays, False)
                Reviewing -> (updateReviewingCard conf details rating elapsedDays, rating == Again)
                Relearning -> (updateRelearningCard conf details rating elapsedDays, False)
      cid = getCardId card
  fuzzedInterval <-
    if suState update == Reviewing
      then getFuzzedInterval (scMaximumInterval conf) (suInterval update)
      else pure $ suInterval update
  let updatedCard =
        ActiveCard $
          CardDetails
            { cardId = cid,
              cardState = suState update,
              cardDue = fuzzedInterval `addUTCTime` reviewDate,
              cardLastReview = reviewDate,
              cardStep = suStep update,
              cardLapses = getLapses card + if didLapse then 1 else 0,
              cardRepetitions = getRepetitions card + 1,
              cardStability = suStability update,
              cardDifficulty = suDifficulty update
            }
      reviewLog = ReviewLog cid rating reviewDate reviewDuration
  return (updatedCard, reviewLog)

updateNewCard :: Scheduler -> Rating -> SchedulingUpdate
updateNewCard conf rating =
  let params = scParameters conf
      updatedStability = initialStability params rating
      updatedDifficulty = initialDifficulty params rating
      learningSteps = scLearningSteps conf
      partialUpdate = SchedulingUpdate updatedStability updatedDifficulty -- currying to curb repetition
   in if null learningSteps
        then partialUpdate (nextInterval conf updatedStability) Reviewing 0
        else case rating of
          Again -> partialUpdate (head learningSteps) Learning 0
          Hard ->
            let updatedInterval
                  | [] <- learningSteps = 0 -- can't occur
                  | [s1] <- learningSteps = s1 * 1.5
                  | (s1 : s2 : _) <- learningSteps = (s1 + s2) / 2
             in partialUpdate updatedInterval Learning 0
          Good ->
            if length learningSteps == 1
              then partialUpdate (nextInterval conf updatedStability) Reviewing 0
              else partialUpdate (learningSteps !! 1) Learning 1
          Easy -> partialUpdate (nextInterval conf updatedStability) Reviewing 0

updateLearningCard :: Scheduler -> CardDetails -> Rating -> NominalDiffTime -> SchedulingUpdate
updateLearningCard conf card rating elapsedDays =
  let updatedStability = shortOrLongTermStability conf card rating elapsedDays
      updatedDifficulty = nextDifficulty conf (cardDifficulty card) rating
      learningSteps = scLearningSteps conf
      step = cardStep card
      partialUpdate = SchedulingUpdate updatedStability updatedDifficulty -- currying to curb repetition
   in if null learningSteps || (step >= length learningSteps && rating /= Again)
        then partialUpdate (nextInterval conf updatedStability) Reviewing 0
        else case rating of
          Again -> partialUpdate (head learningSteps) Learning 0
          Hard ->
            let updatedInterval = hardSteppedInterval step learningSteps
             in partialUpdate updatedInterval Learning step
          Good ->
            if step + 1 == length learningSteps
              then partialUpdate (nextInterval conf updatedStability) Reviewing 0
              else partialUpdate (learningSteps !! (step + 1)) Learning (step + 1)
          Easy -> partialUpdate (nextInterval conf updatedStability) Reviewing 0

updateReviewingCard :: Scheduler -> CardDetails -> Rating -> NominalDiffTime -> SchedulingUpdate
updateReviewingCard conf card rating elapsedDays =
  let difficulty = cardDifficulty card
      updatedStability = shortOrLongTermStability conf card rating elapsedDays
      updatedDifficulty = nextDifficulty conf difficulty rating
      updataedState =
        if rating == Again && not (null (scRelearningSteps conf))
          then Relearning
          else Reviewing
      updatedInterval =
        if updataedState == Relearning
          then head (scRelearningSteps conf)
          else nextInterval conf updatedStability
   in SchedulingUpdate updatedStability updatedDifficulty updatedInterval updataedState 0

updateRelearningCard :: Scheduler -> CardDetails -> Rating -> NominalDiffTime -> SchedulingUpdate
updateRelearningCard conf card rating elapsedDays =
  let difficulty = cardDifficulty card
      updatedStability = shortOrLongTermStability conf card rating elapsedDays
      updatedDifficulty = nextDifficulty conf difficulty rating
      relearningSteps = scRelearningSteps conf
      step = cardStep card
      partialUpdate = SchedulingUpdate updatedStability updatedDifficulty -- currying to curb repetition
   in if null relearningSteps || (step >= length relearningSteps && rating /= Again)
        then partialUpdate (nextInterval conf updatedStability) Reviewing 0
        else case rating of
          Again -> partialUpdate (head relearningSteps) Relearning 0
          Hard ->
            let updatedInterval = hardSteppedInterval step relearningSteps
             in partialUpdate updatedInterval Relearning step
          Good ->
            if step + 1 == length relearningSteps
              then partialUpdate (nextInterval conf updatedStability) Reviewing 0
              else partialUpdate (relearningSteps !! (step + 1)) Reviewing (step + 1)
          Easy -> partialUpdate (nextInterval conf updatedStability) Reviewing 0

shortOrLongTermStability :: Scheduler -> CardDetails -> Rating -> NominalDiffTime -> Stability
shortOrLongTermStability conf card rating elapsedDays
  | elapsedDays < 1 = shortTerm
  | otherwise = longTerm
  where
    shortTerm = shortTermStability conf (cardStability card) rating
    longTerm =
      let retrievability =
            cardRetrievability
              (wRetrievabilityDecay $ scParameters conf)
              (cardStability card)
              elapsedDays
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
      stStabExp = wShortTermStabilityExpCoef params
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
      shortTermInc' =
        if rating == Good || rating == Easy
          then max 1 shortTermInc
          else shortTermInc
   in clampStability $ stability * shortTermInc'

-- Fractional of the number of days
daysSinceLastReview :: CardDetails -> UTCTime -> NominalDiffTime
daysSinceLastReview card reviewDate = max 0 $ (reviewDate `diffUTCTime` cardLastReview card) / nominalDay

initialStability :: Parameters -> Rating -> Stability
initialStability params rating = clampStability $ case rating of
  Again -> wInitialStabilityAgain params
  Hard -> wInitialStabilityHard params
  Good -> wInitialStabilityGood params
  Easy -> wInitialStabilityEasy params

initialDifficulty :: Parameters -> Rating -> Difficulty
initialDifficulty params rating =
  clampDifficulty $
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

cardRetrievability :: Double -> Stability -> NominalDiffTime -> Double
cardRetrievability decayParam stability elapsedDays =
  let decay = (-decayParam)
      totalDays = flooredDays elapsedDays
   in (1 + decayFactor decay * totalDays / stability) ** decay

getFuzzedInterval :: Int -> NominalDiffTime -> IO NominalDiffTime
getFuzzedInterval maxInterval interval =
  if interval / nominalDay < 2.5
    then pure interval
    else do
      let intervalDays = realToFrac (floor $ interval / nominalDay :: Integer)
          delta = getDeltaForInterval intervalDays
          maxIntr = min (round $ intervalDays + delta) maxInterval
          minIntr = min maxIntr (max 2 (round $ intervalDays - delta))
      fuzzedIntr <- randomRIO (minIntr, maxIntr)
      return $ nominalDay * realToFrac (min fuzzedIntr maxInterval)

fuzzRanges :: [(NominalDiffTime, Maybe NominalDiffTime, NominalDiffTime)]
fuzzRanges =
  [ (2.5, Just 7.0, 0.15),
    (7.0, Just 20.0, 0.1),
    (20.0, Nothing, 0.05)
  ]

getDeltaForInterval :: NominalDiffTime -> NominalDiffTime
getDeltaForInterval interval =
  foldl' (fuzzRangeStep interval) 1 fuzzRanges
  where
    factorCap intr maybeTime = case maybeTime of
      Nothing -> intr
      Just upperBound -> min upperBound intr
    fuzzRangeStep intr delta (start, end, factor) =
      delta + factor * max (factorCap intr end - start) 0

flooredDays :: (Num a, Ord a) => NominalDiffTime -> a
flooredDays delta = max 0 $ fromIntegral (floor delta :: Int)

-- a quirk of the fsrs algorithm for stepped processes on hard selectin
hardSteppedInterval :: Int -> [NominalDiffTime] -> NominalDiffTime
hardSteppedInterval _ [] = 0 -- should never occur, checks occur before, I may flatten the structure
hardSteppedInterval 0 [s1] = 1.5 * s1
hardSteppedInterval 0 (s1 : s2 : _) = (s1 + s2) / 2
hardSteppedInterval idx steps = steps !! idx
