module FSRS.Schedule (
  SchedulingConfig(..),
  reviewCard,
  reviewCardWithFuzz,
  reviewCardWithCurrentTime,
  reviewCardWithFuzzAndCurrentTime
) where

import FSRS.Card (Card(..), CardState(..), Difficulty, Stability)
import FSRS.Parameters (Parameters(..), stabilityMin, difficultyMin, difficultyMax)
import FSRS.Rating (Rating(..), fromRating)
import Data.Time (NominalDiffTime, diffUTCTime, nominalDay, addUTCTime, UTCTime, getCurrentTime)
import FSRS.Utils (bound)
import FSRS.ReviewLog (ReviewLog (..))

data SchedulingConfig = SchedulingConfig
  { scParameters       :: Parameters
  , scDesiredRetention :: Double
  , scLearningSteps    :: [NominalDiffTime]
  , scRelearningSteps  :: [NominalDiffTime]
  , scMaximumInterval  :: Int
  } deriving (Show)

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

reviewCard :: SchedulingConfig -> Card -> Rating -> UTCTime -> NominalDiffTime -> (Card, ReviewLog)
reviewCard conf card rating reviewDate reviewDuration =
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

reviewCardWithFuzz ::
  SchedulingConfig -> Card -> Rating -> UTCTime -> NominalDiffTime -> IO (Card, ReviewLog)
reviewCardWithFuzz conf card rating reviewDate reviewDuration = do
  let mElapsedDays = daysSinceLastReview card reviewDate
      update = case cardState card of
        New -> updateNewCard conf card rating
        Learning -> updateLearningCard conf card rating mElapsedDays
        Reviewing -> updateReviewingCard conf card rating mElapsedDays
        Relearning -> updateRelearningCard conf card rating mElapsedDays
      didLapse = rating == Again && cardState card == Reviewing
  fuzzedInterval <- getFuzzedInterval $ suInterval update
  let updatedCard = Card
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
  return (updatedCard, reviewLog)

-- | reviews a card using the current time as the review date
reviewCardWithCurrentTime :: SchedulingConfig -> Card -> Rating -> NominalDiffTime -> IO (Card, ReviewLog)
reviewCardWithCurrentTime s c r d = do
  currentTime <- getCurrentTime
  return $ reviewCard s c r currentTime d

reviewCardWithFuzzAndCurrentTime :: SchedulingConfig -> Card -> Rating -> NominalDiffTime -> IO (Card, ReviewLog)
reviewCardWithFuzzAndCurrentTime s c r d = do
  currentTime <- getCurrentTime
  reviewCardWithFuzz s c r currentTime d



updateNewCard :: SchedulingConfig -> Card -> Rating -> SchedulingUpdate
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
                else partialUpdate (learningSteps !! step + 1) Learning (step + 1)
      Easy -> partialUpdate (nextInterval conf updatedStability) Reviewing 0

updateLearningCard :: SchedulingConfig -> Card -> Rating -> Maybe NominalDiffTime -> SchedulingUpdate
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
                else partialUpdate (learningSteps !! step + 1) Learning (step + 1)
      Easy -> partialUpdate (nextInterval conf updatedStability) Reviewing 0

updateReviewingCard :: SchedulingConfig -> Card -> Rating -> Maybe NominalDiffTime -> SchedulingUpdate
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


updateRelearningCard :: SchedulingConfig -> Card -> Rating -> Maybe NominalDiffTime -> SchedulingUpdate
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
      Again -> partialUpdate (head relearningSteps) Learning 0
      Hard -> let updatedInterval
                   | step == 0 && length relearningSteps == 1
                   = head relearningSteps * 1.5
                   | step == 0 -- omitting check for 2 or greater as it is implied
                   = let (s1:s2:_) = relearningSteps in (s1 + s2) / 2
                   | otherwise
                   = relearningSteps !! step
              in partialUpdate updatedInterval Learning step
      Good -> if step + 1 == length relearningSteps
                then partialUpdate (nextInterval conf updatedStability) Reviewing 0
                else partialUpdate (relearningSteps !! step + 1) Learning (step + 1)
      Easy -> partialUpdate (nextInterval conf updatedStability) Reviewing 0

shortOrLongTermStability :: SchedulingConfig -> Card -> Rating -> Maybe NominalDiffTime -> Stability
shortOrLongTermStability conf card rating mElapsedDays
  | Just d <- mElapsedDays, d < 1 = shortTerm
  | otherwise                     = longTerm
  where shortTerm = shortTermStability conf (cardStability card) rating
        longTerm = let retrievability = cardRetrievability conf card mElapsedDays
                   in nextStability conf (cardDifficulty card) retrievability rating (cardStability card)

-- Fractional of the number of days
daysSinceLastReview :: Card  -> UTCTime -> Maybe NominalDiffTime
daysSinceLastReview card reviewDate = fmap (\lastReview -> reviewDate `diffUTCTime` lastReview / nominalDay) (cardLastReview card)

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
  in difficultyBase + 1 - exp (ratingCoef * fromRating rating)

clampStability :: Stability -> Stability
clampStability stab = max stab stabilityMin

clampDifficulty :: Difficulty -> Difficulty
clampDifficulty = bound difficultyMin difficultyMax

decayFactor :: Double -> Double
decayFactor decay = 0.9 ** (1 / decay) - 1

nextInterval :: SchedulingConfig -> Stability -> NominalDiffTime
nextInterval conf stability =
  let decay = -(wRetrievabilityDecay $ scParameters conf)
      ret = scDesiredRetention conf
      maxInt = scMaximumInterval conf
      rawNextInterval = (stability / decayFactor decay) * (ret ** (1/(-decay)) - 1)
  in nominalDay * realToFrac (bound 1 maxInt (round rawNextInterval))

shortTermStability :: SchedulingConfig -> Stability -> Rating -> Double
shortTermStability conf stability rating =
  let params = scParameters conf
      boost = wShortTermStabilityExpCoef params
      rn = wShortTermStabilityRatingNormalization params
      se = wShortTermStabilityStabilityExp params
      shortTermInc = exp (boost * (fromRating rating - 2 + rn)) * (stability ** (-se))
      shortTermInc' = if rating == Good || rating == Easy
        then max 1 shortTermInc
        else shortTermInc
  in clampStability $ stability * shortTermInc'

nextDifficulty :: SchedulingConfig -> Difficulty -> Rating -> Double
nextDifficulty conf difficulty rating =
  let params = scParameters conf
      dc = wDifficultyDeltaCoef params
      mr = wDifficultyMeanReversion params
      easyDifficulty = initialDifficulty params Easy
      difficultyDelta = -(dc * (fromRating rating - 2))
      dampedDifficulty = difficulty + (10 - difficulty) * difficultyDelta / 9
      meanReversionDifficulty = mr * easyDifficulty + (1 - mr) * dampedDifficulty
  in clampDifficulty meanReversionDifficulty

nextStability :: SchedulingConfig -> Difficulty -> Double -> Rating -> Stability -> Double
nextStability conf difficulty retrievability rating stability = clampStability $ case rating of
  Again -> nextForgetStability conf difficulty retrievability stability
  _ -> nextRecallStability conf difficulty retrievability rating stability

nextForgetStability :: SchedulingConfig -> Difficulty -> Double -> Stability -> Double
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

nextRecallStability :: SchedulingConfig -> Difficulty -> Double -> Rating -> Stability -> Double
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

cardRetrievability :: SchedulingConfig -> Card -> Maybe NominalDiffTime -> Double
cardRetrievability conf card mElapsedDays = case mElapsedDays of
  Nothing -> 0
  Just elapsedDays -> let decay = -(wRetrievabilityDecay $ scParameters conf)
                      in (1 + decayFactor decay * realToFrac elapsedDays / cardStability card)

getFuzzedInterval :: NominalDiffTime -> IO NominalDiffTime
getFuzzedInterval interval = if interval / nominalDay < 2.5
  then pure interval
  else pure interval

