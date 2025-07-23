module FSRS.Schedule where

import FSRS.Card (Card(..), CardState(..))
import FSRS.Parameters (Parameters(..), stabilityMin, difficultyMin, difficultyMax)
import FSRS.Rating (Rating(..), fromRating)
import Data.Time (NominalDiffTime, diffUTCTime, nominalDay)
import Data.Int (Int32)
import Data.Time.Clock (UTCTime)
import FSRS.Utils (bound)

data SchedulingConfig = SchedulingConfig
  { scParameters       :: Parameters
  , scDesiredRetention :: Double
  , scLearningSteps    :: [NominalDiffTime]
  , scRelearningSteps  :: [NominalDiffTime]
  , scMaximumInterval  :: Int32
  } deriving (Show)

reviewCard :: SchedulingConfig -> Card -> Rating -> UTCTime -> Int32 -> Card
reviewCard conf card rating reviewDate reviewDuration =
  let daysSinceLastReview = round $ (reviewDate `diffUTCTime` cardLastReview card) / nominalDay :: Int32
      params = scParameters conf
      cState = cardState card
  in case cState of
    New -> let stability = initialStability params rating
               difficulty = initialDifficulty params rating
           in card


initialStability :: Parameters -> Rating -> Double
initialStability params rating = clampStability $ case rating of
  Again -> wInitialStabilityAgain params
  Hard  -> wInitialStabilityHard  params
  Good  -> wInitialStabilityGood  params
  Easy  -> wInitialStabilityEasy  params

initialDifficulty :: Parameters -> Rating -> Double
initialDifficulty params rating = clampDifficulty $
  let difficultyBase = wInitialDifficultyBaseline params
      ratingCoef = wInitialDifficultyRatingCoef params
  in difficultyBase + 1 - exp (ratingCoef * fromRating rating)

clampStability :: Double -> Double
clampStability stab = max stab stabilityMin

clampDifficulty :: Double -> Double
clampDifficulty = bound difficultyMin difficultyMax

decayFactor :: Double -> Double
decayFactor decay = 0.9 ** (1 / decay) - 1

nextInterval :: SchedulingConfig -> Double -> Int32
nextInterval conf stability =
  let decay = -(wRetrievabilityDecay $ scParameters conf)
      ret = scDesiredRetention conf
      maxInt = scMaximumInterval conf
      rawNextInterval = (stability / decayFactor decay) * (ret ** (1/(-decay)) - 1)
  in bound 1 maxInt (round rawNextInterval)

shortTermStability :: SchedulingConfig -> Double -> Rating -> Double
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

nextDifficulty :: SchedulingConfig -> Double -> Rating -> Double
nextDifficulty conf difficulty rating =
  let params = scParameters conf
      dc = wDifficultyDeltaCoef params
      mr = wDifficultyMeanReversion params
      easyDifficulty = initialDifficulty params Easy
      difficultyDelta = -(dc * (fromRating rating - 2))
      dampedDifficulty = difficulty + (10 - difficulty) * difficultyDelta / 9
      meanReversionDifficulty = mr * easyDifficulty + (1 - mr) * dampedDifficulty
  in clampDifficulty meanReversionDifficulty

nextStability :: SchedulingConfig -> Double -> Double -> Rating -> Double -> Double
nextStability conf difficulty retrievability rating stability = clampStability $ case rating of
  Again -> nextForgetStability conf difficulty retrievability stability
  _ -> nextRecallStability conf difficulty retrievability rating stability

nextForgetStability :: SchedulingConfig -> Double -> Double -> Double -> Double
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

nextRecallStability :: SchedulingConfig -> Double -> Double -> Rating -> Double -> Double
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
