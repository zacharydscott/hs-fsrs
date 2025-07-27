module FSRS.Parameters where

import Data.Aeson (FromJSON, ToJSON (toJSON), withArray)
import Data.Aeson.Types (FromJSON(parseJSON))
import qualified Data.Vector as V
import FSRS.Utils (parseDoubleFromJSON)

stabilityMin :: Double
stabilityMin = 0.002

difficultyMin :: Double
difficultyMin = 1.00

difficultyMax :: Double
difficultyMax = 10.00

initStabilityMax :: Double
initStabilityMax = 100.0

-- | Main FSRS Parameters, ToJSON and FromJSON serialize to expected number array
data Parameters = Parameters
  { wInitialStabilityAgain :: Double -- 0
  , wInitialStabilityHard :: Double -- 1
  , wInitialStabilityGood :: Double -- 2
  , wInitialStabilityEasy :: Double -- 3
  , wInitialDifficultyBaseline :: Double -- 4
  , wInitialDifficultyRatingCoef :: Double -- 5

  , wDifficultyDeltaCoef :: Double -- 6
  , wDifficultyMeanReversion :: Double -- 7

  , wRecallStabilityGrowthRate :: Double -- 8
  , wRecallStabilityDamper :: Double -- 9
  , wRecallStabilityRetievabilityCoef :: Double -- 10

  , wForgetStabilityLongTermBase :: Double -- 11
  , wForgetStabilityDifficultyExp :: Double -- 12
  , wForgetStabilityStabilityExp :: Double -- 13
  , wForgetStabilityRetrievabilityCoef :: Double -- 14
  , wRecallStabilityHardPenalty :: Double -- 15
  , wRecallStabilityEasyBonus :: Double -- 16

  , wShortTermStabilityExpCoef :: Double -- 17
  , wShortTermStabilityRatingNormalization :: Double -- 18
  , wShortTermStabilityStabilityExp :: Double -- 19
  , wRetrievabilityDecay :: Double -- 20
  } deriving (Show, Eq)

defaultParameters :: Parameters
defaultParameters = Parameters
  { wInitialStabilityAgain = 0.2172 -- 0
  , wInitialStabilityHard = 1.1771  -- 1
  , wInitialStabilityGood = 3.2602  -- 2
  , wInitialStabilityEasy = 16.1507  -- 3
  , wInitialDifficultyBaseline = 7.0114  -- 4
  , wInitialDifficultyRatingCoef = 0.57  -- 5
  , wDifficultyDeltaCoef = 2.0966  -- 6
  , wDifficultyMeanReversion = 0.0069  -- 7
  , wRecallStabilityGrowthRate = 1.5261  -- 8
  , wRecallStabilityDamper = 0.112  -- 9
  , wRecallStabilityRetievabilityCoef = 1.0178 -- 10
  , wForgetStabilityLongTermBase = 1.849  -- 11
  , wForgetStabilityDifficultyExp = 0.1133  -- 12
  , wForgetStabilityStabilityExp = 0.3127  -- 13
  , wForgetStabilityRetrievabilityCoef = 2.2934  -- 14
  , wRecallStabilityHardPenalty = 0.2191  -- 15
  , wRecallStabilityEasyBonus = 3.0004  -- 16
  , wShortTermStabilityExpCoef = 0.7536  -- 17
  , wShortTermStabilityRatingNormalization = 0.3332  -- 18
  , wShortTermStabilityStabilityExp = 0.1437  -- 19
  , wRetrievabilityDecay = 0.2 -- 20
  }

listToParameters :: [Double] -> Either String Parameters
listToParameters [w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17, w18, w19, w20] =
  Right $ Parameters w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17 w18 w19 w20
listToParameters weights =  Left $ "Expected 21 parameters, recieved " ++ show (length weights)

parametersToList :: Parameters -> [Double]
parametersToList
  (Parameters w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17 w18 w19 w20) =
  [w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17, w18, w19, w20]

instance FromJSON Parameters where
  parseJSON = withArray "FSRS Parameters" $ \arr -> do
    let v = V.toList arr
    if length v /= 21
      then fail $ "Expected 21 parameters, got " <> show (length v)
      else do
        let eitherParams = traverse parseDoubleFromJSON v >>= listToParameters
        case eitherParams of
          Right params -> pure params
          Left err -> fail err

instance ToJSON Parameters where
  toJSON = toJSON . parametersToList
