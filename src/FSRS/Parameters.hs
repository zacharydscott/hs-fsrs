module FSRS.Parameters where

import Data.Aeson (FromJSON, Value (Number), ToJSON (toJSON), withArray)
import Data.Aeson.Types (FromJSON(parseJSON))
import Data.Scientific (toRealFloat)
import qualified Data.Vector as V

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
  , wRecallStabilityHardPenalty :: Double -- 15
  , wRecallStabilityEasyBonus :: Double -- 16

  , wForgetStabilityLongTermBase :: Double -- 11
  , wForgetStabilityDifficultyExp :: Double -- 12
  , wForgetStabilityStabilityExp :: Double -- 13
  , wForgetStabilityRetrievabilityCoef :: Double -- 14

  , wShortTermStabilityExpCoef :: Double -- 17
  , wShortTermStabilityRatingNormalization :: Double -- 18
  , wShortTermStabilityStabilityExp :: Double -- 19
  , wRetrievabilityDecay :: Double -- 20
  } deriving (Show, Eq)

instance FromJSON Parameters where
  parseJSON = withArray "FSRS Parameters" $ \arr -> do
    let v = V.toList arr
    if length v /= 21
      then fail $ "Expected 21 parameters, got " <> show (length v)
      else do
        let val = valFrom v
        pure $ Parameters
          (val 0)  (val 1)  (val 2)  (val 3)
          (val 4)  (val 5)  (val 6)  (val 7)
          (val 8)  (val 9)  (val 10)
          (val 15) (val 16)
          (val 11) (val 12) (val 13) (val 14)
          (val 17) (val 18) (val 19) (val 20)
      where valFrom v i = parseNumber (v !! i) i
            parseNumber (Number n) _ = toRealFloat n
            parseNumber _ i = error $ "Expected number at index " <> show i

instance ToJSON Parameters where
  toJSON (Parameters a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a15 a16 a11 a12 a13 a14 a17 a18 a19 a20) =
    toJSON
      [ a0, a1, a2, a3, a4, a5
      , a6, a7, a8, a9, a10
      , a11, a12, a13, a14
      , a15, a16
      , a17, a18, a19, a20
      ]
