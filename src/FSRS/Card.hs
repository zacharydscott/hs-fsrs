{-# LANGUAGE DeriveGeneric #-}

module FSRS.Card where

import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (Number),
  genericParseJSON,
  genericToJSON,
  withScientific
  )
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import FSRS.Utils (genericParseOptionsWithPrefix)

type Stability = Double
type Difficulty = Double

-- | Serializes to 0, 1, 2, and 3 for New, Learning,
-- | Reviewing, and Relearning, respectively.
data CardState = New
               | Learning
               | Reviewing
               | Relearning
                 deriving (Show, Eq)

data Card = Card
  { cardId          :: Int
  , cardState       :: CardState
  , cardDue         :: UTCTime
  , cardLastReview  :: Maybe UTCTime
  , cardStep        :: Int
  , cardLapses      :: Int
  , cardRepetitions :: Int
  , cardStability   :: Stability
  , cardDifficulty  :: Difficulty
  } deriving (Show, Eq, Generic)

instance FromJSON CardState where
  parseJSON = withScientific "FSRS Card State" $ \v ->
    case v of
      0 -> pure New
      1 -> pure Learning
      2 -> pure Reviewing
      3 -> pure Relearning
      _ -> fail $ "Invalid CardState value: " <> show v

instance ToJSON CardState where
  toJSON cs = Number $ case cs of
    New        -> 0
    Learning   -> 1
    Reviewing  -> 2
    Relearning -> 3

instance FromJSON Card where
  parseJSON = genericParseJSON $ genericParseOptionsWithPrefix "card"

instance ToJSON Card where
  toJSON = genericToJSON $ genericParseOptionsWithPrefix "card"
