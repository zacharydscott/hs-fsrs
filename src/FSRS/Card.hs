{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module FSRS.Card where

import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (Number),
  withScientific, withObject, (.:), object, (.=), (.:?)
  )
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import FSRS.Utils (blankDate)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (fromMaybe)

type Stability = Double
type Difficulty = Double

-- | Serializes to 0, 1, 2, and 3 for New, Learning,
-- | Reviewing, and Relearning, respectively.
data CardState = New
               | Learning
               | Reviewing
               | Relearning
                 deriving (Show, Eq)

fromCardState :: Num a => CardState -> a
fromCardState rating = case rating of
  New        -> 0
  Learning   -> 1
  Reviewing  -> 2
  Relearning -> 3

toCardState :: (Num a, Eq a) => a -> Maybe CardState
toCardState v = case v of
  0 -> Just New
  1 -> Just Learning
  2 -> Just Reviewing
  3 -> Just Relearning
  _ -> Nothing

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

blankCard :: Card
blankCard = Card
  { cardId          = 0
  , cardState       = New
  , cardDue         = blankDate
  , cardLastReview  = Nothing
  , cardStep        = 0
  , cardLapses      = 0
  , cardRepetitions = 0
  , cardStability   = 0.0
  , cardDifficulty  = 0.0
  }

-- | make a new card with given id and state
mkCard :: Int -> CardState -> Card
mkCard cid state = blankCard { cardId = cid, cardState = state }

-- | Create's a new card using the current time in milliseconds since Unix Epoch
newCard :: IO Card
newCard = do
  posixTime <- round . (*1000) <$> getPOSIXTime
  return $ mkCard posixTime New

instance FromJSON CardState where
  parseJSON = withScientific "FSRS Card State" $ \v ->
    case v of
      0 -> pure New
      1 -> pure Learning
      2 -> pure Reviewing
      3 -> pure Relearning
      _ -> fail $ "Invalid CardState value: " <> show v

instance ToJSON CardState where
  toJSON = Number . fromCardState

instance FromJSON Card where
  parseJSON = withObject "FSRS.Card" $ \v -> do
    cardId <- v .: "card_id"
    cardState <- v .: "state"
    cardDue <- v .: "due"
    cardLastReview <- v .: "last_review"
    mCardStep <- v .:? "step"
    mCardLapses <- v .:? "lapses"
    mCardRepetitions <- v .:? "repetitions"
    cardStability <- v .: "stability"
    cardDifficulty <- v .: "difficulty"
    let cardStep = fromMaybe 0 mCardStep
        cardLapses = fromMaybe 0 mCardLapses
        cardRepetitions = fromMaybe 0 mCardRepetitions
    return Card {..}

instance ToJSON Card where
  toJSON Card {..} = object
    [ "card_id" .= cardId
    , "state" .= cardState
    , "due" .= cardDue
    , "last_review" .= cardLastReview
    , "step" .= cardStep
    , "lapses" .= cardLapses
    , "repetitions" .= cardRepetitions
    , "stability" .= cardStability
    , "difficulty" .= cardDifficulty
    ]
