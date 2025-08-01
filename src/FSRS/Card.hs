{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FSRS.Card where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (Null, Number),
    object,
    withObject,
    withScientific,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import FSRS.Utils (blankDate)
import GHC.Generics (Generic)

type Stability = Double

type Difficulty = Double

-- | Serializes to 0, 1, 2, and 3 for New, Learning,
-- | Reviewing, and Relearning, respectively.
data CardState
  = Learning
  | Reviewing
  | Relearning
  deriving (Show, Eq)

fromCardState :: Num a => CardState -> a
fromCardState rating = case rating of
  Learning -> 1
  Reviewing -> 2
  Relearning -> 3

toCardState :: (Num a, Eq a) => a -> Maybe CardState
toCardState v = case v of
  1 -> Just Learning
  2 -> Just Reviewing
  3 -> Just Relearning
  _ -> Nothing

data Card
  = NewCard Int
  | ActiveCard CardDetails
  deriving (Show, Eq)

data CardDetails = CardDetails
  { cardId :: Int,
    cardState :: CardState,
    cardDue :: UTCTime,
    cardLastReview :: UTCTime,
    cardStep :: Int,
    cardLapses :: Int,
    cardRepetitions :: Int,
    cardStability :: Stability,
    cardDifficulty :: Difficulty
  }
  deriving (Show, Eq, Generic)

isNew :: Card -> Bool
isNew (NewCard _) = True
isNew _ = False

getCardId :: Card -> Int
getCardId (NewCard cid) = cid
getCardId (ActiveCard d) = cardId d

isLearning :: Card -> Bool
isLearning (NewCard _) = False
isLearning (ActiveCard d) = cardState d == Learning

isReviewing :: Card -> Bool
isReviewing (NewCard _) = False
isReviewing (ActiveCard d) = cardState d == Reviewing

isRelearning :: Card -> Bool
isRelearning (NewCard _) = False
isRelearning (ActiveCard d) = cardState d == Relearning

getDetails :: Card -> Maybe CardDetails
getDetails (NewCard _) = Nothing
getDetails (ActiveCard d) = Just d

getLapses :: Card -> Int
getLapses card = maybe 0 cardLapses (getDetails card)

getRepetitions :: Card -> Int
getRepetitions card = maybe 0 cardRepetitions (getDetails card)

-- | Create's a new card using the current time in milliseconds since Unix Epoch
newCard :: IO Card
newCard = do
  posixTime <- round . (* 1000) <$> getPOSIXTime
  return $ NewCard posixTime

instance FromJSON CardState where
  parseJSON = withScientific "FSRS Card State" $ \v ->
    case v of
      1 -> pure Learning
      2 -> pure Reviewing
      3 -> pure Relearning
      _ -> fail $ "Invalid CardState value: " <> show v

instance ToJSON CardState where
  toJSON = Number . fromCardState

-- Some special cases are here for interoperability with the Python implementation
instance FromJSON Card where
  parseJSON = withObject "FSRS.Card" $ \v -> do
    cardId <- v .: "card_id"
    mLastReview <- v .:? "last_review" :: Parser (Maybe UTCTime)
    -- if there is no last review date, the card hasn't been reviewed
    -- this is more reliable than the state type, as new and learning
    -- share '1' for interoperability with the python implementation
    case mLastReview of
      Nothing -> return $ NewCard cardId
      Just cardLastReview -> do
        cardState <- v .: "state"
        cardDue <- v .: "due"
        cardStep <- fromMaybe 0 <$> v .: "step"
        cardLapses <- fromMaybe 0 <$> v .:? "lapses"
        cardRepetitions <- fromMaybe 0 <$> v .:? "repetitions"
        cardStability <- v .: "stability"
        cardDifficulty <- v .: "difficulty"
        return $ ActiveCard $ CardDetails {..}

instance ToJSON Card where
  toJSON (NewCard cid) =
    object
      [ "card_id" .= cid,
        "state" .= Learning,
        "due" .= blankDate, -- purely because of bup in python implementation
        "last_review" .= Null,
        "step" .= Null,
        "stability" .= Null,
        "difficulty" .= Null
      ]
  toJSON (ActiveCard (CardDetails {..})) =
    object
      [ "card_id" .= cardId,
        "state" .= cardState,
        "due" .= cardDue,
        "last_review" .= cardLastReview,
        "step" .= cardStep,
        "lapses" .= cardLapses,
        "repetitions" .= cardRepetitions,
        "stability" .= cardStability,
        "difficulty" .= cardDifficulty
      ]
