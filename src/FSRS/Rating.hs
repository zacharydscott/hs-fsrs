module FSRS.Rating where

import Data.Aeson (FromJSON (..), withScientific, ToJSON, Value (Number))
import Data.Aeson.Types (ToJSON(..))

data Rating = Again
            | Hard
            | Good
            | Easy
              deriving (Show, Eq)

-- Diverging from py implementation slightly
-- I see no reason not to 0 index this
fromRating :: Num a => Rating -> a
fromRating rating = case rating of
  Again -> 0
  Hard  -> 1
  Good  -> 2
  Easy  -> 3

toRating :: (Num a, Eq a) => a -> Maybe Rating
toRating v = case v of
  0 -> Just Again
  1 -> Just Hard
  2 -> Just Good
  3 -> Just Easy
  _ -> Nothing

instance FromJSON Rating where
  parseJSON = withScientific "FSRS Review Rating" $ \v ->
    case v of
      0 -> pure Again
      1 -> pure Hard
      2 -> pure Good
      3 -> pure Easy
      _ -> fail $ "Invalid Review Rating value: " <> show v

instance ToJSON Rating where
  toJSON = Number . fromRating
