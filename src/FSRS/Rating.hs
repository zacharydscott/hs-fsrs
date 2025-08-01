module FSRS.Rating where

import Data.Aeson (FromJSON (..), ToJSON, Value (Number), withScientific)
import Data.Aeson.Types (ToJSON (..))

data Rating
  = Again
  | Hard
  | Good
  | Easy
  deriving (Show, Eq)

fromRating :: Num a => Rating -> a
fromRating rating = case rating of
  Again -> 1
  Hard -> 2
  Good -> 3
  Easy -> 4

toRating :: (Num a, Eq a) => a -> Maybe Rating
toRating v = case v of
  1 -> Just Again
  2 -> Just Hard
  3 -> Just Good
  4 -> Just Easy
  _ -> Nothing

instance FromJSON Rating where
  parseJSON = withScientific "FSRS Review Rating" $ \v ->
    case v of
      1 -> pure Again
      2 -> pure Hard
      3 -> pure Good
      4 -> pure Easy
      _ -> fail $ "Invalid Review Rating value: " <> show v

instance ToJSON Rating where
  toJSON = Number . fromRating
