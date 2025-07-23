module FSRS.Rating where

data Rating = Again
            | Hard
            | Good
            | Easy
              deriving (Eq)

-- Diverging from py implementation slightly
-- I see no reason not to 0 index this
fromRating :: Num a => Rating -> a
fromRating rating = case rating of
  Again -> 0
  Hard  -> 1
  Good  -> 2
  Easy  -> 3
