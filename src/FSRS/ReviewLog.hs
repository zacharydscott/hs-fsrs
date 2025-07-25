module FSRS.ReviewLog where

import FSRS.Rating (Rating)
import Data.Time (UTCTime, NominalDiffTime)

data ReviewLog = ReviewLog
  { revLogCardId         :: Int
  , revLogRating         :: Rating
  , revLogDatetime       :: UTCTime
  , revLogReviewDuration :: NominalDiffTime
  }
