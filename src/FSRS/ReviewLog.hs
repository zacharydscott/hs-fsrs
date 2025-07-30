{-# LANGUAGE DeriveGeneric #-}
module FSRS.ReviewLog where

import FSRS.Rating (Rating)
import Data.Time (UTCTime, NominalDiffTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (..), FromJSON (parseJSON), genericToJSON, genericParseJSON)
import FSRS.Utils (genericParseOptionsWithPrefix)

data ReviewLog = ReviewLog
  { revLogCardId         :: Int
  , revLogRating         :: Rating
  , revLogReviewDatetime       :: UTCTime
  , revLogReviewDuration :: NominalDiffTime
  } deriving (Show, Eq, Generic)

instance FromJSON ReviewLog where
  parseJSON = genericParseJSON $ genericParseOptionsWithPrefix "revLog"

instance ToJSON ReviewLog where
  toJSON = genericToJSON $ genericParseOptionsWithPrefix "revLog"

