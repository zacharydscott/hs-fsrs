{-# LANGUAGE DeriveGeneric #-}

module FSRS.ReviewLog where

import Data.Aeson (FromJSON (parseJSON), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Time (NominalDiffTime, UTCTime)
import FSRS.Rating (Rating)
import FSRS.Utils (genericParseOptionsWithPrefix)
import GHC.Generics (Generic)

data ReviewLog = ReviewLog
  { revLogCardId :: Int,
    revLogRating :: Rating,
    revLogReviewDatetime :: UTCTime,
    revLogReviewDuration :: NominalDiffTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReviewLog where
  parseJSON = genericParseJSON $ genericParseOptionsWithPrefix "revLog"

instance ToJSON ReviewLog where
  toJSON = genericToJSON $ genericParseOptionsWithPrefix "revLog"
