module FSRS.Utils where

import Data.Aeson
  ( Options (fieldLabelModifier),
    Value (..),
    camelTo2,
    defaultOptions,
  )
import Data.Char (toLower)
import Data.Scientific (toRealFloat)
import Data.Time (NominalDiffTime, UTCTime, fromGregorian, nominalDay)
import Data.Time.Clock (UTCTime (..))

blankDate :: UTCTime
blankDate = UTCTime {utctDay = fromGregorian 1970 1 1, utctDayTime = 0}

parseDoubleFromJSON :: Value -> Either String Double
parseDoubleFromJSON (Number n) = Right $ toRealFloat n
parseDoubleFromJSON v = Left $ "Expected number, but recieved value " ++ show v

genericParseOptionsWithPrefix :: String -> Options
genericParseOptionsWithPrefix prefix = defaultOptions {fieldLabelModifier = snakeCase . lowerFirst . drop (length prefix)}

lowerFirst :: String -> String
lowerFirst "" = ""
lowerFirst (c : cs) = toLower c : cs

snakeCase :: String -> String
snakeCase = camelTo2 '_'

-- | 1st min val, 2nd max val, 3rd value to bound
bound :: Ord a => a -> a -> a -> a
bound minVal maxVal = min maxVal . max minVal

nominalMinute :: NominalDiffTime
nominalMinute = nominalDay / (60 * 24)
