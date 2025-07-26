module FSRS.Schedule.Utils where
import FSRS.Card (Card)
import FSRS.Rating (Rating)
import Data.Time (UTCTime)
import FSRS.Schedule (Scheduler, reviewCardFuzzAtTime, reviewCardAtTime, reviewCard, reviewCardFuzz)
import Data.Time.Clock (NominalDiffTime)
import FSRS.ReviewLog (ReviewLog)

-- | A set of small utility wrappers if users want a different API for card reviews

reviewCardAtTimeOptionalFuzz ::
  Scheduler -> Bool -> NominalDiffTime -> Card -> Rating -> UTCTime -> IO (Card, ReviewLog)
reviewCardAtTimeOptionalFuzz cfg shouldFuzz c d r u =
  if shouldFuzz
    then reviewCardFuzzAtTime cfg c d r u
    else pure $ reviewCardAtTime cfg c d r u

reviewCardOptionalReviewDate ::
  Scheduler -> NominalDiffTime -> Card -> Rating -> Maybe UTCTime -> IO (Card, ReviewLog)
reviewCardOptionalReviewDate cfg d c r mu =
  case mu of
    Nothing -> reviewCard cfg d c r
    Just u -> pure $ reviewCardAtTime cfg d c r u

reviewCardFuzzOptionalReviewDate ::
  Scheduler -> NominalDiffTime -> Card -> Rating -> Maybe UTCTime -> IO (Card, ReviewLog)
reviewCardFuzzOptionalReviewDate cfg d c r mu =
  case mu of
    Nothing -> reviewCardFuzz cfg d c r
    Just u ->  reviewCardFuzzAtTime cfg d c r u

reviewCardNowOptionalFuzz ::
  Scheduler -> NominalDiffTime -> Bool -> Card -> Rating -> IO (Card, ReviewLog)
reviewCardNowOptionalFuzz cfg d shouldFuzz c r =
  if shouldFuzz
    then reviewCardFuzz cfg d c r
    else reviewCard cfg  d c r

-- Main full options
reviewCardFullOptions ::
  Scheduler -> Bool -> NominalDiffTime -> Card -> Rating -> Maybe UTCTime -> IO (Card, ReviewLog)
reviewCardFullOptions cfg shouldFuzz d c r mu =
  if shouldFuzz 
    then reviewCardFuzzOptionalReviewDate cfg d c r mu
    else reviewCardOptionalReviewDate cfg  d c r mu
