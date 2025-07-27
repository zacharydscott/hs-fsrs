# HS-FSRS
This library provides a Haskell implementation of the Free Spaced Repetition algorithm from the [Open Spaced Repetition](https://github.com/open-spaced-repetition) group.
<b>This project is currently In progress, this will be submitted to join the organization after completion</b>
The FSRS algorithm is a free and open source algorithm with scientific support to maximize learning efficiency
by scheduling exposure to information at optimal times, or spaced repetition. The ubiquity of such systems for
flashcard systems is why each item tracked is called a "card," but anything data can be associated. This
library provides the card update algorithm, which updates schedules and updates card properties for better
scheduling in the future.

The optimizer algorithm, which updates the core meta-parameters of the algorithm on a per-user basis is in
progress, and will be provided in the hs-rs-fsrs (a lot of 's's, I know) library. You can read more about FSRS system [here](https://github.com/open-spaced-repetition/free-spaced-repetition-scheduler).   

## Table of Contents
- [Installation](#installation)
- [Quickstart](#quickstart)
- [Usage](#usage)
- [Haskell Specifics (Start here if you have limited Haskell Experience)](#haskell-specifics)
- [Reference](#reference)
- [API Documentation](#api-documentation)
- [Contribute](#contribute)

## Installation
-- in progress

## Quickstart

You can import FSRS and use the default scheduler and card creation.

```Haskell
module Example where

import FSRS

main :: IO ()
main = do
  card <- newCard
  putStrLn "New card with timestamp date"
  print card
  (card', reviewLog) <- reviewCard defaultScheduler 9 card Good 
  --                                                   |      | Again, Hard, Good, or Easy
  --                                                   | the duration of time in seconds that the review took
  putStrLn "Card after 'Good' review"
  print card'
  putStrLn "Review log"
  print reviewLog
```

The default `newCard` is `IO Card`, because it sets the current Posix millisecond as the id.
Likewise, `reviewCard` sets the review date as the current time. 

Not all use cases will take advantage of the review duration, so it is placed to allow easy currying.
Likewise, you can simply toss the review log if it's of no use for your application.

```Haskell
module Example where

import FSRS

schedule :: Card -> Rating -> IO (Card, ReviewLog)
review = fst <$> reviewCard defaultScheduler 0

main :: IO ()
main = do
  card <- newCard
  card' <- review card Good
  putStrLn "First review"
  print card'
  card'' <- review card' Easy
  putStrLn "Second review"
  print card''
```

Fuzzing is available, as well as explicitly setting the review time, and more explicit card creation.

```Haskell
module Example where

import FSRS
import Data.Time (fromGregorian, UTCTime(..))

-- Note that reviewCardAtTime is not an IO function
reviewCardCycle :: Int -> Card
reviewCardCycle cid =
  let card = mkCard cid New
      let reviewTime = (fromGregorian 1970 1 1) 0
  in reviewCardAtTime defaultScheduler 0 card Hard reviewTime

main :: IO ()
main = do
  let (cardFromPure, _) = reviewCardCycle 1
  putStrLn "Card from pure generation function and review function"
  print cardFromPure
  cardNormal <- newCard
  (fuzzedCard, _) <- reviewCardFuzz 0 card Again
  putStrLn "fuzzed result: due date may change. Are yuo feeling lucky?"
  print fuzzedCard
  let reviewTime = (fromGregorian 1970 1 1) 0
  -- The fuzzed "atTime" is though, as randomness requires IO
  (fuzzedExplicitDateCard, _) <- reviewCardFuzzWithDate 0 card Again reviewTime
  putStrLn "fuzzed result: deep in the past either way"
  print fuzzedExplicitDateCard
```

Finally, alternative APIs are available in the `FSRS.Utils` module. `reviewCardFullOptions` allows both fuzzing
and explicit dates to be dynamic. This is similar to other implementations scheduler classes.

```Haskell
module Example where

import FSRS.Utils
import Data.Time (fromGregorian, UTCTime(..))

      let reviewTime = (fromGregorian 1970 1 1) 0
main :: IO ()
main = do
  let reviewTime = (fromGregorian 1970 1 1) 0
  card <- newCard
  (fuzzAndDateCard, fuzzAndDateLog) <- reviewCardFullOptions
    -- full arguments
    defaultScheduler True 15 card Good (Just reviewTime)
  putStrLn "fuzzed review with explicit date card and review log"
  print fuzzAndDateCard
  print fuzzAndDateLog
  (noFuzzCurrentTimeCard, noFuzzCurrentTimeLog) <- reviewCardFullOptions
    -- full arguments
    defaultScheduler False 15 card Good Nothing
  putStrLn "Non-fuzzed card with current time"
  print noFuzzCurrentTimeCard
  print noFuzzCurrentTimeLog
```

## Haskell Specifics
This is not a lecture Haskell, but an explanation for some design decisions which differ from implementations
in imperative languages. This is still a proposal, so if you disagree with any decision, open an issue and let
me know. There were no advanced features necessary or proper, so it should be fairly straightforward.

There are really only a few data types declared and some functions. There are no classes or methods in Haskell,
so the main card review functions take in a `Scheduler`, which is really more of a scheduling configuration.
Instead of one `reviewCardTime` function, it was split into `reviewCardAtTime`, `reviewCardFuzz`, 
`reviewCardNow`, and `reviewCardFuzzNow` for a couple of reasons. Accessing random numbers is
non-deterministic, and requires using the IO monad. The actual algorithm itself is entirely pure outside 
of that however, so there should be an option to use it as a plain function.
The other split between "now" versions is for the same reason. Getting current time is IO, but these are 
really simple wrappers. Moreover, it is really just a utility wrapper around the `reviewCardAtTime` function.
The only way to wrap everything together would be to force it into an IO monad in all situations and force
the date review argument to be a `Maybe UTCTime`. This is not particularly idiomatic. That's why those are 
kept in the `FSRS.Schedule.Utils` module.

One thing that may look a bit odd if you're unfamiliar with Haskell is the prefixing in data types. They look
like structs, but it's more complicated, unfortunately. Access occurs by `cardId card` to get a card's id for
instance, so each record is in the main namespace. If it was simply `id` then it would clash with the `id`
function.

One more, slightly more technical point: There could be a `SpaceRepetitionScheduler` typeclass, and the
Scheduler could have an instance of it. There are two main reasons I haven't. Primarily, the inputs and outputs
of the FSRS algorithm seem fairly unique, The SM-2 or similar would not have the same arguments. Secondly,
if it later becomes necessary, it should be possible to add one in later without breaking the current API.
The main reviewCard and related functions would still be exported from `FSRS`, it would only affect users
importing from the sub-modules.
