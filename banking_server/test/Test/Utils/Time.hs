module Test.Utils.Time where

import           Data.Time
import           Data.Fixed

-- https://wiki.haskell.org/Time
-- https://williamyaoh.com/posts/2019-09-16-time-cheatsheet.html
mkUTCTime :: Integer
          -> Int
          -> Int
          -> Int
          -> Int
          -> Pico
          -> UTCTime
mkUTCTime year mon day hour mn sec =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour mn sec))
