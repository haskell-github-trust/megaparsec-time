{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Text.Megaparsec.Time.Char
-- Description : Time parsers for streams of 'Char'.
-- Copyright   : (c) drlkf, 2024
-- License     : GPL-3
-- Maintainer  : drlkf@drlkf.net
-- Stability   : experimental
--
-- Parsers for types related to time for streams of 'Char' ('String', 'Text',
-- lazy 'Text').
module Text.Megaparsec.Time.Char (
  -- * Simple parsers
  hoursParser,
  minutesParser,
  secondsParser,
  timeParser,
  dayParser,
  gregorianDayParser,

  -- * Composite parsers
  dateParser,
  durationParser,
) where

import Control.Applicative (optional)
import Data.Time (Day, NominalDiffTime, TimeOfDay, secondsToNominalDiffTime)
import Text.Megaparsec (MonadParsec, Token)
import Text.Megaparsec.Char (char, char', digitChar, space, space1)
import Text.Megaparsec.Time.Common (
  DayResult,
  mkDateParser,
  mkDayParser,
  mkDurationParser,
  mkGregorianDayParser,
  mkTimeParser,
 )
import Text.Megaparsec.Utils.Char (posNumParser)

-- | Parse a tuple containing a day or not, and a 'TimeOfDay', separated with a
-- space.
dateParser
  :: MonadFail m
  => MonadParsec e s m
  => Token s ~ Char
  => m (Maybe DayResult, TimeOfDay)
dateParser = mkDateParser space1 dayParser timeParser

-- | Parse a day using one of the following, all case-insensitive:
--
--   * a short (3-letters) or long day name e.g @mon@ or @monday@
--   * @yesterday@ or @tomorrow@
--   * a day number relative to the current day i.e @+2@ is two days from today
--   * an absolute number for a 'Data.Time.DayOfWeek', refer to its 'Num' instance for more information.
dayParser
  :: MonadParsec e s m
  => Token s ~ Char
  => m DayResult
dayParser = mkDayParser char' id digitChar id

-- | Parse a 'NominalDiffTime' using strings like @1h23m45s@, with all
-- components being optional as long as one is present.
durationParser
  :: MonadFail m
  => MonadParsec e s m
  => Token s ~ Char
  => m NominalDiffTime
durationParser = mkDurationParser space hoursParser minutesParser secondsParser

-- | Parse a Gregorian 'Day' from a @%F@ or @%d\/%m\/%Y@ format.
gregorianDayParser
  :: MonadFail m
  => MonadParsec e s m
  => Token s ~ Char
  => m Day
gregorianDayParser = mkGregorianDayParser id

-- | Parse a 'NominalDiffTime' from a number of hours from a string like @1h@.
hoursParser
  :: MonadFail m
  => MonadParsec e s m
  => Token s ~ Char
  => m NominalDiffTime
hoursParser = secondsToNominalDiffTime . (* 3600) <$> posNumParser <* char 'h'

-- | Parse a 'NominalDiffTime' from a number of minutes from a string like @1m@.
minutesParser
  :: MonadFail m
  => MonadParsec e s m
  => Token s ~ Char
  => m NominalDiffTime
minutesParser = secondsToNominalDiffTime . (* 60) <$> posNumParser <* char 'm'

-- | Parse a 'NominalDiffTime' from a number of seconds from a string like @1s@.
secondsParser
  :: MonadFail m
  => MonadParsec e s m
  => Token s ~ Char
  => m NominalDiffTime
secondsParser = secondsToNominalDiffTime <$> posNumParser <* optional (char 's')

-- | Parse a 'TimeOfDay' from a string like @01:23@.
timeParser
  :: MonadFail m
  => MonadParsec e s m
  => Token s ~ Char
  => m TimeOfDay
timeParser = mkTimeParser digitChar (char ':') id
