{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Text.Megaparsec.Time.Byte
-- Description : Time parsers for streams of 'Word8'.
-- Copyright   : (c) drlkf, 2026
-- License     : GPL-3
-- Maintainer  : drlkf@drlkf.net
-- Stability   : experimental
--
-- Parsers for types related to time for streams of 'Word8' (strict and lazy
-- 'Data.ByteString.ByteString').
module Text.Megaparsec.Time.Byte (
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
import Data.Word (Word8)
import Text.Megaparsec (MonadParsec, Token)
import Text.Megaparsec.Byte (char, char', digitChar, space, space1)
import Text.Megaparsec.Time.Common (
  DayResult,
  mkDateParser,
  mkDayParser,
  mkDurationParser,
  mkGregorianDayParser,
  mkTimeParser,
 )
import Text.Megaparsec.Utils.Byte (posNumParser)

fromChar :: Char -> Word8
fromChar = toEnum . fromEnum

toChar :: Word8 -> Char
toChar = toEnum . fromIntegral

-- | Parse a tuple containing a day or not, and a 'TimeOfDay', separated with a
-- space.
dateParser
  :: MonadFail m
  => MonadParsec e s m
  => Token s ~ Word8
  => m (Maybe DayResult, TimeOfDay)
dateParser = mkDateParser space1 dayParser timeParser

-- | Parse a day, see 'Text.Megaparsec.Time.Char.dayParser'.
dayParser
  :: MonadParsec e s m
  => Token s ~ Word8
  => m DayResult
dayParser = mkDayParser char' fromChar digitChar toChar

-- | Parse a 'NominalDiffTime' using strings like @1h23m45s@, with all
-- components being optional as long as one is present.
durationParser
  :: MonadFail m
  => MonadParsec e s m
  => Token s ~ Word8
  => m NominalDiffTime
durationParser = mkDurationParser space hoursParser minutesParser secondsParser

-- | Parse a Gregorian 'Day' from a @%F@ or @%d\/%m\/%Y@ format.
gregorianDayParser
  :: MonadFail m
  => MonadParsec e s m
  => Token s ~ Word8
  => m Day
gregorianDayParser = mkGregorianDayParser toChar

-- | Parse a 'NominalDiffTime' from a number of hours from a string like @1h@.
hoursParser
  :: MonadFail m
  => MonadParsec e s m
  => Token s ~ Word8
  => m NominalDiffTime
hoursParser = secondsToNominalDiffTime . (* 3600) <$> posNumParser <* char (fromChar 'h')

-- | Parse a 'NominalDiffTime' from a number of minutes from a string like @1m@.
minutesParser
  :: MonadFail m
  => MonadParsec e s m
  => Token s ~ Word8
  => m NominalDiffTime
minutesParser = secondsToNominalDiffTime . (* 60) <$> posNumParser <* char (fromChar 'm')

-- | Parse a 'NominalDiffTime' from a number of seconds from a string like @1s@.
secondsParser
  :: MonadFail m
  => MonadParsec e s m
  => Token s ~ Word8
  => m NominalDiffTime
secondsParser = secondsToNominalDiffTime <$> posNumParser <* optional (char (fromChar 's'))

-- | Parse a 'TimeOfDay' from a string like @01:23@.
timeParser
  :: MonadFail m
  => MonadParsec e s m
  => Token s ~ Word8
  => m TimeOfDay
timeParser = mkTimeParser digitChar (char (fromChar ':')) toChar
