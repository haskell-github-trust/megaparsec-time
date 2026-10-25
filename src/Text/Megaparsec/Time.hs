-- |
-- Module      : Text.Megaparsec.Time
-- Description : Various parsers for types related to time.
-- Copyright   : (c) drlkf, 2024
-- License     : GPL-3
-- Maintainer  : drlkf@drlkf.net
-- Stability   : experimental
--
-- Re-exports of 'Text.Megaparsec.Time.Char'. New code should use
-- 'Text.Megaparsec.Time.Char' or 'Text.Megaparsec.Time.Byte' directly.
module Text.Megaparsec.Time (
  -- * Types
  DayResult,

  -- * Simple parsers
  Char.hoursParser,
  Char.minutesParser,
  Char.secondsParser,
  Char.timeParser,
  Char.dayParser,
  Char.gregorianDayParser,

  -- * Composite parsers
  Char.dateParser,
  Char.durationParser,
) where

import qualified Text.Megaparsec.Time.Char as Char
import Text.Megaparsec.Time.Common (DayResult)
