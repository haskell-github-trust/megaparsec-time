{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Text.Megaparsec.Time.Common
-- Description : Stream-agnostic time parser builders.
-- Copyright   : (c) drlkf, 2024
-- License     : GPL-3
-- Maintainer  : drlkf@drlkf.net
-- Stability   : experimental
--
-- Builders shared by 'Text.Megaparsec.Time.Char' and 'Text.Megaparsec.Time.Byte'.
module Text.Megaparsec.Time.Common (
  -- * Types
  DayResult,

  -- * Shared parser builders
  mkDateParser,
  mkDayParser,
  mkDurationParser,
  mkGregorianDayParser,
  mkTimeParser,
) where

import Control.Applicative (many, optional, (<|>))
import Control.Monad (replicateM)
import Control.Monad.Combinators (choice, some)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Time (
  Day,
  DayOfWeek (..),
  NominalDiffTime,
  TimeOfDay (..),
  defaultTimeLocale,
  makeTimeOfDayValid,
  parseTimeM,
 )
import Text.Megaparsec (MonadParsec, Token, anySingle, try)
import Text.Printf (printf)

-- | Representation of a parser result with either a number of days relative to
-- the current day, or a 'DayOfWeek'.
--
-- @since 0.3.0.0
type DayResult = Either Int DayOfWeek

-- | Build a parser for a tuple containing a day or not, and a 'TimeOfDay',
-- separated with a space.
--
-- @since 0.3.0.0
mkDateParser
  :: MonadParsec e s m
  => m ()
  -- ^ Non-empty space parser.
  -> m DayResult
  -- ^ Day parser.
  -> m TimeOfDay
  -- ^ Time parser.
  -> m (Maybe DayResult, TimeOfDay)
mkDateParser space1 day time = (,) <$> optional (try (day <* space1)) <*> time

-- | Build a case-insensitive day parser, see 'Text.Megaparsec.Time.Char.dayParser'.
--
-- @since 0.3.0.0
mkDayParser
  :: MonadParsec e s m
  => (Token s -> m (Token s))
  -- ^ Case-insensitive single-token parser.
  -> (Char -> Token s)
  -- ^ Convert a literal 'Char' to a token.
  -> m (Token s)
  -- ^ Digit parser.
  -> (Token s -> Char)
  -- ^ Convert a token to a 'Char'.
  -> m DayResult
mkDayParser char' fromChar digitChar toChar =
  choice
    [ Right <$> longDay
    , Right <$> shortDay
    , Left <$> (str "yesterday" $> -1)
    , Left <$> (str "tomorrow" $> 1)
    , Right <$> absoluteDay
    , Left <$> relativeDay
    ]
 where
  str = try . traverse_ (char' . fromChar)
  shortDay = choice $ map (dayString (take 3 . show)) weekDays
  longDay = choice $ map (dayString show) weekDays
  dayString f d = str (f d) $> d
  weekDays = [Monday .. Friday]
  sign = (str "-" $> negate) <|> (str "+" $> id)
  digits = map toChar <$> some digitChar
  absoluteDay = toEnum . read <$> digits
  relativeDay = ($) <$> sign <*> (read <$> digits)

-- | Build a 'NominalDiffTime' parser from hours, minutes and seconds parsers,
-- with all components being optional as long as one is present.
--
-- @since 0.3.0.0
mkDurationParser
  :: MonadParsec e s m
  => m ()
  -- ^ Space parser.
  -> m NominalDiffTime
  -- ^ Hours parser.
  -> m NominalDiffTime
  -- ^ Minutes parser.
  -> m NominalDiffTime
  -- ^ Seconds parser.
  -> m NominalDiffTime
mkDurationParser space hoursParser minutesParser secondsParser =
  try hours <|> try minutes <|> secondsParser
 where
  hours = do
    h <- hoursParser <* space
    m <- fromMaybe 0 <$> optional (try minutes)
    s <- fromMaybe 0 <$> optional secondsParser

    return (h + m + s)

  minutes = do
    m <- minutesParser <* space
    s <- fromMaybe 0 <$> optional secondsParser

    return (m + s)

-- | Build a Gregorian 'Day' parser from a @%F@ or @%d\/%m\/%Y@ format.
--
-- @since 0.3.0.0
mkGregorianDayParser
  :: MonadFail m
  => MonadParsec e s m
  => (Token s -> Char)
  -- ^ Convert a token to a 'Char'.
  -> m Day
mkGregorianDayParser toChar = do
  s <- map toChar <$> many anySingle

  let parseTime = flip (parseTimeM False defaultTimeLocale) s

  parseTime "%F" <|> parseTime "%d/%m/%Y"

-- | Build a 'TimeOfDay' parser from a string like @01:23@.
--
-- @since 0.3.0.0
mkTimeParser
  :: MonadFail m
  => m t
  -- ^ Digit parser.
  -> m t
  -- ^ Colon parser.
  -> (t -> Char)
  -- ^ Convert a token to a 'Char'.
  -> m TimeOfDay
mkTimeParser digitChar colon toChar = do
  h <- read . map toChar <$> replicateM 2 digitChar <* colon
  m <- read . map toChar <$> replicateM 2 digitChar

  maybe
    (fail (printf "invalid hours or minutes %02d:%02d" h m))
    pure
    (makeTimeOfDayValid h m 0)
