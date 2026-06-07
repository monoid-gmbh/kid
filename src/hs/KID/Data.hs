{-| Generic historical-data backend.

    Risk calculations need, for each underlying, a series of historical
    observations over a date range. Where that data comes from (Quandl, a local
    file, ...) is abstracted behind 'DataSource': a backend is just a function
    that turns underlyings + a date range into one 'TimeSeries' per underlying.

    Concrete backends:

      * "KID.Data.Quandl" — fetches from the Quandl API.
      * "KID.Data.File"   — reads from local CSV files.

    The assembly of those series into the dense @(underlying, day)@ array that
    the Futhark engine consumes ('buildArray' / 'loadArray') is backend-agnostic
    and lives here.
-}

{-# LANGUAGE FlexibleContexts #-}

module KID.Data
  ( DateRange(..)
  , TimeSeries
  , DataSource(..)
  , loadArray
  , buildArray
  ) where

import           Control.Monad.Except   (ExceptT, throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Array.Storable   as S
import           Data.Time.Calendar    (Day)
import           KID.Domain            (Underlying)

-- | A closed historical window @[from, to]@.
data DateRange = DateRange
  { from :: Day
  , to   :: Day
  }

-- | One underlying's historical observations. Gaps (e.g. weekends, holidays)
-- are simply absent and become NaN in the assembled array.
type TimeSeries = [(Day, Double)]

-- | A pluggable historical-data backend (the "handle" pattern).
--
-- Given a date range and the contract's underlyings, a backend produces one
-- 'TimeSeries' per underlying, in the same order. Each backend interprets the
-- 'KID.Domain.InstrumentId' variants it understands and fails (in @ExceptT@)
-- on the ones it does not.
newtype DataSource = DataSource
  { fetchSeries :: DateRange -> [Underlying] -> ExceptT String IO [TimeSeries] }

-- | Fetch the historical series via the given backend and assemble them into
-- the dense @(underlying, day)@ array consumed by the calculation engine.
loadArray :: DataSource
          -> DateRange
          -> [Underlying]
          -> ExceptT String IO (S.StorableArray (Int, Day) Double)
loadArray src range us = do
  series <- fetchSeries src range us
  if all null series
    then throwError "no historical data returned for any underlying"
    else liftIO (buildArray series)

-- | Assemble per-underlying time series into a 2-D array indexed by
-- @(underlying index 1..n, day)@. Missing observations are left as NaN.
buildArray :: [TimeSeries] -> IO (S.StorableArray (Int, Day) Double)
buildArray series = do
  let days = concatMap (map fst) series
  arr <- S.newArray ((1, minimum days), (length series, maximum days)) nan
  mapM_ (\(i, obs) -> mapM_ (\(d, v) -> S.writeArray arr (i, d) v) obs)
        (zip [1 ..] series)
  return arr
  where
    nan :: Double
    nan = 0 / 0
