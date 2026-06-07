{-| Quandl 'DataSource': fetches historical data from the Quandl API.

    Handles 'QuandlId' underlyings; any other 'InstrumentId' variant is an error.
-}

module KID.Data.Quandl
  ( quandlSource
  ) where

import           Control.Monad.Except (ExceptT (..), liftEither, withExceptT)
import           Data.Quandl          (Parameter (..), QuandlResponse, quandlLoads, select_field)
import           KID.Data             (DataSource (..), DateRange (..), TimeSeries)
import           KID.Domain           (InstrumentId (..), Underlying (..))

-- | Quandl-backed data source. The optional API key is used for all requests.
quandlSource :: Maybe String -> DataSource
quandlSource key = DataSource fetch
  where
    fetch :: DateRange -> [Underlying] -> ExceptT String IO [TimeSeries]
    fetch (DateRange f t) us = do
      -- Map each underlying to a request + the field to read from the result.
      reqs <- liftEither (traverse (toParameter . underlying_instrument_id) us)
      let (params, fields) = unzip reqs
      responses <- withExceptT show (ExceptT (quandlLoads params))
      pure (zipWith select_field fields responses)
      where
        toParameter :: InstrumentId -> Either String (Parameter, String)
        toParameter (QuandlId db ds field) =
          Right
            ( Parameter db ds Nothing Nothing (Just f) (Just t) Nothing Nothing Nothing key
            , field )
        toParameter iid =
          Left ("quandl backend: unsupported instrument id: " ++ show iid)
