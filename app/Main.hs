{-# LANGUAGE DeriveDataTypeable #-}

module Main
  where

import           Data.Char            (toLower)
import           KID.Data             (DataSource)
import           KID.Data.File        (fileSource)
import           KID.Data.Quandl      (quandlSource)
import           Server
import           System.Console.CmdArgs

-- | Command line args:
--
--   * @--backend quandl|file@ selects the historical-data backend (default: quandl).
--   * @--key@ provides the Quandl API key (only used by the quandl backend).
data KidApp = KidApp
  { key     :: Maybe String
  , backend :: String
  } deriving (Show, Data, Typeable)

kidApp :: KidApp
kidApp = KidApp
  { key     = def &= help "Quandl API key (quandl backend only)"
  , backend = "quandl" &= typ "quandl|file" &= help "Historical-data backend"
  }

-- | Build the selected data backend.
dataSource :: KidApp -> DataSource
dataSource a = case map toLower (backend a) of
  "file" -> fileSource
  _      -> quandlSource (key a)

-- | Start the web-server with command line args
main :: IO ()
main = cmdArgs kidApp >>= start . dataSource
