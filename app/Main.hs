{-# LANGUAGE DeriveDataTypeable #-}

module Main
  where

import           Server
import           System.Console.CmdArgs

-- | Command line args:
-- The quandl key can be provided
newtype KidApp = KidApp {key :: Maybe String}
  deriving (Show, Data, Typeable)

-- | Start the web-server with command line args
main :: IO ()
main = cmdArgs a >>= start . key
  where a = KidApp {key = def}
