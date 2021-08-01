{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module KID.Domain(
 -- * Contract data types
   ProductType(..)
 , Issuer(..)
 , InstrumentId(..)
 , Costs(..)
 , Contract(..)
 , Currency(..)
 , Underlying(..)
 -- * Risk data types
 , Scenario(..)
 , RiskSummary(..)
 ) where

import           Data.Aeson.TH
import           Data.Time.Calendar

-- === Contract data types

data Currency =
    CHF
  | EUR
  | USD
  deriving (Read, Show, Enum, Bounded)

data ProductType =
    SampleContract1
  | SampleContract2 {
      barrier      :: Double
    , strike       :: Double
    , denomination :: Double
  }
  deriving (Eq, Read, Show)

data Costs = Costs {
    standard_investment_amount :: Double
  -- one off costs
  , entry                      :: Double
  , exit                       :: Double
  -- ongoing costs
  -- incidental costs
  }

data Underlying = Underlying {
    underlying_name          :: String
  , underlying_instrument_id :: InstrumentId
  , weight                   :: Double
  } deriving (Show)

data InstrumentId =
  QuandlId {
    database_code :: String
  , dataset_code  :: String
  , dataset_field :: String
  } |
  DummyData
  deriving (Show)

data Issuer = Issuer {
    issuer_name :: String
  , issuer_web  :: String
  }

data Contract = Contract {
    product_name     :: String
  , product_currency :: Currency
  , product_type     :: ProductType
  , instrument_id    :: [InstrumentId]
  , underlyings      :: [Underlying]
  , issuer           :: Issuer
  , issue_date       :: Day
  , redemption_date  :: Day
  , costs            :: Costs
  }

-- === Risk data types

data Scenario = Scenario {
    favourable   :: Double
  , moderate     :: Double
  , unfavourable :: Double
  , stress       :: Double
} deriving (Show)

data RiskSummary = RiskSummary {
    var               :: Double
  , vev               :: Double
  , mrm               :: Int
  , sri               :: Int
  , rhp               :: Double
  , scenario_rhp      :: Scenario
  , scenario_rhp_half :: Maybe Scenario
  , scenario_one_year :: Maybe Scenario
} deriving (Show)

-- === JSON instances

deriveJSON defaultOptions ''InstrumentId
deriveJSON defaultOptions ''Underlying
deriveJSON defaultOptions ''Costs
deriveJSON defaultOptions ''ProductType
deriveJSON defaultOptions ''Currency
deriveJSON defaultOptions ''Issuer
deriveJSON defaultOptions ''Contract
deriveJSON defaultOptions ''Scenario
deriveJSON defaultOptions ''RiskSummary
