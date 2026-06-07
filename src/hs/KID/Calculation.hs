{-| Risk calculations/simulations

    * VaR
    * VEV (VaR Equivalent Volatility)
    * Performance scenarios
    * Performance scenarios for intermediate holding periods

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module KID.Calculation (
    CalculationCtx(..)
  , calculateRisk
  , ptr_futhark_new
  , summary_risk_indicator
  ) where

import           Control.Monad.Except
import           Control.Monad.IO.Class    (liftIO)
import           Data.Maybe
import           Data.Time.Calendar
import           KID.Calculation.Futhark   (futhark_contract, ptr_futhark_new, Ptr_Futhark(..))
import           KID.Data                  (DataSource, DateRange(..), loadArray)
import           KID.Domain
import           Numeric.AD                (diff)
import           Numeric.Natural           (Natural)

-- | Calculation Context
data CalculationCtx = CalculationCtx {
    data_source :: DataSource   -- ^ pluggable historical-data backend
  , ptr_futhark :: Ptr_Futhark
}

-- | Risk calculations (Annex I, 3)
calculateRisk :: CalculationCtx
               -> Contract
               -> ExceptT String IO RiskSummary

calculateRisk CalculationCtx{..} c@Contract{..} =
         let t = diffDays redemption_date issue_date   -- recommended holding period in days
             d = addGregorianYearsClip (-5) issue_date -- 5 years of historical data (Annex I, 9)
          in measure t <$> (loadArray data_source (DateRange d issue_date) underlyings
             >>= liftIO . futhark_contract product_type ptr_futhark t c)

-- | measure
measure :: Integer
        -> (Double, Double, Int, Scenario, Maybe Scenario, Maybe Scenario)
        -> RiskSummary
measure t (var, vev, mrm, sce, rph_half, one_year) =
  let crm = 1 -- TODO
      rhp = fromIntegral t / 365
      sri = summary_risk_indicator crm mrm
   in RiskSummary var vev mrm sri rhp sce rph_half one_year

-- | Summary Risk Indicator (Annex I, 52)
summary_risk_indicator :: Int -> Int -> Int
summary_risk_indicator 1 1 = 1
summary_risk_indicator 1 2 = 2
summary_risk_indicator 1 3 = 3
summary_risk_indicator 1 4 = 4
summary_risk_indicator 1 5 = 5
summary_risk_indicator 1 6 = 6
summary_risk_indicator 1 7 = 7

summary_risk_indicator 2 1 = 1
summary_risk_indicator 2 2 = 2
summary_risk_indicator 2 3 = 3
summary_risk_indicator 2 4 = 4
summary_risk_indicator 2 5 = 5
summary_risk_indicator 2 6 = 6
summary_risk_indicator 2 7 = 7

summary_risk_indicator 3 1 = 3
summary_risk_indicator 3 2 = 3
summary_risk_indicator 3 3 = 3
summary_risk_indicator 3 4 = 4
summary_risk_indicator 3 5 = 5
summary_risk_indicator 3 6 = 6
summary_risk_indicator 3 7 = 7

summary_risk_indicator 4 1 = 5
summary_risk_indicator 4 2 = 5
summary_risk_indicator 4 3 = 5
summary_risk_indicator 4 4 = 5
summary_risk_indicator 4 5 = 5
summary_risk_indicator 4 6 = 6
summary_risk_indicator 4 7 = 7

summary_risk_indicator 5 1 = 5
summary_risk_indicator 5 2 = 5
summary_risk_indicator 5 3 = 5
summary_risk_indicator 5 4 = 5
summary_risk_indicator 5 5 = 5
summary_risk_indicator 5 6 = 6
summary_risk_indicator 5 7 = 7

summary_risk_indicator 6 1 = 6
summary_risk_indicator 6 2 = 6
summary_risk_indicator 6 3 = 6
summary_risk_indicator 6 4 = 6
summary_risk_indicator 6 5 = 6
summary_risk_indicator 6 6 = 6
summary_risk_indicator 6 7 = 7

summary_risk_indicator _ _ = 7

-- Reduction in yield (RIY)

-- | https://stackoverflow.com/questions/45498859/how-to-properly-match-types-when-using-numeric-ad-in-haskell
newtonRoot
  :: (Show a,Floating a, Ord a)
  => Natural  -- ^ iterations
  -> a        -- ^ epsilon
  -> a        -- ^ starting guess
  -> (forall b. Floating b => b -> b)
  -> Maybe a
newtonRoot i ep x f
   | i == 0 = Nothing
   | abs (f x) - abs ep < 0 = Just x
   | otherwise = newtonRoot (i - 1) ep (x - f x / diff f x) f

-- | Internal Rate of Return (irr)
irr :: (Show a, Ord a, Floating a) => [(Double, Double)] -> Maybe a
irr xs = newtonRoot 1000 0.0001 0.5 p
  where
    p :: (forall b. Floating b => b -> b)
    p r = sum $ map (($ r) . f) xs

    -- TODO: isn't this somehow possible with RankNTypes...?
    f :: (Floating a, Real b) => (b, b) -> a -> a
    f (x,y) r = v / (1+r)**t
      where
        t = realToFrac x
        v = realToFrac y

-- | Reduction in yield, RIY (Annex VI, 70)
reduction_in_yield :: Costs
                   -> Double
                   -> Double
                   -> Double
reduction_in_yield Costs{..} t m = let
  i = irr [(0, -standard_investment_amount+entry), (t, standard_investment_amount*m)]
  r = irr [(0, -standard_investment_amount)      , (t, standard_investment_amount*m)]
  in fromMaybe 0.0 $ (-) <$> i <*> r
