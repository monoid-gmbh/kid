import           Data.Time.Calendar
import Data.Aeson

import KID.Domain.Contract
import KID.Domain.Risk

import KID.Document
import Text.LaTeX

main :: IO ()
main = execLaTeXT (kid DE (sampleContract,sampleRisk)) >>= renderFile "kid.tex"

sampleIssuer :: Issuer
sampleIssuer = Issuer "testname" "http://testname.bank"

sampleContract :: Contract
sampleContract = Contract "testname" CHF SampleContract1 [] [sampleUnderlying1, sampleUnderlying2] sampleIssuer d d sampleCosts
  where d = fromGregorian 2020 5 11

sampleCosts :: Costs
sampleCosts = Costs 0.0 0.0 0.0

sampleUnderlying1 :: Underlying
sampleUnderlying1 = Underlying "test1" instrumentId 0.5
  where instrumentId = DummyData

sampleUnderlying2 :: Underlying
sampleUnderlying2 = Underlying "test2" instrumentId 0.5
  where instrumentId = DummyData

sampleRisk :: RiskSummary
sampleRisk = RiskSummary 0.0 0.0 0 7 0.0 [sampleScenario, sampleScenario]

sampleScenario :: Scenarios
sampleScenario = Scenarios OneYear 0.0 0.0 0.0 0.0
