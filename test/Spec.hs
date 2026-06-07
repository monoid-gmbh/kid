{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for kid.
--
-- Focuses on the deterministic, side-effect-light logic:
--
--   * the CSV file data backend ("KID.Data.File"),
--   * the generic array assembly ('KID.Data.buildArray'),
--   * the summary risk indicator lookup ('summary_risk_indicator'),
--   * JSON decoding of the bundled example contracts.
--
-- The Quandl backend (network) and document generation (pdflatex) are out of
-- scope here.
module Main (main) where

import           Control.Monad.Except (runExceptT)
import qualified Data.Array.Storable  as S
import           Data.Aeson           (decodeFileStrict, decode, encode)
import           Data.List            (isInfixOf)
import           Data.Maybe           (isJust)
import           Data.Time.Calendar   (Day, fromGregorian)
import           System.FilePath      ((</>))
import           System.IO.Temp       (withSystemTempDirectory)
import           Test.Hspec

import           KID.Calculation      (summary_risk_indicator)
import           KID.Data             (DataSource (..), DateRange (..), TimeSeries, buildArray)
import           KID.Data.File        (fileSource)
import           KID.Domain

main :: IO ()
main = hspec $ do
  fileBackendSpec
  buildArraySpec
  riskIndicatorSpec
  jsonSpec

--------------------------------------------------------------------------------
-- File backend
--------------------------------------------------------------------------------

fileBackendSpec :: Spec
fileBackendSpec = describe "KID.Data.File.fileSource" $ do

  let sampleCsv = unlines
        [ "Date,Price,Volume"
        , "2018-11-21,100.0,1200"
        , "2018-11-22,100.5,1500"
        , "2018-11-23,101.25,1700"
        ]

  it "reads the selected value column over the full range" $
    withCsv sampleCsv $ \path -> do
      result <- fetchOne (wideRange) (FileId path "Price")
      result `shouldBe` Right
        [ (day 21, 100.0), (day 22, 100.5), (day 23, 101.25) ]

  it "selects a different column when asked" $
    withCsv sampleCsv $ \path -> do
      result <- fetchOne wideRange (FileId path "Volume")
      result `shouldBe` Right
        [ (day 21, 1200), (day 22, 1500), (day 23, 1700) ]

  it "drops rows outside the requested date range" $
    withCsv sampleCsv $ \path -> do
      result <- fetchOne (DateRange (day 22) (day 23)) (FileId path "Price")
      result `shouldBe` Right [ (day 22, 100.5), (day 23, 101.25) ]

  it "trims surrounding whitespace in cells" $
    withCsv "Date,Price\n  2018-11-21 , 100.0 \n" $ \path -> do
      result <- fetchOne wideRange (FileId path "Price")
      result `shouldBe` Right [ (day 21, 100.0) ]

  it "fails with a clear message when the column is missing" $
    withCsv sampleCsv $ \path -> do
      result <- fetchOne wideRange (FileId path "Close")
      result `shouldSatisfy` leftContaining "no column"

  it "fails when the file does not exist" $ do
    result <- fetchOne wideRange (FileId "/no/such/file.csv" "Price")
    result `shouldSatisfy` isLeft

  it "rejects instrument ids it does not understand" $ do
    result <- fetchOne wideRange (QuandlId "SIX" "ABC" "Price")
    result `shouldSatisfy` leftContaining "unsupported instrument id"

--------------------------------------------------------------------------------
-- Array assembly
--------------------------------------------------------------------------------

buildArraySpec :: Spec
buildArraySpec = describe "KID.Data.buildArray" $ do

  let d1 = fromGregorian 2020 1 1
      d2 = fromGregorian 2020 1 2

  it "spans (1..n, minDay..maxDay) and fills observations" $ do
    arr <- buildArray [ [(d1, 1.0), (d2, 2.0)], [(d1, 3.0)] ]
    bounds <- S.getBounds arr
    bounds `shouldBe` ((1, d1), (2, d2))
    v11 <- S.readArray arr (1, d1)
    v21 <- S.readArray arr (2, d1)
    v12 <- S.readArray arr (1, d2)
    v11 `shouldBe` 1.0
    v21 `shouldBe` 3.0
    v12 `shouldBe` 2.0

  it "leaves missing observations as NaN" $ do
    arr <- buildArray [ [(d1, 1.0), (d2, 2.0)], [(d1, 3.0)] ]
    gap <- S.readArray arr (2, d2)   -- underlying 2 has no value on d2
    gap `shouldSatisfy` isNaN

--------------------------------------------------------------------------------
-- Summary risk indicator (Annex I, 52)
--------------------------------------------------------------------------------

riskIndicatorSpec :: Spec
riskIndicatorSpec = describe "KID.Calculation.summary_risk_indicator" $ do
  it "equals the MRM for the lowest credit risk classes" $ do
    summary_risk_indicator 1 1 `shouldBe` 1
    summary_risk_indicator 2 4 `shouldBe` 4
  it "raises the indicator for higher credit risk classes" $ do
    summary_risk_indicator 3 1 `shouldBe` 3
    summary_risk_indicator 4 1 `shouldBe` 5
    summary_risk_indicator 6 1 `shouldBe` 6
  it "caps at 7" $ do
    summary_risk_indicator 6 7 `shouldBe` 7
    summary_risk_indicator 4 7 `shouldBe` 7

--------------------------------------------------------------------------------
-- JSON decoding of the bundled examples
--------------------------------------------------------------------------------

jsonSpec :: Spec
jsonSpec = describe "KID.Domain JSON" $ do

  it "decodes the bundled Quandl example contract" $ do
    c <- decodeFileStrict "test/example/contract.json" :: IO (Maybe Contract)
    isJust c `shouldBe` True

  it "decodes the bundled file-backend example contract" $ do
    c <- decodeFileStrict "test/example/contract-file.json" :: IO (Maybe Contract)
    isJust c `shouldBe` True

  it "round-trips instrument ids through JSON" $ do
    roundTrips (FileId "x.csv" "Price")        `shouldBe` True
    roundTrips (QuandlId "SIX" "ABC123" "Last") `shouldBe` True
    roundTrips DummyData                        `shouldBe` True

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Run the file backend for a single underlying with the given instrument id.
-- The backend returns one series per underlying; with a single underlying
-- 'concat' flattens that to just its series (and is total, unlike 'head').
fetchOne :: DateRange -> InstrumentId -> IO (Either String TimeSeries)
fetchOne range iid =
  fmap (fmap concat) . runExceptT $
    fetchSeries fileSource range [Underlying "u" iid 1.0]

-- | A date range wide enough to include every test observation.
wideRange :: DateRange
wideRange = DateRange (fromGregorian 2000 1 1) (fromGregorian 2100 1 1)

-- | A day in November 2018.
day :: Int -> Day
day = fromGregorian 2018 11

-- | Create a temporary CSV file with the given contents.
withCsv :: String -> (FilePath -> IO a) -> IO a
withCsv contents act =
  withSystemTempDirectory "kid-test" $ \dir -> do
    let path = dir </> "data.csv"
    writeFile path contents
    act path

-- | Encode/decode round-trip, compared at the JSON-bytes level (the domain
-- types do not derive 'Eq').
roundTrips :: InstrumentId -> Bool
roundTrips x = (encode <$> (decode (encode x) :: Maybe InstrumentId)) == Just (encode x)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

leftContaining :: String -> Either String b -> Bool
leftContaining needle (Left msg) = needle `isInfixOf` msg
leftContaining _      _          = False
