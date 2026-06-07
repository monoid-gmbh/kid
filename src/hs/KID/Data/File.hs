{-| File 'DataSource': reads historical data from local CSV files.

    Handles 'FileId' underlyings; any other 'InstrumentId' variant is an error.

    Each 'FileId' points to a CSV file with a header row whose first column is an
    ISO date (@YYYY-MM-DD@) and whose remaining columns are named value columns,
    e.g.

    > Date,Price,Volume
    > 2018-11-23,98.20,1200
    > 2018-11-26,99.05,1500

    'file_column' selects which value column to read (@\"Price\"@ above). Rows
    outside the requested 'DateRange' are dropped.
-}

module KID.Data.File
  ( fileSource
  ) where

import           Control.Exception    (IOException, try)
import           Control.Monad.Except (ExceptT (..), liftEither, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.List            (elemIndex)
import           Data.Time.Calendar   (Day)
import           Data.Time.Format     (defaultTimeLocale, parseTimeM)
import           KID.Data             (DataSource (..), DateRange (..), TimeSeries)
import           KID.Domain           (InstrumentId (..), Underlying (..))

-- | CSV-file-backed data source. Self-contained: each underlying names its own
-- file and column via its 'FileId', so no global configuration is needed.
fileSource :: DataSource
fileSource = DataSource fetch
  where
    fetch :: DateRange -> [Underlying] -> ExceptT String IO [TimeSeries]
    fetch range = traverse (readUnderlying range . underlying_instrument_id)

    readUnderlying :: DateRange -> InstrumentId -> ExceptT String IO TimeSeries
    readUnderlying range (FileId path column) = do
      result <- liftIO (try (readFile path) :: IO (Either IOException String))
      content <- either (throwError . show) pure result
      liftEither (parseCsv range path column content)
    readUnderlying _ iid =
      throwError ("file backend: unsupported instrument id: " ++ show iid)

-- | Parse a CSV, selecting the named column and keeping rows within the range.
parseCsv :: DateRange -> FilePath -> String -> String -> Either String TimeSeries
parseCsv (DateRange f t) path column content =
  case lines content of
    []         -> Left (path ++ ": empty file")
    (hdr:rows) -> do
      let header = splitOn ',' hdr
      col <- maybe (Left (path ++ ": no column " ++ show column)) Right
               (elemIndex column header)
      obs <- traverse (parseRow path col) (filter (not . null) rows)
      Right (filter (inRange . fst) obs)
  where
    inRange d = d >= f && d <= t

-- | Parse one data row: ISO date in column 0, value in the selected column.
parseRow :: FilePath -> Int -> String -> Either String (Day, Double)
parseRow path col row = do
  let cells = map trim (splitOn ',' row)
  dayCell <- note (path ++ ": missing date in row: " ++ row) (atMay cells 0)
  day     <- parseDay path dayCell
  valCell <- note (path ++ ": missing value column in row: " ++ row) (atMay cells col)
  val     <- parseDouble path valCell
  Right (day, val)

parseDay :: FilePath -> String -> Either String Day
parseDay path s =
  note (path ++ ": invalid date " ++ show s)
       (parseTimeM True defaultTimeLocale "%Y-%m-%d" s)

parseDouble :: FilePath -> String -> Either String Double
parseDouble path s = case reads s of
  [(x, "")] -> Right x
  _         -> Left (path ++ ": invalid number " ++ show s)

-- Small helpers (kept local to avoid extra dependencies)

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (chunk, [])        -> [chunk]
  (chunk, _ : rest)  -> chunk : splitOn c rest

trim :: String -> String
trim = f . f where f = reverse . dropWhile (`elem` " \t\r")

atMay :: [a] -> Int -> Maybe a
atMay xs i
  | i < 0     = Nothing
  | otherwise = case drop i xs of
      (x : _) -> Just x
      []      -> Nothing

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right
