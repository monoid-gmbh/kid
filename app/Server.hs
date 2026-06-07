{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}

module Server (
    start
  ) where

import           Control.Exception          (SomeException, displayException, try)
import           Control.Monad.Except       (ExceptT (..), runExceptT)
import           Data.ByteString.Lazy.Char8 as BL (ByteString (..), pack)
import           Data.Maybe                 (fromMaybe)
import           KID.Calculation            (CalculationCtx (..), calculateRisk, ptr_futhark_new)
import           KID.Data                   (DataSource)
import           KID.Document               (Language (..), generateDocument)
import           KID.Domain                 (Contract, RiskSummary)
import           MimeTypes
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO                  (hPutStrLn, stderr)

-- | KID server API
type API =
       "kid"  :> QueryParam "lang" Language :> ReqBody '[JSON] Contract               :> Post '[PDF] BL.ByteString
  :<|> "calc"                               :> ReqBody '[JSON] Contract               :> Post '[JSON] RiskSummary
  :<|> "doc"  :> QueryParam "lang" Language :> ReqBody '[JSON] (Contract,RiskSummary) :> Post '[PDF] BL.ByteString

-- | KID server implementation, default language is DE
server :: CalculationCtx -> Server API
server x = flip kid :<|> calc :<|> flip doc
  where

    kid :: Contract -> Maybe Language -> Handler BL.ByteString
    kid c = Handler . go . calc_and_doc c . fromMaybe EN

    calc_and_doc :: Contract -> Language -> ExceptT String IO BL.ByteString
    calc_and_doc c l = calculateRisk x c >>= generateDocument l . (c,)

    calc :: Contract -> Handler RiskSummary
    calc = Handler . go . calculateRisk x

    doc :: (Contract, RiskSummary) -> Maybe Language -> Handler BL.ByteString
    doc c l = let lang = fromMaybe EN l in Handler . go $ generateDocument lang c

    -- Turn any failure into a 400 response and log it to stderr so it is visible
    -- on the server console (not only in the response body, which clients may
    -- save straight to a file). Both expected errors (the @ExceptT String@
    -- channel) and unexpected IO exceptions (e.g. a missing @pdflatex@) are
    -- reported rather than collapsing into an opaque 500.
    go :: forall a. ExceptT String IO a -> ExceptT ServerError IO a
    go m = ExceptT $ do
      outcome <- try (runExceptT m)
      case outcome of
        Right (Right a)  -> pure (Right a)
        Right (Left err) -> report err
        Left (ex :: SomeException) -> report ("unexpected error: " ++ displayException ex)
      where
        report :: String -> IO (Either ServerError a)
        report err = do
          hPutStrLn stderr ("kid: request failed: " ++ err)
          pure (Left (handleError err))

-- | Return http 400 in case of an error
handleError :: String -> ServerError
handleError s = err400
  { errBody    = BL.pack s
  , errHeaders = [("Content-Type", "text/plain; charset=utf-8")]
  }

-- | Servant API
api :: Proxy API
api = Proxy

-- | Servant Application
app :: CalculationCtx -> Application
app x = serve api $ server x

-- | Server listening on port 8081
start :: DataSource -> IO ()
start src = ptr_futhark_new >>= run 8081 . app . CalculationCtx src
