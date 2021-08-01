{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}

module Server (
    start
  ) where

import           Control.Monad.Except       (ExceptT, liftIO, withExceptT)
import           Data.ByteString.Lazy.Char8 as BL (ByteString (..), pack)
import           Data.Maybe                 (fromMaybe)
import           KID.Calculation            (CalculationCtx (..), calculateRisk, ptr_futhark_new)
import           KID.Document               (Language (..), generateDocument)
import           KID.Domain                 (Contract, RiskSummary)
import           MimeTypes
import           Network.Wai.Handler.Warp
import           Servant

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
    calc_and_doc c l = calculateRisk x c >>= liftIO . generateDocument l . (c,)

    calc :: Contract -> Handler RiskSummary
    calc = Handler . go . calculateRisk x

    doc :: (Contract, RiskSummary) -> Maybe Language -> Handler BL.ByteString
    doc c l = let lang = fromMaybe EN l in Handler . go . liftIO $ generateDocument lang c

    go :: ExceptT String IO a -> ExceptT ServerError IO a
    go = withExceptT handleError

-- | Return http 400 in case of an error
handleError :: String -> ServerError
handleError s = err400 { errBody = BL.pack s }

-- | Servant API
api :: Proxy API
api = Proxy

-- | Servant Application
app :: CalculationCtx -> Application
app x = serve api $ server x

-- | Server listening on port 8081
start :: Maybe String -> IO ()
start k = ptr_futhark_new >>= run 8081 . app . CalculationCtx k
