{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MimeTypes where

import           Control.Arrow              (left)
import qualified Data.ByteString.Lazy.Char8 as BS (fromStrict, pack, ByteString(..))
import qualified Data.Text                  as T (pack, unpack)
import           Data.Text.Encoding         (encodeUtf8)
import           KID.Document               (Language)
import           Network.HTTP.Media         ((//), (/:))
import           Servant
import           System.IO.Unsafe           (unsafePerformIO)
import           Text.Read                  (readEither)

data PDF

instance Accept PDF where
  contentType _ = "application" // "pdf" /: ("filename", "document.pdf") -- TODO: not hardcoded, use: addHeader

instance MimeRender PDF BS.ByteString where
  mimeRender _ doc = doc -- unsafePerformIO $ run >>= handleError
    --where
    --  run = runIO $ either id id <$> to_latex doc

-- | Language is exposed
instance FromHttpApiData Language where
  parseUrlPiece t = left T.pack $ readEither $ T.unpack t
