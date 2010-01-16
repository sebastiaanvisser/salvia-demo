{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.CleverCSS
( hFilterCSS
, hCleverCSS
, hParametrizedCleverCSS
)
where

import Control.Applicative
import Control.Monad.State
import Data.Record.Label hiding (get)
import Network.Protocol.Uri
import Network.Protocol.Http
import Network.Salvia
import Text.CSS.CleverCSS 

hFilterCSS :: (MonadIO m, HttpM' m, SendM m, Alternative m) => m () -> m () -> m ()
hFilterCSS css = hExtension (Just "css") (hFile <|> css)

hCleverCSS :: (MonadIO m, BodyM Request m, HttpM' m, SendM m) => m ()
hCleverCSS = hRequestParameters "utf-8" >>= hParametrizedCleverCSS

hParametrizedCleverCSS :: (MonadIO m, HttpM' m, SendM m) => Parameters -> m ()
hParametrizedCleverCSS p =
  do hRewriteExt (fmap ('c':)) (hFileFilter convert)
     response (contentType =: Just ("text/css", Just "utf-8"))
  where convert = either id id . flip (cleverCSSConvert "") (map (fmap $ maybe "" id) p)

