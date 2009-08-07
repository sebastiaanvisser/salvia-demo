module Network.Salvia.Handler.CleverCSS (
    hFilterCSS
  , hCleverCSS
  , hParametrizedCleverCSS
  ) where

import Control.Monad.Trans
import Network.Protocol.Uri
import Network.Salvia.Handlers
import Network.Salvia.Httpd
import Text.CSS.CleverCSS 

hFilterCSS
  :: (MonadIO m, Request m, Response m, Send m, Socket m)
  => m () -> m () -> m ()
hFilterCSS cssfilter handler = do
  hExtension (Just "css")
    (hFile `hOr` cssfilter)
    handler

hCleverCSS :: (Request m, MonadIO m, Response m, Send m) => m ()
hCleverCSS = hParameters >>= hParametrizedCleverCSS

hParametrizedCleverCSS
  :: (MonadIO m, Request m, Response m, Send m)
  => Parameters -> m ()
hParametrizedCleverCSS p = do
  hRewriteExt (fmap ('c':)) (hFileFilter convert)
  where convert = either id id . flip (cleverCSSConvert "") (map (fmap $ maybe "" id) p)

