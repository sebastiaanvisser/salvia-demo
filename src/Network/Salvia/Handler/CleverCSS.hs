module Network.Salvia.Handler.CleverCSS (
    hFilterCSS
  , hCleverCSS
  , hParametrizedCleverCSS
  ) where

import Control.Applicative
import Control.Monad.Trans
import Network.Protocol.Uri
import Network.Salvia.Handlers
import Network.Salvia.Httpd
import Text.CSS.CleverCSS 

hFilterCSS
  :: (MonadIO m, Request m, Response m, Send m, Alternative m)
  => m () -> m () -> m ()
hFilterCSS css = hExtension (Just "css") (hFile <|> css)

hCleverCSS :: (MonadIO m, Request m, Response m, Send m) => m ()
hCleverCSS = hParameters >>= hParametrizedCleverCSS

hParametrizedCleverCSS
  :: (MonadIO m, Request m, Response m, Send m)
  => Parameters -> m ()
hParametrizedCleverCSS p =
  hRewriteExt (fmap ('c':)) (hFileFilter convert)
  where convert = either id id . flip (cleverCSSConvert "") (map (fmap $ maybe "" id) p)

