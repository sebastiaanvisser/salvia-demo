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
  :: (MonadIO m, RequestM m, ResponseM m, SendM m, Alternative m)
  => m () -> m () -> m ()
hFilterCSS css = hExtension (Just "css") (hFile <|> css)

hCleverCSS :: (MonadIO m, RequestM m, ResponseM m, SendM m) => m ()
hCleverCSS = hParameters >>= hParametrizedCleverCSS

hParametrizedCleverCSS
  :: (MonadIO m, RequestM m, ResponseM m, SendM m)
  => Parameters -> m ()
hParametrizedCleverCSS p =
  hRewriteExt (fmap ('c':)) (hFileFilter convert)
  where convert = either id id . flip (cleverCSSConvert "") (map (fmap $ maybe "" id) p)

