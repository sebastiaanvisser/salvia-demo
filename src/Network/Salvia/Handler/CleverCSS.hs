module Network.Salvia.Handler.CleverCSS
  ( hFilterCSS
  , hCleverCSS
  , hParametrizedCleverCSS
  )
where

import Control.Applicative
import Control.Monad.Trans
import Network.Protocol.Uri
import Network.Protocol.Http
import Network.Salvia
import Text.CSS.CleverCSS 

hFilterCSS
  :: (MonadIO m, HttpM Request m, HttpM Response m, QueueM m, Alternative m)
  => m () -> m () -> m ()
hFilterCSS css = hExtension (Just "css") (hFile <|> css)

hCleverCSS :: (MonadIO m, BodyM Request m, HttpM Request m, QueueM m, HttpM Response m) => m ()
hCleverCSS = 
  do p <- hRequestParameters "utf-8"
     maybe (return ()) hParametrizedCleverCSS p

hParametrizedCleverCSS
  :: (MonadIO m, HttpM Request m, HttpM Response m, QueueM m)
  => Parameters -> m ()
hParametrizedCleverCSS p =
  hRewriteExt (fmap ('c':)) (hFileFilter convert)
  where convert = either id id . flip (cleverCSSConvert "") (map (fmap $ maybe "" id) p)

