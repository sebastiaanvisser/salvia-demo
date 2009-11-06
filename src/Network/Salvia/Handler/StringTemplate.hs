module Network.Salvia.Handler.StringTemplate where

import Control.Monad.Trans
import Network.Protocol.Http
import Network.Salvia
import Text.StringTemplate

hStringTemplate
  :: (ToSElem a, MonadIO m, HttpM Response m, SendM m)
  => FilePath -> [(String, a)] -> m ()
hStringTemplate template attrs = hFileResourceFilter (render . setManyAttrib attrs . newSTMP) template

