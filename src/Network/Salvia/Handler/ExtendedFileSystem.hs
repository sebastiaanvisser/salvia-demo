module Network.Salvia.Handler.ExtendedFileSystem (hExtendedFileSystem) where

import Control.Applicative
import Control.Monad.Trans
import Network.Protocol.Http
import Network.Salvia
import Network.Salvia.Handler.CleverCSS
import Network.Salvia.Handler.HsColour
import Network.Salvia.Handler.SendFile

hExtendedFileSystem
  :: (MonadIO m, HttpM Request m, HttpM Response m, QueueM m, BodyM Request m, Alternative m)
  => String -> m ()
hExtendedFileSystem dir =
  hFileTypeDispatcher
  hDirectoryResource (\r ->
      hHighlightHaskell (hHsColour r)
    $ hWithDir dir
    $ hFilterCSS hCleverCSS
    $ hSendFileResource r
    ) dir

