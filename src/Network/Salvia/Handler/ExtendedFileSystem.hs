module Network.Salvia.Handler.ExtendedFileSystem (hExtendedFileSystem) where

import Control.Applicative
import Control.Monad.Trans
import Network.Salvia.Handler.CleverCSS
import Network.Salvia.Handler.HsColour
import Network.Salvia.Handler.SendFile
import Network.Salvia.Handlers
import Network.Salvia.Httpd

hExtendedFileSystem
  :: (MonadIO m, Alternative m, RequestM m, ResponseM m, SendM m, SocketM m)
  => String -> m ()
hExtendedFileSystem dir =
  hFileTypeDispatcher
  hDirectoryResource (\r ->
      hHighlightHaskell (hHsColour r)
    $ hWithDir dir
    $ hFilterCSS hCleverCSS
    $ hSendFileResource r
    ) dir

