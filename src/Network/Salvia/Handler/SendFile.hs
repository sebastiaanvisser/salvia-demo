module Network.Salvia.Handler.SendFile (
    hSendFileResource
  ) where

import Control.Monad.Trans
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Handlers
import Network.Salvia.Httpd
import Network.Socket.SendFile
import System.IO

hSendFileResource :: (Socket m, Send m, MonadIO m, Response m) => String -> m ()
hSendFileResource f =
  hSafeIO (openBinaryFile f ReadMode) $ \fd ->
  hSafeIO (hFileSize fd)                 $ \fs ->
    do response $
         do setM status OK
            setM contentType (fileMime f, Just utf8)
            setM contentLength (Just fs)
       raw (\(s, _) -> sendFile' s fd 0 fs)

