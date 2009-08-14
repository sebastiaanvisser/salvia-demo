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

hSendFileResource :: (SocketM m, SendM m, MonadIO m, ResponseM m) => String -> m ()
hSendFileResource f =
  hSafeIO (openBinaryFile f ReadMode) $ \fd ->
  hSafeIO (hFileSize fd)              $ \fs ->
    do response $
         do status        =: OK
            contentType   =: Just (fileMime f, Just "utf-8")
            contentLength =: Just fs
       raw (\(s, _) -> sendFile' s fd 0 fs)

