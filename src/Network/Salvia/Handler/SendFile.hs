module Network.Salvia.Handler.SendFile (hSendFileResource) where

import Control.Monad.Trans
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia
import Network.Socket.SendFile
import System.IO

hSendFileResource :: (MonadIO m, HttpM Response m, SendM m, QueueM m) => FilePath -> m ()
hSendFileResource f =
  hSafeIO (openBinaryFile f ReadMode) $ \fd ->
  hSafeIO (hFileSize fd)              $ \fs ->
    do response $
         do status        =: OK
            contentType   =: Just (fileMime f, Just "utf-8")
            contentLength =: Just fs
       enqueue (\(s, _) -> sendFile' s fd 0 fs)

