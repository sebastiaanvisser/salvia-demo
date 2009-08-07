module Network.Salvia.Handler.SendFile (
    hSendFileResource
  ) where

import Data.Record.Label
import Control.Monad.Trans
import Network.Protocol.Uri
import Network.Protocol.Http
import Network.Protocol.Mime
import Network.Salvia.Handlers
import Network.Salvia.Httpd
import Network.Socket.SendFile
import System.IO

hSendFileResource :: (Socket m, Send m, MonadIO m, Response m) => String -> m ()
hSendFileResource file = do
  let m = maybe defaultMime id $ (either (const Nothing) Just (parseURI file) >>= mimetype . lget path)
  sck <- rawSock
  hSafeIO (openBinaryFile file ReadMode) $ \fd ->
    hSafeIO (hFileSize fd)               $ \fs ->
      do response $
           do setM status OK
              setM contentType (m, Just utf8)
              setM contentLength (Just fs)
         hSafeIO (sendFile' sck fd 0 fs) $
           return

