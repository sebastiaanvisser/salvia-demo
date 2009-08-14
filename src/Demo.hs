module Main where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Trans
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Salvia.Handlers
import Network.Salvia.Httpd
import Network.Socket hiding (Socket)

-- Serve the current directory.

main :: IO ()
main =
  do addr     <- inet_addr "127.0.0.1"
     count    <- atomically (newTVar 0)
     sessions <- mkSessions :: IO (Sessions ())
     putStrLn "started"
     server
       (defaultConfig { listenAddr = addr, listenPort = 8080 })
       (hSessionEnv count sessions myHandler)
       ()

-- Serve the current directory.

myHandler
  :: (MonadIO m, Alternative m, RequestM m, ResponseM m, SendM m, SocketM m)
  => TSession () -> m ()
myHandler _ = hExtendedFileSystem "."

