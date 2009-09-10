module Main where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Trans
import Network.Protocol.Http hiding (server)
import Network.Salvia hiding (server)
import Network.Salvia.Core.Server
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Salvia.Handler.ColorLog
import Network.Socket hiding (Socket)
import System.IO

-- Serve the current directory.

main :: IO ()
main =
  do addr     <- inet_addr "127.0.0.1"
     count    <- atomically (newTVar 0)
     sessions <- mkSessions :: IO (Sessions ())
     nul <- openFile "/dev/null" AppendMode
     putStrLn "started"
     server
       (defaultConfig { listenAddr = addr, listenPort = 8080 })
       (hSessionEnv nul count sessions (\s -> myHandler s >> hColorLogWithCounter count stdout))
       ()

-- Serve the current directory.

myHandler
  :: (MonadIO m, BodyM Request m, HttpM Request m, HttpM Response m, QueueM m, Alternative m)
  => TSession () -> m ()
myHandler _ = hExtendedFileSystem "."

