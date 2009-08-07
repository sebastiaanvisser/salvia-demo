module Main where

import Control.Monad.Trans
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Salvia.Handlers
import Network.Salvia.Httpd
import Network.Socket hiding (Socket)

-- Serve the current directory.

main :: IO ()
main =
  do conf <- defaultConfig
     addr <- inet_addr "127.0.0.1"
     putStrLn "started"
     start 
       (conf { listenAddr = addr, listenPort = 8080 })
       (hDefaultEnv myHandler)

-- Serve the current directory.

myHandler :: (MonadIO m, Request m, Response m, Send m, Socket m) => m ()
myHandler = hExtendedFileSystem "."

