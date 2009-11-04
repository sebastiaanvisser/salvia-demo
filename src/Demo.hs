module Main where

import Control.Concurrent.STM
import Network.Salvia hiding (server)
import Network.Protocol.Http (Status (..))
import Network.Salvia.Impl.Server
import Network.Salvia.Handler.ColorLog
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Socket hiding (Socket, send)
import System.IO
import Prelude hiding (read)

main :: IO ()
main =
  do addr     <- inet_addr "127.0.0.1"
     count    <- atomically (newTVar 0)
     sessions <- mkSessions :: IO (Sessions (UserPayload ()))

     udb <- read (fileBackend "users.db") >>= atomically . newTVar

     let myConfig = defaultConfig
           { listenAddr = addr
           , listenPort = 8080
           }

     let myHandler = hDefaultEnv $
           do prolongSession 60
              hPathRouter
                [ ("/loginfo",   hLoginfo)
                , ("/logout",    logout)
                , ("/login",     login udb unauth (const $ hRedirect "/"))
                , ("/signup",    signup udb ["secret"] unauth (const $ hRedirect "/"))
                , ("/thesecret", authorized "secret" unauth (const $ send "surprise!"))
                ] $ hExtendedFileSystem "www"
              hColorLogWithCounter count stdout
           where unauth = hCustomError Unauthorized "fail!"

     putStrLn "started"
     server myConfig myHandler sessions

