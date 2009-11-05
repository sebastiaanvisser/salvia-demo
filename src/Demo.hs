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

     let myHandlerEnv handler =
           do prolongSession (60 * 60) -- Expire after one hour of inactivity.
              hPortRouter
                [ ( 8080
                  , hVirtualHosting
                      [ ("127.0.0.1", hRedirect "http://localhost:8080/")
                      ] handler
                  )
                ] (hCustomError Forbidden "Public service running on port 8080.")
              hColorLogWithCounter count stdout

     let myHandler = (hDefaultEnv . myHandlerEnv) $
           do hPathRouter
                [ ("/",            hFileResource "www/index.html")
                , ("/favicon.ico", hError BadRequest)
                , ("/loginfo",     hLoginfo)
                , ("/logout",      logout >> hRedirect "/")
                , ("/login",       login udb unauth (const $ hRedirect "/"))
                , ("/signup",      signup udb ["secret"] unauth (const $ hRedirect "/"))
                , ("/users.db",    authorized "secret" unauth (const $ hFileResource "users.db"))
                , ("/sources",     hCGI "./www/demo.cgi")
                ] $ hExtendedFileSystem "."
           where unauth = hCustomError Unauthorized "unauthorized, please login"

     putStrLn "started"
     server myConfig myHandler sessions

