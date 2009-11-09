module Main where

import Control.Monad.Trans
import Control.Applicative
import Control.Concurrent.STM
import Data.Maybe
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia hiding (server)
import Network.Salvia.Handler.ColorLog
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Salvia.Handler.StringTemplate
import Network.Salvia.Impl.Server
import Network.Socket hiding (Socket, send)
import Prelude hiding (read)
import System.IO

main :: IO ()
main =
  do let unauth          = hCustomError Unauthorized "unauthorized, please login"
         whenReadAccess  = authorized (Just "read-udb")  unauth . const
         whenWriteAccess = authorized (Just "write-udb") unauth . const

     addr     <- inet_addr "127.0.0.1"
     count    <- atomically (newTVar (Counter 0))
     sessions <- mkSessions >>= atomically . newTVar :: IO (TVar (Sessions (UserPayload Bool)))
     udb      <- read (fileBackend "www/data/users.db") >>= atomically . newTVar

     let template tmpl =
           do s <- show . isJust . get sPayload <$> getSession
              u <- maybe "anonymous" (get username) <$> hGetUser
              c <- show . (+1) . unCounter <$> liftIO (atomically (readTVar count))
              hStringTemplate tmpl
                [ ("loggedin", s)
                , ("username", u)
                , ("counter",  c)
                ]

     let myHandlerEnv handler =
           do prolongSession (60 * 60) -- Expire after one hour of inactivity.
              hPortRouter
                [ ( 8080
                  , hVirtualHosting
                      [ ("127.0.0.1", hRedirect "http://localhost:8080/")
                      ] handler)
                ] (hCustomError Forbidden "Public service running on port 8080.")
              hColorLogWithCounter stdout

     let myHandler = (hDefaultEnv . myHandlerEnv) $
           do hPrefix "/code" (hExtendedFileSystem "src")
                $ hPathRouter
                    [ ("/",            template "www/index.html")
                    , ("/favicon.ico", hError BadRequest)
                    , ("/loginfo",     hLoginfo)
                    , ("/logout",      logout >> hRedirect "/")
                    , ("/login",       login unauth (const $ hRedirect "/"))
                    , ("/signup",      whenWriteAccess (signup ["read-udb"] unauth (const $ hRedirect "/")))
                    , ("/users.db",    whenReadAccess (hFileResource "www/data/users.db"))
                    , ("/sources",     hCGI "www/demo.cgi")
                    ] (hExtendedFileSystem "www")

     let myConfig = defaultConfig
           { listenOn =
               [ SockAddrInet 8080 addr
               , SockAddrInet 9090 addr
               ] }

     start myConfig myHandler (udb, (count, sessions))

