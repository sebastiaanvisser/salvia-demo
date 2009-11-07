module Main where

import Control.Monad.Trans
import Control.Applicative
import Control.Concurrent.STM
import Data.Maybe
import Data.Record.Label
import Network.Protocol.Http (Status (..))
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
  do addr     <- inet_addr "127.0.0.1"
     count    <- atomically (newTVar 0)
     sessions <- mkSessions :: IO (Sessions (UserPayload ()))

     udb <- read (fileBackend "www/data/users.db") >>= atomically . newTVar

     let myConfig = defaultConfig
           { listenOn = [ SockAddrInet 8080 addr
                        , SockAddrInet 9090 addr
                        ] }

         myHandlerEnv handler =
           do prolongSession (60 * 60) -- Expire after one hour of inactivity.
              hPortRouter
                [ ( 8080
                  , hVirtualHosting
                      [ ("127.0.0.1", hRedirect "http://localhost:8080/")
                      ] handler)
                ] (hCustomError Forbidden "Public service running on port 8080.")
              hColorLogWithCounter count stdout

         unauth          = hCustomError Unauthorized "unauthorized, please login"
         whenReadAccess  = authorized (Just "read-udb")  unauth . const
         whenWriteAccess = authorized (Just "write-udb") unauth . const
         getUser         = authorized Nothing (return Nothing) (return . Just)
         initialRights   = ["read-udb"]

         template tmpl =
           do s <- show . isJust . get sPayload <$> getSession
              u <- maybe "anonymous" (get username) <$> getUser
              c <- show <$> liftIO (atomically (readTVar count))
              hStringTemplate tmpl
                [ ("loggedin", s)
                , ("username", u)
                , ("counter",  c)
                ]

         myHandler = (hDefaultEnv . myHandlerEnv) $
           do hPrefix "/code" (hExtendedFileSystem "src")
                $ hPathRouter
                    [ ("/",            template "www/index.html")
                    , ("/favicon.ico", hError BadRequest)
                    , ("/loginfo",     hLoginfo)
                    , ("/logout",      logout >> hRedirect "/")
                    , ("/login",       login udb unauth (const $ hRedirect "/"))
                    , ("/signup",      whenWriteAccess (signup udb initialRights unauth (const $ hRedirect "/")))
                    , ("/users.db",    whenReadAccess (hFileResource "www/data/users.db"))
                    , ("/sources",     hCGI "www/demo.cgi")
                    ] (hExtendedFileSystem "www")

     putStrLn "started"
     server myConfig myHandler sessions

