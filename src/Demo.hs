module Main where

import Control.Applicative
import Control.Concurrent.STM
import Data.FileStore.Git
import Data.FileStore
import Data.Maybe
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia hiding (server)
import Network.Salvia.Handler.ColorLog
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Salvia.Handler.StringTemplate
import Network.Salvia.Handler.FileStore
import Network.Salvia.Impl.Server
import Network.Socket hiding (Socket, send)
import Prelude hiding (read)
import System.IO
import qualified Control.Monad.State as S

main :: IO ()
main =
  do let template tmpl =
           do s <- show . isJust . get sPayload <$> getSession
              u <- maybe "anonymous" (get username) <$> hGetUser
              c <- show . (+1) . unCounter <$> payload S.get
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

     let store repo =
           hFileTypeDispatcher
             hDirectoryResource
             (hFileStore (gitFileStore repo) (Author "sebas" "sfvisser@cs.uu.nl"))
             repo

     let myHandler =
             (hDefaultEnv . myHandlerEnv)
             . hPrefixRouter
                 [ ("/code",        hExtendedFileSystem "src")
                 , ("/store",       store "www/ap")
                 ]
             . hPathRouter
                 [ ("/",            template "www/index.html")
                 , ("/favicon.ico", hError BadRequest)
                 , ("/loginfo",     hLoginfo)
                 , ("/logout",      logout >> hRedirect "/")
                 , ("/login",       login unauth (const $ hRedirect "/"))
                 , ("/signup",      whenWriteAccess (signup ["read-udb"] unauth (const $ hRedirect "/")))
                 , ("/users.db",    whenReadAccess (hFileResource "www/data/users.db"))
                 , ("/sources",     hCGI "www/demo.cgi")
                 ]
             $ (hExtendedFileSystem "www")
           where
             unauth          = hCustomError Unauthorized "unauthorized, please login"
             whenReadAccess  = authorized (Just "read-udb")  unauth . const
             whenWriteAccess = authorized (Just "write-udb") unauth . const


     counter  <- atomically (newTVar (Counter 0))
     sessions <- mkSessions >>= atomically . newTVar :: IO (TVar (Sessions (UserPayload Bool)))
     userDB   <- read (fileBackend "www/data/users.db") >>= atomically . newTVar
     addr <- inet_addr "127.0.0.1"

     let myPayload = userDB & counter & sessions

     let myConfig = defaultConfig
           { listenOn =
               [ SockAddrInet 8080 addr
               , SockAddrInet 9090 addr
               ] }

     start myConfig myHandler myPayload

