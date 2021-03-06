{-# LANGUAGE CPP, FlexibleContexts, NoMonomorphismRestriction #-}
module Main where

import Control.Applicative
-- import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import Data.FileStore
import Data.Maybe
import Data.Record.Label
import Network.Protocol.Http hiding (hostname)
import Network.Salvia
import Network.Salvia.Handler.ColorLog
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Salvia.Handler.FileStore
import Network.Salvia.Handler.Login
import Network.Salvia.Handler.Session
import Network.Salvia.Handler.StringTemplate
-- import Network.Salvia.Handler.WebSocket
import Network.Socket hiding (Socket, send)
import Prelude hiding (read)
import System.IO
-- import qualified Control.Concurrent.ThreadManager as Tm
#ifdef Cabal
import Paths_salvia_demo
#endif
-- At the bottom to prevent warnings (?)

main :: IO ()
main =
  do -- Index HTML template.
     let template tmpl =
           do s <- show . isJust . getL sPayload <$> getSession
              u <- maybe "anonymous" (getL username) <$> hGetUser
              c <- show . (+1) . unCounter <$> payload get
              hStringTemplate tmpl
                [ ("loggedin", s)
                , ("username", u)
                , ("counter",  c)
                ]

     -- Handler environment to run handler in.
     let myHandlerEnv handler =
           do prolongSession (60 * 60) -- Expire after one hour of inactivity.
              hPortRouter
                [ ( 8080
                  , hVirtualHosting
                      [ ("localhost", hRedirect "http://127.0.0.1:8080/")
                      ] handler)
                ] (hCustomError Forbidden "Public service running on port 8080.")
              hColorLogWithCounter stdout

     -- Data directories packed in Cabal package.
#ifdef Cabal
     www <- getDataFileName "www"
     db  <- getDataFileName "www/data/users.db"
     cgi <- getDataFileName "www/demo.cgi"
     idx <- getDataFileName "www/index.html"
#else
     www <- return "www"
     db  <- return "www/data/users.db"
     cgi <- return "www/demo.cgi"
     idx <- return "www/index.html"
#endif

--      tm       <- Tm.make
     counter  <- atomically (newTVar (Counter 0))
--      ping     <- atomically (newTMVar (0 :: Integer))
     sessions <- atomically (newTVar mkSessions) :: IO (TVar (Sessions (UserPayload Bool)))
     userDB   <- read (fileBackend db) >>= atomically . newTVar
     addr     <- inet_addr "0.0.0.0"

     let filestore repo = hFileStore (defaultRouter (gitFileStore repo)) 
                                     (Author "sebas" "haskell@fvisser.nl")
                                     repo

--      let forker tm ac = fork ac >> liftIO . Tm.fork tm

--      let ws = forker tm (hSendTMVar 100 ping show)
--               >> hOnMessageUpdateTMVar 100 (const (+1)) ping

     let myHandler =
             (hDefaultEnv . myHandlerEnv)
             . hPrefixRouter
                 [ ("/code",        hExtendedFileSystem "src")
                 , ("/store",       filestore ".")
                 ]
             . hPathRouter
                 [ ("/",            template idx)
                 , ("/لغة عربية",   hCustomError OK "arabic")
                 , ("/Ελληνική",    hCustomError OK "greek")
                 , ("/Русский",     hCustomError OK "russian")
                 , ("/עִבְרִית",    hCustomError OK "hebrew")
--                  , ("/ping",        hWebSocket "myproto" (lift (hColorLogWithCounter stdout) >> ws))
                 , ("/favicon.ico", hError BadRequest)
                 , ("/loginfo",     loginfo)
                 , ("/logout",      logout >> hRedirect "/")
                 , ("/login",       login unauth (const $ hRedirect "/"))
                 , ("/signup",      whenWriteAccess (signup ["read-udb"] unauth (const $ hRedirect "/")))
                 , ("/users.db",    whenReadAccess (hFileResource db))
                 , ("/sources",     hCGI cgi)
                 ]
             $ (hExtendedFileSystem www)
           where
             unauth          = hCustomError Unauthorized "unauthorized, please login"
             whenReadAccess  = authorized (Just "read-udb")  unauth . const
             whenWriteAccess = authorized (Just "write-udb") unauth . const

     let myPayload = userDB & counter & sessions

     let myConfig = defaultConfig
           { domain = "localhost"
           , listenOn =
               [ SockAddrInet 8080 addr
               , SockAddrInet 9090 addr
               ] }

     start myConfig myHandler myPayload


