{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Data.FileStore
import Data.Maybe
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia hiding (server)
import Network.Salvia.Handler.ColorLog
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Salvia.Handler.FileStore
import Network.Salvia.Handler.StringTemplate
-- import Network.Salvia.Handler.WebSocket
import Network.Salvia.Impl.Server
import Network.Socket hiding (Socket, send)
import Prelude hiding (read)
import System.IO
import qualified Control.Monad.State as S
import qualified Control.Concurrent.ThreadManager as Tm

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

     counter  <- atomically (newTVar (Counter 0))
     ping     <- atomically (newTMVar (0 :: Integer))
     sessions <- mkSessions >>= atomically . newTVar :: IO (TVar (Sessions (UserPayload Bool)))
     userDB   <- read (fileBackend "www/data/users.db") >>= atomically . newTVar
     addr <- inet_addr "127.0.0.1"

     let filestore repo = hFileStore (gitFileStore repo) (Author "sebas" "sfvisser@cs.uu.nl") repo

--      let wsLoop =  do tm <- (lift . liftIO) Tm.make
--                       _ <- lift . forker tm . forever $
--                              do c <- (liftIO . atomically) (readTMVar ping)
--                                 hSendFrame (show c)
-- 
--                       -- hOnMessage 100 $ (const . liftIO . atomically) (takeTMVar ping >>= putTMVar ping . (+1))
--                     
--                       _ <- (lift . liftIO) (Tm.waitForAll tm)
--                       (lift . liftIO) (print "done")
--                       return ()

     let myHandler =
             (hDefaultEnv . myHandlerEnv)
             . hPrefixRouter
                 [ ("/code",        hExtendedFileSystem "src")
                 , ("/store",       filestore ".")
                 , ("/g",           hProxy "http://")
                 ]
             . hPathRouter
                 [ ("/",            template "www/index.html")
                 , ("/لغة عربية",   hCustomError OK "arabic")
                 , ("/Ελληνική",    hCustomError OK "greek")
                 , ("/Русский",     hCustomError OK "russian")
                 , ("/עִבְרִית",    hCustomError OK "hebrew")
                 -- , ("/ping",        hWebSocket "myproto" wsLoop)
                 , ("/favicon.ico", hError BadRequest)
                 , ("/loginfo",     loginfo)
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

     let myPayload = userDB & counter & sessions

     let myConfig = defaultConfig
           { listenOn =
               [ SockAddrInet 8080 addr
               , SockAddrInet 9090 addr
               ] }

     start myConfig myHandler myPayload

forker :: (ForkM m IO, MonadIO m) => Tm.ThreadManager -> m () -> m ThreadId
forker tm = forkM >=> liftIO . Tm.fork tm

