{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Salvia.Impl.C10k where

import Data.Monoid
import Control.Monad.State
import Control.Applicative
import Network.C10kServer
import Network.Protocol.Http hiding (accept, hostname)
import Network.Salvia.Impl.Context 
import Network.Salvia.Interface
import Network.Salvia.Impl.Handler
import Network.Socket
import System.IO

newtype C10kHandler p a = C10kHandler (Handler p a)
  deriving
  ( BodyM Request 
  , Alternative 
  , Applicative 
  , BodyM Response 
  , ClientAddressM 
  , FlushM Request 
  , FlushM Response 
  , ForkM IO
  , Functor 
  , HandleM 
  , HttpM Request 
  , HttpM Response 
  , Monad
  , MonadIO 
  , MonadPlus 
  , Monoid
  , QueueM 
  , RawHttpM Request 
  , RawHttpM Response 
  , SendM 
  , ServerAddressM 
  , ServerM 
  , SocketM 
  )

runC10kHandler :: C10kHandler p a -> Context p -> IO (a, Context p)
runC10kHandler (C10kHandler h) = runHandler h

start :: String -> String -> C10kConfig -> C10kHandler p () -> p -> IO ()
start hst admn conf handler pyld =
   do putStrLn ("Starting listening server on: 0.0.0.0:" ++ portName conf)
      flip runC10kServer conf $ \s ->
        do p <- getPeerName s
           n <- getSocketName s
           h <- socketToHandle s ReadWriteMode
           _ <- runC10kHandler handler
             Context
             { _cServerHost  = hst
             , _cAdminMail   = admn
             , _cListenOn    = [{-todo-}]
             , _cPayload     = pyld
             , _cRequest     = emptyRequest
             , _cResponse    = emptyResponse
             , _cRawRequest  = emptyRequest
             , _cRawResponse = emptyResponse
             , _cSocket      = s
             , _cHandle      = h
             , _cClientAddr  = p
             , _cServerAddr  = n
             , _cQueue       = []
             }
           return ()

{-

test :: IO ()
test = 
  server "localhost" "root@localhost"
  C10kConfig
    { initHook               = return ()
    , exitHook               = \s -> putStrLn ("C10k error:" ++ s)
    , parentStartedHook      = return ()
    , startedHook            = return ()
    , sleepTimer             = 10
    , preforkProcessNumber   = 10
    , threadNumberPerProcess = 100
    , portName               = "8080"
    , pidFile                = "pid"
    , user                   = "sebas"
    , group                  = "wheel"
    }
  (hDefaultEnv (hExtendedFileSystem "." >> hColorLog stdout))
  ()

-}
