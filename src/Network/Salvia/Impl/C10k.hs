{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Salvia.Impl.C10k (start) where

import Data.Monoid
import Control.Monad.State
import Control.Applicative
import Network.C10kServer
import Network.Protocol.Http hiding (accept, hostname)
import Network.Salvia.Impl.Context 
import Network.Salvia.Interface
import Network.Salvia.Impl.Handler
import Network.Socket
import Network.TCPInfo

newtype C10kHandler p a = C10kHandler (Handler p a)
  deriving
  ( BodyM Request 
  , Alternative 
  , Applicative 
  , BodyM Response 
  , ClientAddressM 
  , FlushM Request 
  , FlushM Response 
  , Functor 
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
  , ForkM IO
  , ServerAddressM 
  , ServerM 
  , SockM 
  )

runC10kHandler :: C10kHandler p a -> Context p -> IO (a, Context p)
runC10kHandler (C10kHandler h) = runHandler h

start :: String -> String -> C10kConfig -> C10kHandler p () -> p -> IO ()
start hst admn conf handler pyld =
   do putStrLn ("Starting listening server on: 0.0.0.0:" ++ portName conf)
      flip runC10kServer conf $ \s h i ->
        do let addrPort a p = addrAddress . head <$> getAddrInfo Nothing (Just a) (Just p)
           my   <- addrPort (myAddr   i) (myPort   i)
           peer <- addrPort (peerAddr i) (peerPort i)
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
             , _cClientAddr  = peer
             , _cServerAddr  = my
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
