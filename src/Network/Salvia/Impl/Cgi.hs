{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Network.Salvia.Impl.Cgi
( CgiHandler (..)
, hCgiEnv
, runCgiHandler
, start
)
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Record.Label
import Network.Protocol.Http hiding (accept, hostname)
import Network.Salvia.Handlers
import Network.Salvia.Impl.Context 
import Network.Salvia.Impl.Handler
import Network.Salvia.Interface
import Network.Socket
import System.Environment
import System.IO

newtype CgiHandler p a = CgiHandler (Handler p a)
  deriving
  ( BodyM Request 
  , Alternative 
  , Applicative 
  , ClientAddressM 
  , FlushM Request 
  , FlushM Response 
  , Functor 
  , HandleM
  , HttpM Request 
  , HttpM Response 
  , Monad
  , MonadIO 
  , MonadPlus 
  , Monoid
  , HandleQueueM 
  , QueueM 
  , RawHttpM Request 
  , RawHttpM Response 
  , SendM 
  , ForkM IO
  , ServerAddressM 
  , ServerM 
  )

hCgiEnv :: (FlushM Response m, MonadIO m, QueueM m, HttpM' m, HandleM m) => m a -> m ()
hCgiEnv handler =
  do hBanner "salvia-httpd"
     _ <- hHead handler
     h <- handle
     st <- response (getM status)
     liftIO $ hPutStr h (intercalate " " ["Status:", show (codeFromStatus st), show st] ++ "\r\n")
     hFlushHeadersOnly forResponse
     flushQueue forResponse

runCgiHandler :: CgiHandler p a -> Context p -> IO (a, Context p)
runCgiHandler (CgiHandler h) = runHandler h

start :: Show p => CgiHandler p () -> p -> IO ()
start handler pyld =
  do env <- getEnvironment

     -- Setup HTTP request from environment variables.
     let ur   = fromMaybe ""                   (lookup "REQUEST_URI"     env)
         qy   = maybe "" ('?':)                (lookup "QUERY_STRING"    env)
         mthd = maybe GET methodFromString     (lookup "REQUEST_METHOD"  env)
         prot = maybe http11 versionFromString (lookup "SERVER_PROTOCOL" env)
         req  = Http (Request mthd (fromJust (stripPrefix "/code/salvia-extras" ur) ++ qy)) prot (getHeaders env)

     -- Both the server and client address/port combinations.
     sa <- getAddrInfo Nothing (lookup "SERVER_ADDR" env) (lookup "SERVER_PORT" env)
     ca <- getAddrInfo Nothing (lookup "REMOTE_ADDR" env) (lookup "REMOTE_PORT" env)

     -- Run the handler with the context from the CGI environment.
     _ <- runCgiHandler handler
       Context
       { _cServerHost  = fromMaybe "" (lookup "SERVER_NAME"  env)
       , _cAdminMail   = fromMaybe "" (lookup "SERVER_ADMIN" env)
       , _cListenOn    = map addrAddress sa
       , _cPayload     = pyld
       , _cRequest     = req
       , _cResponse    = emptyResponse
       , _cRawRequest  = req
       , _cRawResponse = emptyResponse
       , _cSocket      = error "No socket available in CGI mode."
       , _cHandle      = stdout
       , _cClientAddr  = addrAddress (head ca)
       , _cServerAddr  = addrAddress (head sa)
       , _cQueue       = []
       }
     return ()

getHeaders :: [(String, String)] -> Headers
getHeaders =
    Headers
  . map (\(a, b) -> (norm a, b))
  . filter (("HTTP_" `isPrefixOf`) . fst)
  where norm = normalizeHeader . replace '_' '-' . fromJust . stripPrefix "HTTP_"

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)

