{-# LANGUAGE FlexibleContexts, TypeOperators #-}
module Network.Salvia.Handler.WebSocket
( Protocol
, upgrade
, wsOrigin
, wsLocation
, wsProtocol
, hWebSocket
, hRecvFrameNonBlocking
, hSendFrame
, hOnMessage
)
where

import Control.Concurrent
import Control.Monad.State
import Data.Record.Label hiding (get)
import Network.Protocol.Http hiding (NotFound)
import Network.Salvia.Handlers
import Network.Salvia.Httpd hiding (server, hostname)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as U

type Protocol = String
type WebSocketT m a = StateT U.ByteString m a

wsOrigin, wsLocation, wsProtocol :: Http a :-> Maybe Value
wsOrigin   = header "WebSocket-Origin"
wsLocation = header "WebSocket-Location"
wsProtocol = header "WebSocket-Protocol"

hWebSocket :: (RawHttpM Request m, FlushM Response m, HttpM' m) => Value -> WebSocketT m a -> m a
hWebSocket proto act =
  do loc <- rawRequest (getM hostname)
     raw <- rawRequest (getM uri)
     org <- request (getM (header "Origin"))
     response $
       do headers    =: emptyHeaders
          status     =: CustomStatus 101 "Web Socket Protocol Handshake"
          upgrade    =: Just "WebSocket"
          connection =: Just "Upgrade"
          wsOrigin   =: org
          wsLocation =: fmap (("ws://" ++) . (++ raw)) loc 
          wsProtocol =: Just proto
     hFlushResponseHeaders
     evalStateT act B.empty

hRecvFrameNonBlocking :: (MonadIO m, SockM m) => Int -> StateT U.ByteString m (Maybe String)
hRecvFrameNonBlocking size =
  do prev <- get
     (frame, rest) <- lift $
       do s <- sock
          raw <- liftIO (B.hGetNonBlocking s size)
          let (first, rest) = B.break (== 0xFF) raw
          if not (B.null rest) && B.head rest == 0xFF
            then let frame = U.toString (B.dropWhile (== 0) (prev `B.append` first)) in
                 return (Just frame, B.tail rest)
            else return (Nothing, prev `B.append` first)
     put rest
     return frame

hSendFrame :: (FlushM Response m, SendM m) => String -> m ()
hSendFrame str =
  do sendBs (B.singleton 0x00)
     send str
     sendBs (B.singleton 0xFF)
     flushQueue forResponse

hOnMessage :: (SockM m, MonadIO m) => Int -> (String -> m ()) -> WebSocketT m ()
hOnMessage ms act = loop
  where
  loop =
    do lift . liftIO $ threadDelay (ms * 1000)
       frame <- hRecvFrameNonBlocking 100
       case frame of
         Nothing -> return ()
         Just f  -> lift (act f)
       loop

