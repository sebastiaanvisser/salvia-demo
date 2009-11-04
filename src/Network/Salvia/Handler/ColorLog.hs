module Network.Salvia.Handler.ColorLog
( Counter
, hColorLog
, hColorLogWithCounter
, hCounter
)
where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.State
import Data.List
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Core.Aspects hiding (server)
import System.IO
import Terminal

type Counter = TVar Integer

{- |
A simple logger that prints a summery of the request information to the
specified file handle.
-}

hColorLog :: (PeerM m, MonadIO m, HttpM' m) => Handle -> m ()
hColorLog = logger Nothing

{- | Like `hLog` but also prints the request count since server startup. -}

hColorLogWithCounter :: (PeerM m, MonadIO m, HttpM' m) => Counter -> Handle -> m ()
hColorLogWithCounter a h =
  do hCounter a
     c <- Just <$> (liftIO . atomically . readTVar) a
     logger c h

{- | This handler simply increases the request counter variable. -}

hCounter :: MonadIO m => TVar Integer -> m ()
hCounter c = (liftIO . atomically) (readTVar c >>= writeTVar c . (+1))

-- Helper functions.

logger :: (PeerM m, MonadIO m, HttpM' m) => Maybe Integer -> Handle -> m ()
logger mcount handle =
  do let count = maybe "-" show mcount
     mt <- request  (getM method)
     ur <- request  (getM uri)
     st <- response (getM status)
     dt <- response (getM date)
     ad <- peer
     liftIO
       $ hPutStrLn handle
       $ intercalate " ; "
         [ maybe "" id dt
         , count
         , show ad
         , show mt
         , ur
         , statusToColor st ++ show (codeFromStatus st) ++
           " " ++ show st ++ reset
         ]

statusToColor :: Status -> String
statusToColor st =
  case codeFromStatus st of
    c | c <= 199 -> blueBold
      | c <= 299 -> greenBold
      | c <= 399 -> yellowBold
      | c <= 499 -> redBold
    _            -> magentaBold

