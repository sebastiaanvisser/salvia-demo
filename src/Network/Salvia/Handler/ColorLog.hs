module Network.Salvia.Handler.ColorLog {- doc ok -}
  ( hColorLog
  , hColorLogWithCounter
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

{- |
A simple logger that prints a summery of the request information to the
specified file handle.
-}

hColorLog :: (PeerM m, MonadIO m, HttpM Response m, HttpM Request m) => Handle -> m ()
hColorLog = logger Nothing

{- | Like `hLog` but also prints the request count since server startup. -}

hColorLogWithCounter :: (PeerM m, MonadIO m, HttpM Response m, HttpM Request m) => TVar Int -> Handle -> m ()
hColorLogWithCounter a = logger (Just a)

logger :: (PeerM m, MonadIO m, HttpM Response m, HttpM Request m) => Maybe (TVar Int) -> Handle -> m ()
logger count handle =
  do cnt <- case count of
       Nothing -> return "-"
       Just c' -> liftIO (show <$> atomically (readTVar c'))
     mt <- request  (getM method)
     ur <- request  (getM uri)
     st <- response (getM status)
     dt <- response (getM date)
     ad <- peer
     let code = codeFromStatus st
         t = case code of
                 c | c <= 199 -> blue
                   | c <= 299 -> green
                   | c <= 399 -> yellow
                   | c <= 499 -> red
                 _            -> magenta
     liftIO
       $ hPutStrLn handle
       $ intercalate " ; "
         [ maybe "" id dt
         , cnt
         , show ad
         , show mt
         , ur
         , t ++ (show code ++ " " ++ show st) ++ reset
         ]

