{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.ColorLog
( Counter (..)
, hCounter
, hColorLog
, hColorLogWithCounter
)
where

import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Record.Label hiding (get)
import Network.Protocol.Http
import Network.Salvia.Interface
import System.IO
import Terminal

newtype Counter = Counter { unCounter :: Integer }

{- | This handler simply increases the request counter variable. -}

hCounter :: PayloadM p Counter m => m Counter
hCounter = payload (modify (Counter . (+1) . unCounter) >> get)

{- |
A simple logger that prints a summery of the request information to the
specified file handle.
-}

hColorLog :: (AddressM' m, MonadIO m, HttpM' m) => Handle -> m ()
hColorLog = logger Nothing

{- | Like `hLog` but also prints the request count since server startup. -}

hColorLogWithCounter :: (PayloadM p Counter m, AddressM' m, MonadIO m, HttpM' m) => Handle -> m ()
hColorLogWithCounter h = hCounter >>= flip logger h . Just

-- Helper functions.

logger :: (AddressM' m, MonadIO m, HttpM' m) => Maybe Counter -> Handle -> m ()
logger mcount h =
  do let count = maybe "-" (show . unCounter) mcount
     mt <- request  (getM method)
     ur <- request  (getM uri)
     st <- response (getM status)
     dt <- response (getM date)
     ca <- clientAddress
     sa <- serverAddress
     liftIO
       . hPutStrLn h
       $ intercalate " ; "
         [ fromMaybe "" dt
         , show sa
         , count
         , show mt
         , show ca
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

