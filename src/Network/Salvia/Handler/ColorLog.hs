{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.ColorLog
( Counter (..)
, hCounter
, hColorLog
, hColorLogWithCounter
)
where

import Control.Applicative
import Control.Monad.State
import Data.List
import Data.Record.Label hiding (get)
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Network.Protocol.Http
import Network.Salvia.Interface
import System.IO
import System.Locale
import Util.Terminal

newtype Counter = Counter { unCounter :: Integer }

-- | This handler simply increases the request counter variable.

hCounter :: PayloadM p Counter m => m Counter
hCounter = payload (modify (Counter . (+1) . unCounter) >> get)

{- |
A simple logger that prints a summery of the request information to the
specified file handle.
-}

hColorLog :: (AddressM' m, MonadIO m, HttpM' m) => Handle -> m ()
hColorLog = logger Nothing

-- | Like `hLog` but also prints the request count since server startup.

hColorLogWithCounter :: (PayloadM p Counter m, AddressM' m, MonadIO m, HttpM' m) => Handle -> m ()
hColorLogWithCounter h = hCounter >>= flip logger h . Just

-- Helper functions.

logger :: (AddressM' m, MonadIO m, HttpM' m) => Maybe Counter -> Handle -> m ()
logger mcount h =
  do let count = maybe "-" (show . unCounter) mcount
     mt <- request (getM method)
     ur <- request (getM uri)
     st <- response (getM status)
     ca <- clientAddress
     sa <- serverAddress
     dt <- liftIO $
       do zone <- getCurrentTimeZone
          time <- utcToLocalTime zone <$> getCurrentTime
          return $ formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z" time
     liftIO
       . hPutStrLn h
       $ intercalate " ; "
         [ dt
         , show sa
         , count
         , methodToColor mt ++ show mt ++ reset
         , show ca
         , whiteBold ++ ur ++ reset
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

methodToColor :: Method -> String
methodToColor GET = whiteBold
methodToColor _   = yellowBold

