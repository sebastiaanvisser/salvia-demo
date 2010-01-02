{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.FileStore where

import Control.Monad.Trans
import Data.FileStore hiding (NotFound)
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Handlers
import Network.Salvia.Httpd
import Prelude hiding ((.), id)

hFileStore
  :: (MonadIO m, SendM m, HttpM' m, BodyM Request m) =>
     FileStore -> Author -> m ()
hFileStore fs author =
  do u <- request (getM asUri)
     let p = mkRelative (get path u)
         q = get query u
     liftIO (print "aap")
     contents <- hRawRequestBody
--      hMethodRouter
--        [ (GET,    liftIO (retrieve fs p (listToMaybe q)) >>= send)
--        , (PUT,    liftIO (save     fs p author q contents)     >> send "ok")
--        , (DELETE, liftIO (delete   fs p author q))
--        ] (hError NotFound)

     return ()
     where mkRelative = dropWhile (=='/')
           listToMaybe a = if null a then Nothing else Just a

hFileStoreSave fs name author mcontents =
  do return ()
