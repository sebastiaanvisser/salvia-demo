{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.FileStore (hFileStore) where

import Control.Exception
import Control.Monad.Trans
import Data.List (intercalate)
import Data.FileStore
import Data.Record.Label
import Network.Protocol.Http hiding (NotFound)
import Network.Protocol.Uri
import Network.Salvia.Handlers
import Network.Salvia.Httpd
import qualified Network.Protocol.Http as Http

-- Top level filestore server.

hFileStore
  :: (MonadIO m, SendM m, HttpM' m, HttpM Request m, BodyM Request m)
  => FileStore -> Author -> FilePath -> m ()
hFileStore fs author _ =
  do m <- request (getM method)
     u <- request (getM asUri)
     let p = dropWhile (=='/') (get path u)
         q = get query u
     case (m, q) of
       (GET,    "history") -> hHistory   fs p
       (GET,    "latest" ) -> hLatest    fs p
       (GET,    _        ) -> hRetrieve  fs p q
       (PUT,    _        ) -> hSave      fs p q author
       (DELETE, _        ) -> hDelete    fs p q author
       _                   -> hError Http.NotFound

-- Specific filestore handlers.

hRetrieve :: (MonadIO m, HttpM Response m, SendM m) => FileStore -> FilePath -> String -> m ()
hRetrieve fs p q = run (retrieve fs p (if null q then Nothing else Just q)) id

hLatest :: (MonadIO m, HttpM Response m, SendM m) => FileStore -> FilePath -> m ()
hLatest fs p = run (latest fs p) id

hSave :: (BodyM Request m, MonadIO m, HttpM Response m, SendM m) => FileStore -> FilePath -> Description -> Author -> m ()
hSave fs p q author =
  do mb <- hRawRequestBody
     case mb of
       Nothing -> hCustomError InternalServerError "no document available"
       Just b  -> run (save fs p author q b) (const "document saved\n")

hDelete :: (MonadIO m, HttpM Response m, SendM m) => FileStore -> FilePath -> Description -> Author -> m ()
hDelete fs p q author = run (delete fs p author q) (const "document deleted\n")

hHistory :: (MonadIO m, HttpM Response m, SendM m) => FileStore -> FilePath -> m ()
hHistory fs p =
  run (history fs [p] (TimeRange Nothing Nothing)) showHistory
  where
    showHistory = intercalate "\n" . map showRevision
    showRevision (Revision i d a s _) = intercalate "," [i, show d, showAuthor a, s]
    showAuthor (Author n e) = n ++ " <" ++ e ++ ">"

-- Helper functions.

run :: (MonadIO m, HttpM Response m, SendM m) => IO a -> (a -> String) -> m ()
run action f =
  do e <- liftIO (try action)
     case e of
       Left err  -> hCustomError (mkError err) (show err)
       Right res -> send (f res)

mkError :: FileStoreError -> Status
mkError RepositoryExists     = Http.BadRequest
mkError ResourceExists       = Http.BadRequest
mkError NotFound             = Http.NotFound
mkError IllegalResourceName  = Http.NotFound
mkError Unchanged            = Http.NotFound
mkError UnsupportedOperation = Http.BadRequest
mkError NoMaxCount           = Http.InternalServerError
mkError _                    = Http.BadRequest

