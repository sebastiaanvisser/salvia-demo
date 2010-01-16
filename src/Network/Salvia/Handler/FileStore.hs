{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Network.Salvia.Handler.FileStore (hFileStore, hFileStoreFile, hFileStoreDirectory) where

import Control.Exception
import Control.Monad.Trans
import Data.List (intercalate)
import Data.List.Split
import Data.FileStore
import Data.Record.Label
import Network.Protocol.Http hiding (NotFound)
import Network.Protocol.Uri
import Network.Salvia.Handlers
import Network.Salvia.Httpd
import qualified Network.Protocol.Http as Http

-- Top level filestore server.

hFileStore
  :: (MonadIO m, BodyM Request m, HttpM' m, SendM m)
  => FileStore -> Author -> FilePath -> m ()
hFileStore fs author =
  hFileTypeDispatcher
    (hFileStoreDirectory fs)
    (hFileStoreFile fs author)

hFileStoreFile
  :: (MonadIO m, BodyM Request m, HttpM' m, SendM m)
  => FileStore -> Author -> FilePath -> m ()
hFileStoreFile fs author _ =
  do m <- request (getM method)
     u <- request (getM asUri)
     let p = mkRelative (get path u)
         q = get query u

     -- REST based routing.
     case (p, m, q) of
       ("index",  GET,    _        ) -> hIndex     fs
       ("search", GET,    _        ) -> hSearch    fs q
       (_,        GET,    "history") -> hHistory   fs p
       (_,        GET,    "latest" ) -> hLatest    fs p
       (_,        GET,    _        ) -> hRetrieve  fs p q
       (_,        PUT,    _        ) -> hSave      fs p q author
       (_,        DELETE, _        ) -> hDelete    fs p q author
       _                             -> hError Http.NotFound

hFileStoreDirectory
  :: (MonadIO m, BodyM Request m, HttpM' m, SendM m)
  => FileStore -> FilePath -> m ()
hFileStoreDirectory fs _ =
  do u <- request (getM asUri)
     let p = mkRelative (get path u)
     run (directory fs p) (intercalate "\n" . map showFS)
  where showFS (FSFile      f) = f
        showFS (FSDirectory d) = d ++ "/"

-- Type class alias.

class    (MonadIO m, BodyM Request m, HttpM' m, SendM m) => F m where
instance (MonadIO m, BodyM Request m, HttpM' m, SendM m) => F m

-- Specific filestore handlers.

hIndex :: F m => FileStore -> m ()
hIndex fs = run (index fs) (intercalate "\n")

hSearch :: F m => FileStore -> String -> m ()
hSearch fs q =
  do run (search fs sq) showMatches
  where showMatches = intercalate "\n" . map showMatch
        showMatch (SearchMatch f n l) = intercalate ":" [f, show n, l]
        sq = defaultSearchQuery
               { queryMatchAll   = False
               , queryWholeWords = False
               , queryIgnoreCase = False
               , queryPatterns   = splitOn "&" q
               }

hRetrieve :: F m => FileStore -> FilePath -> String -> m ()
hRetrieve fs p q = run (retrieve fs p (if null q then Nothing else Just q)) id

hLatest :: F m => FileStore -> FilePath -> m ()
hLatest fs p = run (latest fs p) id

hSave :: F m => FileStore -> FilePath -> Description -> Author -> m ()
hSave fs p q author =
  do b <- hRawRequestBody
     run (save fs p author q b) (const "document saved\n")

hDelete :: F m => FileStore -> FilePath -> Description -> Author -> m ()
hDelete fs p q author = run (delete fs p author q) (const "document deleted\n")

hHistory :: F m => FileStore -> FilePath -> m ()
hHistory fs p =
  run (history fs [p] (TimeRange Nothing Nothing)) showHistory
  where showHistory = intercalate "\n" . map showRevision
        showRevision (Revision i d a s _) = intercalate "," [i, show d, showAuthor a, s]
        showAuthor (Author n e) = n ++ " <" ++ e ++ ">"

-- Helper functions.

run :: F m => IO a -> (a -> String) -> m ()
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

mkRelative :: String -> String
mkRelative = dropWhile (=='/')

