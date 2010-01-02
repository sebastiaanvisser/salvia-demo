{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.ExtendedFileSystem (hExtendedFileSystem) where

import Control.Applicative
import Control.Monad.Trans
import Network.Protocol.Http
import Network.Salvia
import Network.Salvia.Handler.CleverCSS
import Network.Salvia.Handler.HsColour
import Network.Salvia.Handler.SendFile

hExtendedFileSystem
  :: (MonadIO m, HttpM' m, QueueM m, SendM m, BodyM Request m, Alternative m)
  => String -> m ()
hExtendedFileSystem dir = hFileTypeDispatcher hDirectoryResource f dir
  where
  f file = hHighlightHaskell (hHsColour file)
         . hWithDir dir
         . hFilterCSS hCleverCSS
         . hSendFileResource 
         $ file

