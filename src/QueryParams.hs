module Main where

import Network.Salvia.Handler.ColorLog
import Network.Salvia.Handlers
import Network.Salvia.Impl.Server
import Network.Salvia.Impl.Config
import Network.Salvia.Interface
import Network.Protocol.Uri
import Network.Protocol.Http
import System.IO
import Prelude hiding ((.))
import Control.Category
import Data.Record.Label

main :: IO ()
main = 
  start 
  defaultConfig
  -- This echoes the (parsed) query paramaters.
  (hDefaultEnv (do r <- request (getM $ queryParams . asUri)
                   send (show r)
                   hColorLog stdout))
  ()
