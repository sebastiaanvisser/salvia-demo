module Main where

import Network.Salvia.Handler.ColorLog
import Network.Salvia.Handlers
import Network.Salvia.Impl.Server
import Network.Salvia.Impl.Config
import Network.Salvia.Interface
import System.IO

main :: IO ()
main = 
  start 
  defaultConfig
  -- This echoes the (parsed) query paramaters.
  (hDefaultEnv (do r <- hQueryParameters
                   send (show r)
                   hColorLog stdout))
  ()
