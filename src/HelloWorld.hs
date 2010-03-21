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
  (hDefaultEnv (send "hello, world." >> hColorLog stdout))
  ()
