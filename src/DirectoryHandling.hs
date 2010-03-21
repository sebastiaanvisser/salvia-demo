module Main where

import Network.Salvia.Handler.ColorLog
import Network.Salvia.Handlers
import Network.Salvia.Impl.Server
import Network.Salvia.Impl.Config
import Network.Salvia.Interface
import Network.Protocol.Http
import System.IO

main :: IO ()
main = 
  start 
  defaultConfig
  -- hPathRouter takes a list of paths and a default handler (in our case, a 404)
  (hDefaultEnv (do hPathRouter [ ("/erik",  send "hi erik" )
                               , ("/sebas", send "hi sebas")
                               ]
                               (hError NotFound)
                   hColorLog stdout))
  ()
