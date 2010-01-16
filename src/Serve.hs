module Main where

import Network.Salvia
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Salvia.Handler.ColorLog
import System.IO
import qualified Network.Salvia.Impl.Server as Server

main :: IO ()
main = Server.start defaultConfig serve ()
  where serve = hDefaultEnv (hExtendedFileSystem "." >> hColorLog stdout)
