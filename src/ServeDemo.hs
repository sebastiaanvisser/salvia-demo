module Main where

import Network.C10kServer
import Network.Salvia.Handler.ColorLog
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Salvia.Handlers
import Network.Salvia.Impl.C10k
import System.IO

main :: IO ()
main = 
  start "127.0.0.1" "root@localhost"
  C10kConfig
    { initHook               = return ()
    , exitHook               = \s -> putStrLn ("C10kServer error:" ++ s)
    , parentStartedHook      = return ()
    , startedHook            = return ()
    , sleepTimer             = 10
    , preforkProcessNumber   = 200
    , threadNumberPerProcess = 200
    , portName               = "8080"
    , pidFile                = "pid"
    , user                   = "sebas"
    , group                  = "wheel"
    }
  (hDefaultEnv (hExtendedFileSystem "." >> hColorLog stdout))
  ()

