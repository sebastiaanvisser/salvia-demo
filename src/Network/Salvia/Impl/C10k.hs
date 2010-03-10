module Network.Salvia.Impl.C10k (start) where

import Control.Monad.State
import Network.C10kServer
import Network.TCPInfo
import Network.Salvia.Impl.Config
import Network.Salvia.Impl.Context 
import Network.Salvia.Impl.Handler
import Network.Socket

start :: String -> String -> C10kConfig -> Handler Config p () -> p -> IO ()
start host admin conf handler payload =
   do putStrLn ("Starting listening server on: 0.0.0.0:" ++ portName conf)
      runC10kServer
        (\s h i ->
          do my   <- (addrAddress . head) `fmap` getAddrInfo Nothing (Just (myAddr i))   (Just (myPort i))
             peer <- (addrAddress . head) `fmap` getAddrInfo Nothing (Just (peerAddr i)) (Just (peerPort i))
             evalStateT
               (unHandler handler)
               (mkContext (Config host admin [] 16) payload peer my s h)
        ) conf

{-

test :: IO ()
test = 
  server "localhost" "root@localhost"
  C10kConfig
    { initHook               = return ()
    , exitHook               = \s -> putStrLn ("C10k error:" ++ s)
    , parentStartedHook      = return ()
    , startedHook            = return ()
    , sleepTimer             = 10
    , preforkProcessNumber   = 10
    , threadNumberPerProcess = 100
    , portName               = "8080"
    , pidFile                = "pid"
    , user                   = "sebas"
    , group                  = "wheel"
    }
  (hDefaultEnv (hExtendedFileSystem "." >> hColorLog stdout))
  ()

-}
