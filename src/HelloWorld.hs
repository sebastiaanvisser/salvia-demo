{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia
import Network.Salvia.Handler.ColorLog
import System.IO

main :: IO ()
main = start 
  defaultConfig                                    -- server configuration
  (hDefaultEnv (helloWorld >> hColorLog stdout))   -- handler in default environment
  ()                                               -- no server payload

-- Set the response content-type and send our HTML snippet.

helloWorld :: (SendM m, HttpM Response m) => m ()
helloWorld =
  do response (contentType =: Just ("text/html", Just "utf-8"))
     send "hello, <b>world</b>"

