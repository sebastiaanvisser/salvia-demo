module Main where

import Network.Salvia.Impl.Cgi
import Network.Salvia.Handler.ExtendedFileSystem

main :: IO ()
main = start (hCgiEnv (hExtendedFileSystem ".")) ()

