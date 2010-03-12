module Main where

import Network.Salvia.Impl.Cgi
import Network.Salvia.Handler.ExtendedFileSystem

main :: IO ()
main = start "/code/salvia-extras" (hCgiEnv (hExtendedFileSystem ".")) ()

