{-# LANGUAGE FlexibleContexts #-}
module Network.Salvia.Handler.HsColour
( hHighlightHaskell
, hHsColour
, hHsColourCustomStyle
, defaultStyleSheet
)
where

import Control.Monad.Trans
import Data.List
import Data.Record.Label
import Language.Haskell.HsColour.CSS
import Network.Protocol.Http
import Network.Salvia.Handlers
import Network.Salvia.Httpd

hHighlightHaskell :: HttpM Request m => m a -> m a -> m a
hHighlightHaskell highlighter = 
  hExtensionRouter
    [ (Just "hs",  highlighter) -- Haskell sources.
    , (Just "lhs", highlighter) -- Literate Haskell sources.
    , (Just "ag",  highlighter) -- Attribute grammar files.
    ]

hHsColour :: (SendM m, HttpM Response m, MonadIO m) => FilePath -> m ()
hHsColour = hHsColourCustomStyle (Left defaultStyleSheet)

-- | Left means direct inclusion of stylesheet, right means link to external
-- stylesheet.

hHsColourCustomStyle :: (SendM m, HttpM Response m, MonadIO m) => Either String String -> FilePath -> m ()
hHsColourCustomStyle style file =
  do send (either id makeStyleLink style)
     hFileResourceFilter (hscolour True) file
     response (contentType =: Just ("text/html", Just "utf-8"))

makeStyleLink :: String -> String
makeStyleLink css = "<link rel=\"stylesheet\" type=\"text/css\" href=\"" ++ css ++ "\"></link>"

defaultStyleSheet :: String
defaultStyleSheet = filter (/=' ') $ intercalate "\n"
  [ "<style>"
  , ".hs-varop      { color : #960; font-weight : normal; }"
  , ".hs-keyglyph   { color : #960; font-weight : normal; }"
  , ".hs-definition { color : #005; font-weight : bold;   }"
  , ".hs-varid      { color : #444; font-weight : normal; }"
  , ".hs-keyword    { color : #000; font-weight : bold;   }"
  , ".hs-comment    { color : #44f; font-weight : normal; }"
  , ".hs-conid      { color : #000; font-weight : normal; }"
  , ".hs-num        { color : #00a; font-weight : normal; }"
  , ".hs-str        { color : #a00; font-weight : normal; }"
  , "</style>"
  , ""
  ]

