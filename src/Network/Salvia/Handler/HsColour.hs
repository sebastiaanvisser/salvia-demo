module Network.Salvia.Handler.HsColour (
    hHighlightHaskell
  , hHsColour
  , hHsColourCustomStyle

  , defaultStyleSheet
  ) where

import Control.Monad.Trans
import Data.Record.Label
import Language.Haskell.HsColour.CSS
import Network.Protocol.Http
import Network.Salvia.Handlers
import Network.Salvia.Httpd

hHighlightHaskell :: RequestM m => m a -> m a -> m a
hHighlightHaskell highlighter = 
  hExtensionRouter [
    (Just "hs",  highlighter)
  , (Just "lhs", highlighter)
  , (Just "ag",  highlighter)
  ]

hHsColour :: (SendM m, ResponseM m, MonadIO m) => FilePath -> m ()
hHsColour = hHsColourCustomStyle (Left defaultStyleSheet)

-- Left means direct inclusion of stylesheet, right means link to external
-- stylesheet.

hHsColourCustomStyle
  :: (SendM m, ResponseM m, MonadIO m)
  => Either String String -> FilePath -> m ()
hHsColourCustomStyle style r = do
  sendStr (either id makeStyleLink style)
  hFileResourceFilter (hscolour False True "") r
  response (contentType =: Just ("text/html", Just "utf-8"))

makeStyleLink :: String -> String
makeStyleLink css = "<link rel=\"stylesheet\" type=\"text/css\" href=\"" ++ css ++ "\"></link>"

defaultStyleSheet :: String
defaultStyleSheet = filter (/=' ') $ concat [
    "<style>"
  , ".varop      { color : #960; font-weight : normal; }"
  , ".keyglyph   { color : #960; font-weight : normal; }"
  , ".definition { color : #005; font-weight : bold;   }"
  , ".varid      { color : #444; font-weight : normal; }"
  , ".keyword    { color : #000; font-weight : bold;   }"
  , ".comment    { color : #44f; font-weight : normal; }"
  , ".conid      { color : #000; font-weight : normal; }"
  , ".num        { color : #00a; font-weight : normal; }"
  , ".str        { color : #a00; font-weight : normal; }"
  , "</style>"
  , ""
  ]

