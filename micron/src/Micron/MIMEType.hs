module Micron.MIMEType (module Micron.MIMEType) where

import Data.ByteString.Char8 qualified as BC

appJson :: BC.ByteString
appJson = BC.pack "application/json"

textPlain :: BC.ByteString
textPlain = BC.pack "text/plain"

textHtml :: BC.ByteString
textHtml = BC.pack "text/html"
