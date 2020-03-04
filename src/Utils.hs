module Utils where

import           Network.OAuth.OAuth2
import           Data.ByteString                ( ByteString )
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Encoding            as TE
import           URI.ByteString
import           Web.Scotty.Internal.Types
import qualified Data.ByteString.Lazy.Char8    as BSL


createCodeUri :: OAuth2 -> [(ByteString, ByteString)] -> Text
createCodeUri key params =
  TL.fromStrict
    $ TE.decodeUtf8
    $ serializeURIRef'
    $ appendQueryParams params
    $ authorizationUrl key


paramValue :: Text -> [Param] -> [Text]
paramValue key = fmap snd . filter (hasParam key)

hasParam :: Text -> Param -> Bool
hasParam t = (== t) . fst

bslToText :: BSL.ByteString -> TL.Text
bslToText = TL.pack . BSL.unpack
