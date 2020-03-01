module Utils where

import           Network.OAuth.OAuth2
import           Data.ByteString                ( ByteString )
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Encoding            as TE
import           URI.ByteString


createCodeUri :: OAuth2 -> [(ByteString, ByteString)] -> Text
createCodeUri key params =
  TL.fromStrict
    $ TE.decodeUtf8
    $ serializeURIRef'
    $ appendQueryParams params
    $ authorizationUrl key