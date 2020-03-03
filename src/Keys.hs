{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Keys where

import           Network.OAuth.OAuth2
import           URI.ByteString.QQ
import           Data.Text                      ( Text )

githubKey :: (Text, Text) -> OAuth2
githubKey (key, secret) = OAuth2
  { oauthClientId            = key
  , oauthClientSecret        = Just secret
  , oauthCallback            = Just [uri|http://localhost:3000/oauthCallback|]
  , oauthOAuthorizeEndpoint  = [uri|https://github.com/login/oauth/authorize|]
  , oauthAccessTokenEndpoint =
    [uri|https://github.com/login/oauth/access_token|]
  }
