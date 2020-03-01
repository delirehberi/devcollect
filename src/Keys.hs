{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Keys where

import           Network.OAuth.OAuth2
import           URI.ByteString.QQ

githubKey :: OAuth2
githubKey = OAuth2
  { oauthClientId            = ""
  , oauthClientSecret        = Just ""
  , oauthCallback            = Just [uri|http://localhost:3000/oauthCallback|]
  , oauthOAuthorizeEndpoint  = [uri|https://github.com/login/oauth/authorize|]
  , oauthAccessTokenEndpoint =
    [uri|https://github.com/login/oauth/access_token|]
  }
