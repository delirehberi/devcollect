{-#LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Github where

import           Data.Aeson
import Data.Bifunctor
import           GHC.Generics
import           Data.Hashable
import           Types
import           Network.OAuth.OAuth2

import           URI.ByteString
import           URI.ByteString.QQ

import           Keys
import           Data.Text.Lazy                 ( Text )
import Utils


data Github = Github deriving (Show,Generic)

instance Hashable Github

instance IDP Github

instance HasLabel Github

instance HasTokenReq Github where
  tokenReq _ mgr = fetchAccessToken mgr githubKey

instance HasTokenRefreshReq Github where
  tokenRefreshReq _ mgr = refreshAccessToken mgr githubKey

instance HasUserReq Github where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Github where
    authUri _ = createCodeUri githubKey [("state","Github.test-state-123")]

instance FromJSON GithubUser where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

data GithubUser = GithubUser { name :: Text
    ,id :: Integer
} deriving (Show, Generic)

userInfoUri :: URI
userInfoUri = [uri|https://api.github.com/user|]

toLoginUser :: GithubUser -> LoginUser
toLoginUser guser = LoginUser { loginUserName = name guser }
