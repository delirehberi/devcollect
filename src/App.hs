{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module App
  ( app
  , waiApp
  )
where
import qualified Network.Wai                   as WAI
import           Network.Wai.Handler.Warp       ( run )
import           Network.OAuth.OAuth2
import           Network.HTTP.Conduit
import           Session
import           Types
import           IDP
import           Control.Monad.Error.Class
import           Control.Monad
import           Web.Scotty
import           Web.Scotty.Internal.Types
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad
import           Views
import           Utils
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as TL
import           Data.Maybe
import           Data.Bifunctor
import           IDP.Github
import qualified Data.Text                     as T

app :: Int -> T.Text -> T.Text -> IO ()
app port githubClientKey githubClientSecret =
  putStrLn ("Starting server at " ++ show port)
    >>  waiApp githubClientKey githubClientSecret
    >>= run port

waiApp :: T.Text -> T.Text -> IO WAI.Application
waiApp key secret = do
  cache <- initCacheStore
  initGithubIDP cache (key, secret)
  scottyApp $ do
    get "/" $ indexH cache
    get "/oauth2/callback" $ callbackH cache (key, secret)

debug :: Bool
debug = True

indexH :: CacheStore -> ActionM ()
indexH c = liftIO (allValues c) >>= (render "index")

callbackH :: CacheStore -> (T.Text, T.Text) -> ActionM ()
callbackH c secrets = do
  pas <- params
  let codeP  = paramValue "code" pas
  let stateP = paramValue "state" pas
  when (null codeP)  (errorM "CallbackH: no code from callback request")
  when (null stateP) (errorM "CallbackH: no state from callback request")
  fetchTokenAndUser c (head codeP) secrets

errorM :: Text -> ActionM ()
errorM = throwError . ActionError

redirectToHomeM :: ActionM ()
redirectToHomeM = redirect "/"

fetchTokenAndUser :: CacheStore -> TL.Text -> (T.Text, T.Text) -> ActionM ()
fetchTokenAndUser c code secrets = do
  maybeIdpData <- lookIdp c IDP.Github.Github
  when (isNothing maybeIdpData) (errorM "IDP data not found in cache")
  let idpData = fromJust maybeIdpData
  result <- liftIO $ tryFetchUser IDP.Github.Github code secrets
  case result of
    Right luser -> updateIdp c idpData luser >> redirectToHomeM
    Left  err   -> errorM ("FetchTokenAndUser: " `TL.append` err)
 where
  lookIdp c1 idp1 = liftIO $ lookupKey c1 (idpLabel idp1)
  updateIdp c1 oldIdpData luser = liftIO $ insertIDPData c1 (oldIdpData)


tryFetchUser
  :: (HasTokenReq a, HasUserReq a, HasLabel a)
  => a
  -> TL.Text
  -> (T.Text, T.Text)
  -> IO (Either Text LoginUser)

tryFetchUser idp code secrets = do
  mgr   <- newManager tlsManagerSettings
  token <- tokenReq idp mgr secrets (ExchangeToken $ TL.toStrict code)
  when debug (print token)
  case token of
    Right at -> fetchUser idp mgr (accessToken at)
    Left e ->
      return (Left $ TL.pack $ "tryFetchUser: cannot fetch access token.")

fetchUser
  :: (HasUserReq a) => a -> Manager -> AccessToken -> IO (Either Text LoginUser)
fetchUser idp mgr token = do
  re <- userReq idp mgr token
  return (first displayOAuth2Error re) {-TODO-}

displayOAuth2Error :: OAuth2Error Errors -> Text
displayOAuth2Error = TL.pack . show
