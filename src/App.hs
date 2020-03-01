{-# LANGUAGE OverloadedStrings         #-}

module App
  ( app
  , waiApp
  )
where
import qualified Network.Wai                   as WAI
import           Network.Wai.Handler.Warp       ( run )
import           Session
import           Types
import           IDP

import           Web.Scotty
import           Control.Monad.IO.Class         ( liftIO )
import           Views

app :: Int -> IO ()
app port = putStrLn ("Starting server at " ++ show port) >> waiApp >>= run port

waiApp :: IO WAI.Application
waiApp = do
  cache <- initCacheStore
  initGithubIDP cache
  scottyApp $ do
    get "/" $ indexH cache
    {-get "/oauth2/callback" $ callbackH cache-}




indexH :: CacheStore -> ActionM ()
indexH c = liftIO (allValues c) >>= (render "index")

