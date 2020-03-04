{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Web.Scotty
import           System.Environment
import           Data.Monoid                    ( mconcat )
import           App
import qualified Data.Text                     as T

main :: IO ()
main = do
  githubClientKey    <- getEnv "CLIENT_KEY"
  githubClientSecret <- getEnv "CLIENT_SECRET"
  port               <- getEnv "PORT"
  app (read port) (T.pack githubClientKey) (T.pack githubClientSecret)
