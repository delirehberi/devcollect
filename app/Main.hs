{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import System.Environment
import Data.Monoid (mconcat)
import App


main :: IO ()
main = do
    port <- getEnv "PORT"
    app $ read port