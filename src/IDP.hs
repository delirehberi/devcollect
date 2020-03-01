{-# LANGUAGE OverloadedStrings #-}
module IDP where

import Types
import Session
import IDP.Github as IGithub
import qualified Data.HashMap.Strict as Map
import Data.Text.Lazy (Text)

initGithubIDP :: CacheStore -> IO ()
initGithubIDP cache = insertIDPData cache $ mkIDPData (IDPApp IGithub.Github)

mkIDPData :: IDPApp -> IDPData
mkIDPData (IDPApp idp) = IDPData "authUri idp" Nothing Nothing (idpLabel idp)