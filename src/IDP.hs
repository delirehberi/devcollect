module IDP where

import           Types
import           Session
import           IDP.Github                    as IGithub
import qualified Data.HashMap.Strict           as Map
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text                     as T

initGithubIDP :: CacheStore -> (T.Text, T.Text) -> IO ()
initGithubIDP cache secrets =
  insertIDPData cache $ mkIDPData (IDPApp IGithub.Github) secrets

mkIDPData :: IDPApp -> (T.Text, T.Text) -> IDPData
mkIDPData (IDPApp idp) secrets =
  IDPData (authUri idp secrets) Nothing Nothing (idpLabel idp)
