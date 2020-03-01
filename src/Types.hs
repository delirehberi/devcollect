{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ExistentialQuantification #-}

module Types where
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest
                                               as TR
import qualified Text.Mustache
import qualified Text.Mustache                 as M
import qualified Data.Text.Lazy                as TL
import           Text.Mustache
import           Data.Text.Lazy
import           Data.Maybe
import qualified Data.HashMap.Strict           as Map
import           Control.Concurrent.MVar
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Hashable
import           Network.HTTP.Conduit

type IDPLabel = Text
type CacheStore = MVar (Map.HashMap IDPLabel IDPData)


data IDPData =
    IDPData { codeFlowUri :: Text
            , loginUser :: Maybe LoginUser
            , oauth2Token :: Maybe OAuth2Token
            , idpDisplayLabel :: IDPLabel
            }

class (Hashable a, Show a) => IDP a

class (IDP a) => HasAuthUri a where
    authUri :: a -> Text
class (IDP a) => HasLabel a where
    idpLabel :: a -> IDPLabel
    idpLabel = TL.pack . show
class (IDP a) => HasUserReq a where
    userReq :: a -> Manager -> AccessToken -> IO (Either BSL.ByteString LoginUser)
class (IDP a) => HasTokenReq a where
    tokenReq :: a -> Manager ->ExchangeToken -> IO (OAuth2Result TR.Errors OAuth2Token)
class (IDP a) => HasTokenRefreshReq a where
    tokenRefreshReq :: a -> Manager -> RefreshToken -> IO (OAuth2Result TR.Errors OAuth2Token)

data IDPApp = forall a. (IDP a,
                    HasTokenRefreshReq a,
                    HasTokenReq a,
                    HasUserReq a,
                    HasLabel a,
                    HasAuthUri a) => IDPApp a

newtype LoginUser =
    LoginUser { loginUserName :: Text
    } deriving (Eq,Show)

instance Eq IDPData where
  a == b = idpDisplayLabel a == idpDisplayLabel b

instance Ord IDPData where
  a `compare` b = idpDisplayLabel a `compare` idpDisplayLabel b

instance ToMustache IDPData where
  toMustache t' = M.object
    [ "codeFlowUri" ~> codeFlowUri t'
    , "isLogin" ~> isJust (loginUser t')
    , "user" ~> loginUser t'
    , "name " ~> TL.unpack (idpDisplayLabel t')
    ]

instance ToMustache LoginUser where
  toMustache t' = M.object ["name" ~> loginUserName t']


newtype TemplateData = TemplateData { idpTemplateData :: [IDPData]} deriving (Eq)

instance ToMustache TemplateData where
  toMustache td' = M.object ["idps" ~> idpTemplateData td']
