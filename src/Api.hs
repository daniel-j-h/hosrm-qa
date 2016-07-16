{-# LANGUAGE OverloadedStrings, DeriveGeneric, DataKinds, TypeOperators #-}

module Api
  ( RouteResponse(..)
  , runRoute
  , runExceptT
  , explainError )
where

import Protolude
import Data.Aeson
import Servant.API
import Servant.Client
import Network.HTTP.Client (Manager)
import qualified Data.Text as T


data RouteResponse = RouteResponse
  { routeResponseCode :: Text } deriving (Show, Eq, Generic)

instance FromJSON RouteResponse


type RouteAPI = "driving" :> Capture "query" Text :> Get '[JSON] RouteResponse

api :: Proxy RouteAPI
api = Proxy

routeAPI :: Text -> Manager -> BaseUrl -> ExceptT ServantError IO RouteResponse
routeAPI = client api


explainError ::  ServantError -> Text
explainError (FailureResponse _ _ _)        = "Response Failure" 
explainError (DecodeFailure msg _ _)        = "Decode Failure" <> T.pack msg
explainError (UnsupportedContentType _ _)   = "Unsupported Content Type"
explainError (InvalidContentTypeHeader _ _) = "Invalid Content Type Header"
explainError (ConnectionError _)            = "Connection Error"


runRoute :: Manager -> Text -> Int -> ExceptT ServantError IO RouteResponse
runRoute manager host port = routeAPI coordinates manager baseurl
  where
    coordinates = "-3.279966,51.406314;-3.281205,51.407274"  -- dummy
    baseurl = BaseUrl Http (T.unpack host) port driving
    driving = "route/v1/driving/"
