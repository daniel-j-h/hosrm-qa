{-# LANGUAGE OverloadedStrings #-}

module Response
  ( RouteResponse(..)
  , RouteResponseRoute(..) )
where

import Protolude
import Data.Aeson


data RouteResponseRoute = RouteResponseRoute
  { routeResponseRouteDistance :: Float
  , routeResponseRouteDuration :: Float }
  deriving (Show)

instance FromJSON RouteResponseRoute where
  parseJSON (Object v) = RouteResponseRoute <$>
                         v .: "distance"    <*>
                         v .: "duration"
  parseJSON _          = mzero


data RouteResponse = RouteResponse
  { routeResponseCode     :: Text
  , routeResponseMessage  :: Maybe Text
  , routeResponseRoutes   :: Maybe [RouteResponseRoute] }
  deriving (Show)

instance FromJSON RouteResponse where
  parseJSON (Object v) = RouteResponse   <$>
                         v .:  "code"    <*>
                         v .:? "message" <*>
                         v .:? "routes"
  parseJSON _          = mzero
