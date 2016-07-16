{-# LANGUAGE OverloadedStrings #-}

module Response
  ( RouteResponse(..) )
where

import Protolude
import Data.Aeson


data RouteRoutes = RouteRoutes
  { routeRouteDistance :: Float
  , routeRouteDuration :: Float }
  deriving (Show)

instance FromJSON RouteRoutes where
  parseJSON (Object v) = RouteRoutes     <$>
                         v .: "distance" <*>
                         v .: "duration"
  parseJSON _          = mzero


data RouteResponse = RouteResponse
  { code     :: Text
  , message  :: Maybe Text
  , routes   :: Maybe [RouteRoutes] }
  deriving (Show)

instance FromJSON RouteResponse where
  parseJSON (Object v) = RouteResponse   <$>
                         v .:  "code"    <*>
                         v .:? "message" <*>
                         v .:? "routes"
  parseJSON _          = mzero
