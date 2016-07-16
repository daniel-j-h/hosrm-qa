{-# LANGUAGE OverloadedStrings #-}

module Response
  ( Response(..)
  , Route(..)
  , RouteLeg(..)
  , RouteStep(..)
  , RouteManeuver(..) )
where

import Protolude
import Data.Aeson


data Response = Response
  { responseCode    :: Text
  , responseMessage :: Maybe Text
  , responseRoutes  :: Maybe [Route] }
  deriving (Show)

instance FromJSON Response where
  parseJSON (Object v) = Response        <$>
                         v .:  "code"    <*>
                         v .:? "message" <*>
                         v .:? "routes"
  parseJSON _          = mzero


data Route = Route
  { routeDistance :: Float
  , routeDuration :: Float
  , routeLeg      :: [RouteLeg] }
  deriving (Show)

instance FromJSON Route where
  parseJSON (Object v) = Route           <$>
                         v .: "distance" <*>
                         v .: "duration" <*>
                         v .: "legs"
  parseJSON _          = mzero


data RouteLeg = RouteLeg
  { routeLegDistance :: Float
  , routeLegDuration :: Float
  , routeLegSteps    :: Maybe [RouteStep] }
  deriving (Show)

instance FromJSON RouteLeg where
  parseJSON (Object v) = RouteLeg         <$>
                         v .:  "distance" <*>
                         v .:  "duration" <*>
                         v .:? "steps"
  parseJSON _          = mzero


data RouteStep = RouteStep
  { routeStepDistance :: Float
  , routeStepDuration :: Float
  , routeStepManeuver :: RouteManeuver }
  deriving (Show)

instance FromJSON RouteStep where
  parseJSON (Object v) = RouteStep       <$>
                         v .: "distance" <*>
                         v .: "duration" <*>
                         v .: "maneuver"
  parseJSON _          = mzero


data RouteManeuver = RouteManeuver
  { routeManeuverType     :: Text
  , routeManeuverModifier :: Maybe Text }
  deriving (Show)

instance FromJSON RouteManeuver where
  parseJSON (Object v) = RouteManeuver    <$>
                         v .:  "type"     <*>
                         v .:? "modifier"
  parseJSON _          = mzero
