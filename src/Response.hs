{-# LANGUAGE OverloadedStrings #-}

module Response
  ( Response(..)
  , Route(..)
  , RouteLeg(..)
  , RouteStep(..)
  , StepManeuver(..)
  , ManeuverType(..)
  , ManeuverModifier(..) )
where

import Protolude
import Data.Aeson

-- Response sum types live in the Enum module, due to GHC's Stage Restriction
import Enum


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
  { routeDistance :: Double
  , routeDuration :: Double
  , routeLegs     :: [RouteLeg] }
  deriving (Show)

instance FromJSON Route where
  parseJSON (Object v) = Route           <$>
                         v .: "distance" <*>
                         v .: "duration" <*>
                         v .: "legs"
  parseJSON _          = mzero


data RouteLeg = RouteLeg
  { routeLegDistance :: Double
  , routeLegDuration :: Double
  , routeLegSteps    :: Maybe [RouteStep] }
  deriving (Show)

instance FromJSON RouteLeg where
  parseJSON (Object v) = RouteLeg         <$>
                         v .:  "distance" <*>
                         v .:  "duration" <*>
                         v .:? "steps"
  parseJSON _          = mzero


data RouteStep = RouteStep
  { routeStepDistance :: Double
  , routeStepDuration :: Double
  , routeStepManeuver :: StepManeuver }
  deriving (Show)

instance FromJSON RouteStep where
  parseJSON (Object v) = RouteStep       <$>
                         v .: "distance" <*>
                         v .: "duration" <*>
                         v .: "maneuver"
  parseJSON _          = mzero


data StepManeuver = StepManeuver
  { stepManeuverType     :: ManeuverType
  , stepManeuverModifier :: Maybe ManeuverModifier }
  deriving (Show)

instance FromJSON StepManeuver where
  parseJSON (Object v) = StepManeuver     <$>
                         v .:  "type"     <*>
                         v .:? "modifier"
  parseJSON _          = mzero
