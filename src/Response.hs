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
  { routeDistance :: Double
  , routeDuration :: Double
  , routeLeg      :: [RouteLeg] }
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
  , routeStepManeuver :: RouteManeuver }
  deriving (Show)

instance FromJSON RouteStep where
  parseJSON (Object v) = RouteStep       <$>
                         v .: "distance" <*>
                         v .: "duration" <*>
                         v .: "maneuver"
  parseJSON _          = mzero


data RouteManeuver = RouteManeuver
  { routeManeuverType     :: ManeuverType
  , routeManeuverModifier :: Maybe ManeuverModifier }
  deriving (Show)

instance FromJSON RouteManeuver where
  parseJSON (Object v) = RouteManeuver    <$>
                         v .:  "type"     <*>
                         v .:? "modifier"
  parseJSON _          = mzero


data ManeuverType
  = ManeuverTypeTurn
  | ManeuverTypeNewName
  | ManeuverTypeDepart
  | ManeuverTypeArrive
  | ManeuverTypeMerge
  | ManeuverTypeRamp
  | ManeuverTypeOnRamp
  | ManeuverTypeOffRamp
  | ManeuverTypeFork
  | ManeuverTypeEndOfRoad
  | ManeuverTypeUseLane
  | ManeuverTypeContinue
  | ManeuverTypeRoundabout
  | ManeuverTypeRotary
  | ManeuverTypeRoundaboutTurn
  | ManeuverTypeNotification
  | ManeuverTypeUnknown
  deriving (Show, Eq)

instance FromJSON ManeuverType where
  parseJSON (String "turn")            = return ManeuverTypeTurn
  parseJSON (String "new name")        = return ManeuverTypeNewName
  parseJSON (String "depart")          = return ManeuverTypeDepart
  parseJSON (String "arrive")          = return ManeuverTypeArrive
  parseJSON (String "merge")           = return ManeuverTypeMerge
  parseJSON (String "ramp")            = return ManeuverTypeRamp
  parseJSON (String "on ramp")         = return ManeuverTypeOnRamp
  parseJSON (String "off ramp")        = return ManeuverTypeOffRamp
  parseJSON (String "fork")            = return ManeuverTypeFork
  parseJSON (String "end of road")     = return ManeuverTypeEndOfRoad
  parseJSON (String "use lane")        = return ManeuverTypeUseLane
  parseJSON (String "continue")        = return ManeuverTypeContinue
  parseJSON (String "roundabout")      = return ManeuverTypeRoundabout
  parseJSON (String "rotary")          = return ManeuverTypeRotary
  parseJSON (String "roundabout turn") = return ManeuverTypeRoundaboutTurn
  parseJSON (String "notification")    = return ManeuverTypeNotification
  parseJSON (String _)                 = return ManeuverTypeUnknown
  parseJSON _                          = mzero


data ManeuverModifier
  = ManeuverModifierUturn
  | ManeuverModifierSharpRight
  | ManeuverModifierRight
  | ManeuverModifierSlightRight
  | ManeuverModifierStraight
  | ManeuverModifierSlightLeft
  | ManeuverModifierLeft
  | ManeuverModifierSharpLeft
  | ManeuverModifierUnknown
  deriving (Show, Eq)

instance FromJSON ManeuverModifier where
  parseJSON (String "uturn")        = return ManeuverModifierUturn
  parseJSON (String "sharp right")  = return ManeuverModifierSharpRight
  parseJSON (String "right")        = return ManeuverModifierRight
  parseJSON (String "slight right") = return ManeuverModifierSlightRight
  parseJSON (String "straight")     = return ManeuverModifierStraight
  parseJSON (String "slight left")  = return ManeuverModifierSlightLeft
  parseJSON (String "left")         = return ManeuverModifierLeft
  parseJSON (String "sharp left")   = return ManeuverModifierSharpLeft
  parseJSON (String _)              = return ManeuverModifierUnknown
  parseJSON _                       = mzero
