{-# LANGUAGE OverloadedStrings, EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

-- For sum types: derivePersistField has to live in own module due to GHC's Stage Restriction
--   https://github.com/yesodweb/persistent/wiki/Persistent-entity-syntax#sum-types
--   https://mail.haskell.org/pipermail/beginners/2013-January/011218.html
module Enum where

import Protolude
import Data.Aeson
import Database.Persist.TH

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
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)


derivePersistField "ManeuverType"
derivePersistField "ManeuverModifier"


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
