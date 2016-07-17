{-# LANGUAGE OverloadedStrings, EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

-- For sum types: derivePersistField has to live in own module due to GHC's Stage Restriction
--   https://github.com/yesodweb/persistent/wiki/Persistent-entity-syntax#sum-types
--   https://mail.haskell.org/pipermail/beginners/2013-January/011218.html
module Enum where

import Protolude
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
