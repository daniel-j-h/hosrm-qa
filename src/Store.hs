{-# LANGUAGE OverloadedStrings, EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Store
  ( migrateAll
  , runMigrationSilent
  , createSqlitePool
  , runSqlPool
  , Route(..)
  , Leg(..)
  , Step(..)
  , Maneuver(..)
  , ManeuverType(..)
  , ManeuverModifier(..)
  , ToStorable(..)
  , module Database.Persist )
where

import Protolude
import Database.Persist
import Database.Persist.TH  (share, mkPersist,sqlSettings, mkMigrate, persistLowerCase)
import Database.Persist.Sqlite (runMigrationSilent, createSqlitePool, runSqlPool)

import qualified Response (Route(..), RouteLeg(..), RouteStep(..), StepManeuver(..))
import Enum (ManeuverType(..), ManeuverModifier(..))


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Route
  distance Double
  duration Double
  legs     [Leg]
Leg
  distance Double
  duration Double
  steps    [Step] Maybe
Step
  distance Double
  duration Double
  maneuver Maneuver
Maneuver
  type     ManeuverType
  modifier ManeuverModifier Maybe
|]


class ToStorable a b where
  toStorable :: a -> b

instance ToStorable Response.Route Route where
  toStorable r = Route (Response.routeDistance r)
                       (Response.routeDuration r)
                       (toStorable <$> Response.routeLegs r)

instance ToStorable Response.RouteLeg Leg where
  toStorable l = Leg (Response.routeLegDistance l)
                     (Response.routeLegDuration l)
                     (fmap toStorable <$> Response.routeLegSteps l)

instance ToStorable Response.RouteStep Step where
  toStorable s = Step (Response.routeStepDistance s)
                      (Response.routeStepDuration s)
                      (toStorable $ Response.routeStepManeuver s)

instance ToStorable Response.StepManeuver Maneuver where
  toStorable m = Maneuver (toStorable $   Response.stepManeuverType m)
                          (toStorable <$> Response.stepManeuverModifier m)

instance ToStorable ManeuverType ManeuverType where
  toStorable = identity

instance ToStorable ManeuverModifier ManeuverModifier where
  toStorable = identity
