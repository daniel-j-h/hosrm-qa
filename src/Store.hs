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
  , toStorableRoute
  , toStorableLeg
  , toStorableStep
  , toStorableStepManeuver
  , toStorableManeuverType
  , toStorableManeuverModifier
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


toStorableRoute :: Response.Route -> Route
toStorableRoute r = Route (Response.routeDistance r)
                          (Response.routeDuration r)
                          (toStorableLeg <$> Response.routeLegs r)

toStorableLeg :: Response.RouteLeg -> Leg
toStorableLeg l = Leg (Response.routeLegDistance l)
                      (Response.routeLegDuration l)
                      (fmap toStorableStep <$> Response.routeLegSteps l)

toStorableStep :: Response.RouteStep -> Step
toStorableStep s = Step (Response.routeStepDistance s)
                        (Response.routeStepDuration s)
                        (toStorableStepManeuver $ Response.routeStepManeuver s)

toStorableStepManeuver :: Response.StepManeuver -> Maneuver
toStorableStepManeuver m = Maneuver (toStorableManeuverType $ Response.stepManeuverType m)
                                    (toStorableManeuverModifier <$> Response.stepManeuverModifier m)

toStorableManeuverType :: ManeuverType -> ManeuverType
toStorableManeuverType = identity

toStorableManeuverModifier :: ManeuverModifier -> ManeuverModifier
toStorableManeuverModifier = identity
