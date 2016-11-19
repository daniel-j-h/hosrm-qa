{-# LANGUAGE OverloadedStrings, EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Main where

import Protolude
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Data.Text as T

import Database.Persist.Sqlite (withSqliteConn)
import Database.Persist.Sql (SqlBackend)

import Control.Monad.Logger (runNoLoggingT, NoLoggingT(..))
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Reader (ReaderT(..))

import qualified Api
import qualified Args
import qualified Response
import qualified Store


main :: IO ()
main = do
  command <- Args.execParser Args.commandParser

  case command of
    Args.Fetch fetchOptions -> fetchRoutes fetchOptions
    Args.Check checkOptions -> checkRoutes checkOptions


withDB :: Text -> (SqlBackend -> IO a) -> IO a
withDB store action = runNoLoggingT $ withSqliteConn store $ NoLoggingT <$> action


fetchRoutes :: Args.FetchOptions -> IO ()
fetchRoutes options = do
  let host  = Args.fetchOptionsHost  options
      port  = Args.fetchOptionsPort  options
      store = Args.fetchOptionsStore options

  putStrLn $  "Endpoint: " <> host <> ":" <> (show port) <> "\n"
           <> "Store: "    <> store

  manager <- newManager defaultManagerSettings

  withDB store $ runReaderT (void $ Store.runMigrationSilent Store.migrateAll)


--
--  query <- runExceptT $ Api.runRoute manager host port
--
--  case query of
--    Left  err  -> putStrLn $ "Error: " <> Api.explainError err
--    Right resp -> case Response.responseRoutes resp of
--      Nothing     -> putStrLn $ "Service: " <> Response.responseCode resp
--      Just routes -> do
--        putStrLn $ "Success: " <> Response.responseCode resp
--        mapM_ checkRoute routes
--        mapM_ storeRoute routes
--        where
--          storeRoute route = flip Store.runSqlPool pool $ do
--                               Store.insert $ (Store.toStorableRoute route :: Store.Route)
--          checkRoute route = putStrLn $  "Duration: " <> (T.pack . show . Response.routeDuration) route <> "s, "
--                                      <> "Distance: " <> (T.pack . show . Response.routeDistance) route <> "m"
--


checkRoutes :: Args.CheckOptions -> IO ()
checkRoutes options = do
  let verbose = Args.checkOptionsVerbose options
      store   = Args.checkOptionsStore   options

  putStrLn $  "Verbose: " <> (show verbose) <> "\n"
           <> "Store: "   <> store
