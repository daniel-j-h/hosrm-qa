{-# LANGUAGE OverloadedStrings, EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Main where

import Protolude
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Data.Text as T

import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)

import Args (Arguments(..), execParser, argparser)
import Api (runRoute, explainError)
import qualified Response
import qualified Store


main :: IO ()
main = do
  (Arguments host port store) <- execParser argparser
  putStrLn $  "Endpoint: " <> host <> ":" <> show port <> "\n"
           <> "Store: "    <> store

  manager  <- newManager defaultManagerSettings

  pool <- runResourceT . runNoLoggingT $ Store.createSqlitePool store 1

  flip Store.runSqlPool pool $ do
    _ <- Store.runMigrationSilent Store.migrateAll
    return ()

  query <- runExceptT $ runRoute manager host port

  case query of
    Left  err  -> putStrLn $ "Error: " <> explainError err
    Right resp -> case Response.responseRoutes resp of
      Nothing     -> putStrLn $ "Service: " <> Response.responseCode resp
      Just routes -> do
        putStrLn $ "Success: " <> Response.responseCode resp
        mapM_ checkRoute routes
        mapM_ storeRoute routes
        where
          storeRoute route = flip Store.runSqlPool pool $ do
                               Store.insert $ (Store.toStorable route :: Store.Route)
          checkRoute route = putStrLn $  "Duration: " <> (T.pack . show . Response.routeDuration) route <> "s, "
                                      <> "Distance: " <> (T.pack . show . Response.routeDistance) route <> "m"
