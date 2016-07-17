{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Data.Text as T

import Args (Arguments(..), execParser, argparser)
import Api (runRoute, explainError)
import qualified Response
import qualified Store


checkRoute :: Response.Route -> IO ()
checkRoute route = putStrLn $  "Duration: " <> duration route <> "s, "
                            <> "Distance: " <> distance route <> "m"
  where
    duration = T.pack . show . Response.routeDuration
    distance = T.pack . show . Response.routeDistance


main :: IO ()
main = do
  (Arguments host port store) <- execParser argparser
  putStrLn $  "Endpoint: " <> host <> ":" <> show port <> "\n"
           <> "Store: "    <> store

  manager  <- newManager defaultManagerSettings

  Store.runSqlite store $ do
    _ <- Store.runMigrationSilent Store.migrateAll
    return ()

  response <- runExceptT $ runRoute manager host port 

  case response of
    Left  err   -> putStrLn $ "Error: " <> explainError err
    Right route -> case Response.responseRoutes route of
      Nothing     -> putStrLn $ "Service: " <> Response.responseCode route
      Just routes -> do
        putStrLn $ "Success: " <> Response.responseCode route
        mapM_ checkRoute routes
