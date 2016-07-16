{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Data.Text as T

import Args (Arguments(..), execParser, argparser)
import Api (runRoute, explainError)
import Response (Response(..), Route(..), RouteLeg(..), RouteStep(..), RouteManeuver(..))


checkRoute :: Route -> IO ()
checkRoute route = putStrLn $  "Duration: " <> duration route <> "s, "
                            <> "Distance: " <> distance route <> "m"
  where
    duration = T.pack . show . routeDuration
    distance = T.pack . show . routeDistance


main :: IO ()
main = do
  (Arguments host port) <- execParser argparser
  putStrLn $ "Endpoint: " <> host <> ":" <> show port

  manager  <- newManager defaultManagerSettings
  response <- runExceptT $ runRoute manager host port 

  case response of
    Left  err   -> putStrLn $ "Error: " <> explainError err
    Right route -> case responseRoutes route of
      Nothing     -> putStrLn $ "Service: " <> responseCode route
      Just routes -> do
        putStrLn $ "Success: " <> responseCode route
        mapM_ checkRoute routes
