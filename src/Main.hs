{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Data.Text as T

import Args (Arguments(..), execParser, argparser)
import Api (runRoute, explainError)
import Response (RouteResponse(..), RouteResponseRoute(..))


checkRoute :: RouteResponseRoute -> IO ()
checkRoute route = putStrLn $  "Duration: " <> duration route <> "s, "
                            <> "Distance: " <> distance route <> "m"
  where
    duration = T.pack . show . routeResponseRouteDuration
    distance = T.pack . show . routeResponseRouteDistance


main :: IO ()
main = do
  (Arguments host port) <- execParser argparser
  putStrLn $ "Endpoint: " <> host <> ":" <> show port

  manager  <- newManager defaultManagerSettings
  response <- runExceptT $ runRoute manager host port 

  case response of
    Left  err   -> putStrLn $ "Error: " <> explainError err
    Right route -> case routeResponseRoutes route of
      Nothing     -> putStrLn $ "Service: " <> routeResponseCode route
      Just routes -> do
        putStrLn $ "Success: " <> routeResponseCode route
        mapM_ checkRoute routes
