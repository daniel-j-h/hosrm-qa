{-# LANGUAGE OverloadedStrings #-}

module Args
  ( Arguments(..)
  , argparser
  , execParser )
where

import Protolude
import Options.Applicative
import qualified Data.Text as T


data Arguments = Arguments
  { argumentsHost :: Text
  , argumentsPort :: Int }
  deriving (Show, Eq)


arguments :: Parser Arguments
arguments = Arguments
  <$> (T.pack <$> strOption (long "host" <> help "Server Host") <|> pure "router.project-osrm.org")
  <*> (option auto          (long "port" <> help "Server Port") <|> pure 80)


argparser :: ParserInfo Arguments
argparser = info (helper <*> arguments) (fullDesc <> progDesc "OSRM QA")
