{-# LANGUAGE OverloadedStrings #-}

module Args
  ( Command(..)
  , FetchOptions(..)
  , CheckOptions(..)
  , commandParser
  , execParser )
where

import Protolude hiding ((<>))
import Options.Applicative
import qualified Data.Text as T


data Command
  = Fetch FetchOptions
  | Check CheckOptions
  deriving (Show, Eq)

data FetchOptions = FetchOptions
  { fetchOptionsHost  :: Text
  , fetchOptionsPort  :: Int
  , fetchOptionsStore :: Text }
  deriving (Show, Eq)

data CheckOptions = CheckOptions
  { checkOptionsVerbose :: Bool
  , checkOptionsStore   :: Text }
  deriving (Show, Eq)


fetchOptionsParser :: ParserInfo FetchOptions
fetchOptionsParser = info (helper <*> arguments) (fullDesc <> progDesc "Fetch Routes")
  where
    arguments = FetchOptions
      <$> (T.pack <$> strOption (long "host"  <> help "Server Host") <|> pure "router.project-osrm.org")
      <*> (option auto          (long "port"  <> help "Server Port") <|> pure 80)
      <*> (T.pack <$> strOption (long "store" <> help "Store Path")  <|> pure "store.sqlite3")

checkOptionsParser :: ParserInfo CheckOptions
checkOptionsParser = info (helper <*> arguments) (fullDesc <> progDesc "Check Routes")
  where
    arguments = CheckOptions
      <$> (switch               (long "verbose" <> help "More Verbose") <|> pure False)
      <*> (T.pack <$> strOption (long "store"   <> help "Store Path")   <|> pure "store.sqlite3")

commandParser :: ParserInfo Command
commandParser = info (helper <*> arguments) (fullDesc <> progDesc "OSRM QA")
  where
    arguments = subparser $
                  command "fetch" (Fetch <$> fetchOptionsParser) <>
                  command "check" (Check <$> checkOptionsParser)
