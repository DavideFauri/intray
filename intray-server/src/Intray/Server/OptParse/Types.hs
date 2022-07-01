{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Database.Persist.Sqlite
import Import
import Intray.API

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagHost :: !(Maybe String),
    flagPort :: !(Maybe Int),
    flagDb :: !(Maybe Text),
    flagAdmins :: ![String],
    flagLogLevel :: Maybe LogLevel,
    flagSigningKeyFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confHost :: !(Maybe String),
    confPort :: !(Maybe Int),
    confDb :: !(Maybe Text),
    confAdmins :: !(Maybe [String]),
    confLogLevel :: !(Maybe LogLevel),
    confSigningKeyFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldOrNull "host" "The host to serve the api-server on" .= confHost
        <*> optionalFieldOrNull "port" "The port to serve the api-server on" .= confPort
        <*> optionalFieldOrNull "database" "The database file" .= confDb
        <*> optionalFieldOrNull "admins" "The list of usernames that will be considered administrators" .= confAdmins
        <*> optionalFieldOrNull "log-level" "The minimal log level for log messages" .= confLogLevel
        <*> optionalFieldOrNull "signing-key-file" "The file to store the JWT signing key in" .= confSigningKeyFile


data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envHost :: !(Maybe String),
    envPort :: !(Maybe Int),
    envDb :: !(Maybe Text),
    envLogLevel :: !(Maybe LogLevel),
    envSigningKeyFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

data Settings = Settings
  { setHost :: !Text,
    setPort :: !Int,
    setLogLevel :: !LogLevel,
    setSigningKeyFile :: !(Path Abs File),
    setConnectionInfo :: !SqliteConnectionInfo,
    setAdmins :: ![Username]
  }
  deriving (Show)
