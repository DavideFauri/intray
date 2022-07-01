module Intray.Server.Types where

import Control.Monad.Logger
import Database.Persist.Sqlite
import Import
import Intray.API
import Servant
import Servant.Auth.Server

type LF = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

data IntrayServerEnv = IntrayServerEnv
  { envHost :: !Text,
    envLogFunc :: !LF,
    envConnectionPool :: !ConnectionPool,
    envCookieSettings :: !CookieSettings,
    envJWTSettings :: !JWTSettings,
    envAdmins :: ![Username]
  }

type IntrayHandler = ReaderT IntrayServerEnv (LoggingT Handler)
