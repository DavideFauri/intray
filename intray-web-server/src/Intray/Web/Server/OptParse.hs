{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.OptParse
  ( getInstructions
  , Instructions
  , Dispatch(..)
  , Settings(..)
  , ServeSettings(..)
  ) where

import Import

import qualified Data.Text as T
import System.Environment (getArgs, getEnvironment)
import Text.Read

import Database.Persist.Sqlite

import Options.Applicative

import Intray.API
import Intray.Web.Server.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
  (cmd, flags) <- getArguments
  config <- getConfiguration cmd flags
  env <- getEnv
  combineToInstructions cmd flags config env

combineToInstructions :: Command -> Flags -> Configuration -> Environment -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags Configuration Environment {..} = do
  let port = fromMaybe 8000 $ serveFlagPort `mplus` envPort
  let apiPort = fromMaybe 8001 $ serveFlagAPIPort `mplus` envAPIPort
  let connInfo = mkSqliteConnectionInfo $ fromMaybe "intray.db" serveFlagAPIDB
  when (apiPort == port) $
    die $
    unlines ["Web server port and API port must not be the same.", "They are both: " ++ show port]
  admins <-
    forM serveFlagAPIAdmins $ \s ->
      case parseUsername $ T.pack s of
        Nothing -> die $ unwords ["Invalid admin username:", s]
        Just u -> pure u
  pure
    ( DispatchServe
        ServeSettings
          { serveSetPort = port
          , serveSetPersistLogins = fromMaybe False serveFlagPersistLogins
          , serveSetTracking = serveFlagTracking
          , serveSetVerification = serveFlagVerification
          , serveSetAPIPort = apiPort
          , serveSetAPIConnectionInfo = connInfo
          , serveSetAPIAdmins = admins
          }
    , Settings)

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getEnv :: IO Environment
getEnv = do
  env <- getEnvironment
  let mv k = lookup k env
  pure Environment {envPort = mv "WEB_PORT" >>= readMaybe, envAPIPort = mv "API_PORT" >>= readMaybe}

getArguments :: IO Arguments
getArguments = do
  args <- getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
      ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Intray web server"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "serve" parseCommandServe]

parseCommandServe :: ParserInfo Command
parseCommandServe = info parser modifier
  where
    parser =
      CommandServe <$>
      (ServeFlags <$>
       option
         (Just <$> auto)
         (mconcat [long "port", metavar "PORT", value Nothing, help "the port to serve on"]) <*>
       flag
         Nothing
         (Just True)
         (mconcat
            [ long "persist-logins"
            , help
                "Whether to persist logins accross restarts. This should not be used in production."
            ]) <*>
       option
         (Just . T.pack <$> str)
         (mconcat
            [ long "analytics-tracking-id"
            , value Nothing
            , metavar "TRACKING_ID"
            , help "The google analytics tracking ID"
            ]) <*>
       option
         (Just . T.pack <$> str)
         (mconcat
            [ long "search-console-verification"
            , value Nothing
            , metavar "VERIFICATION_TAG"
            , help "The contents of the google search console verification tag"
            ]) <*>
       option
         (Just <$> auto)
         (mconcat [long "api-port", value Nothing, help "the port to serve the API on"]) <*>
       option
         (Just . T.pack <$> str)
         (mconcat
            [ long "database"
            , value Nothing
            , metavar "DATABASE_CONNECTION_STRING"
            , help "The sqlite connection string"
            ]) <*>
       many (strOption (mconcat [long "admin", metavar "USERNAME", help "An admin to use"])))
    modifier = fullDesc <> progDesc "Serve."

parseFlags :: Parser Flags
parseFlags = pure Flags
