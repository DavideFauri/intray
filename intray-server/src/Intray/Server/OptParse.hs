{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.OptParse
    ( module Intray.Server.OptParse
    , module Intray.Server.OptParse.Types
    ) where

import Import

import qualified Data.Text as T
import Database.Persist.Sqlite

import System.Environment (getArgs)

import Options.Applicative

import Intray.API
import Intray.Server.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags Configuration = do
    let port = fromMaybe 8000 serveFlagPort
    let connInfo = mkSqliteConnectionInfo $ fromMaybe "intray.db" serveFlagDb
    let connCount = fromMaybe 4 serveFlagConnectionCount
    admins <-
        forM serveFlagAdmins $ \s ->
            case parseUsername $ T.pack s of
                Nothing -> die $ unwords ["Invalid admin username:", s]
                Just u -> pure u
    pure
        ( DispatchServe
              ServeSettings
              { serveSetPort = port
              , serveSetConnectionInfo = connInfo
              , serveSetConnectionCount = connCount
              , serveSetAdmins = admins
              }
        , Settings)

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

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
    description = "Intray server"

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
             (mconcat
                  [ long "port"
                  , value Nothing
                  , metavar "PORT"
                  , help "the port to serve on"
                  ]) <*>
         option
             (Just . T.pack <$> str)
             (mconcat
                  [ long "database"
                  , value Nothing
                  , metavar "DATABASE_CONNECTION_STRING"
                  , help "The sqlite connection string"
                  ]) <*>
         option
             (Just <$> auto)
             (mconcat
                  [ long "connection-count"
                  , value Nothing
                  , metavar "CONNECTION_COUNT"
                  , help "the number of database connections to use"
                  ]) <*>
         many
             (strOption
                  (mconcat
                       [ long "admin"
                       , metavar "USERNAME"
                       , help "An admin to use"
                       ])))
    modifier = fullDesc <> progDesc "Command example."

parseFlags :: Parser Flags
parseFlags = pure Flags
