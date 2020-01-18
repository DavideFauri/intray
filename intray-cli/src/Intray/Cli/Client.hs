module Intray.Cli.Client where

import Import

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.Client

import Intray.Cli.OptParse

runSingleClientOrErr :: ClientM a -> CliM (Maybe a)
runSingleClientOrErr func = do
  mErrOrRes <- runSingleClient func
  case mErrOrRes of
    Nothing -> pure Nothing
    Just errOrRes ->
      fmap Just $
      case errOrRes of
        Left err -> liftIO $ die $ unlines ["Error while contacting the intray server:", show err]
        Right r -> pure r

runSingleClient :: ClientM a -> CliM (Maybe (Either ServantError a))
runSingleClient func = do
  mburl <- asks setBaseUrl
  case mburl of
    Nothing -> pure Nothing
    Just burl ->
      fmap Just $
      liftIO $ do
        man <- newManager tlsManagerSettings
        let env = ClientEnv man burl Nothing
        runClientM func env
