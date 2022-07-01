{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Admin
  ( getAdminPanelR,
    getAdminAccountR,
    postAdminAccountDeleteR,
  )
where

import Control.Exception (throwIO)
import Data.Time
import Import
import Intray.Client
import Intray.Web.Server.Foundation
import Intray.Web.Server.Time
import Network.HTTP.Types as HTTP
import Text.Time.Pretty
import Yesod

getAdminPanelR :: Handler Html
getAdminPanelR =
  withAdminCreds $ \t -> do
    AdminStats {..} <- runClientOrErr $ clientAdminGetStats t
    let ActiveUsers {..} = adminStatsActiveUsers
    users <- runClientOrErr $ clientAdminGetAccounts t
    now <- liftIO getCurrentTime
    token <- genToken
    withNavBar $(widgetFile "admin/panel")

getAdminAccountR :: Username -> Handler Html
getAdminAccountR username = withAdminCreds $ \t -> withUserViaAdmin t username $ \AccountInfo {..} -> do
  now <- liftIO getCurrentTime
  withNavBar $ do
    token <- genToken
    $(widgetFile "admin/account")

postAdminAccountDeleteR :: Username -> Handler Html
postAdminAccountDeleteR username =
  withAdminCreds $ \t -> do
    NoContent <- runClientOrErr $ clientAdminDeleteAccount t username
    redirect $ AdminR AdminPanelR

withAdminCreds :: (Token -> Handler a) -> Handler a
withAdminCreds func =
  withLogin $ \t -> do
    adminInfo <- runClientOrErr $ clientGetAccountInfo t
    if accountInfoAdmin adminInfo
      then func t
      else notFound

withUserViaAdmin :: Token -> Username -> (AccountInfo -> Handler a) -> Handler a
withUserViaAdmin t username func = do
  errOrAccountInfo <- runClient $ clientAdminGetAccount t username
  case errOrAccountInfo of
    Left err -> case err of
      FailureResponse _ resp ->
        if HTTP.statusCode (responseStatusCode resp) == 404
          then notFound
          else liftIO $ throwIO err
      _ -> liftIO $ throwIO err
    Right accountInfo -> func accountInfo
