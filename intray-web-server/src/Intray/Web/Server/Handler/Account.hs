{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Account
  ( getAccountR,
    postAccountDeleteR,
  )
where

import Data.Time
import Import
import Intray.Client
import Intray.Web.Server.Foundation
import Intray.Web.Server.Time
import Yesod
import Yesod.Auth

getAccountR :: Handler Html
getAccountR =
  withLogin $ \t -> do
    mai <- runClientOrDisallow $ clientGetAccountInfo t
    accountInfoWidget <- accountInfoSegment mai
    token <- genToken
    withNavBar $(widgetFile "account")

accountInfoSegment :: Maybe AccountInfo -> Handler Widget
accountInfoSegment Nothing =
  pure
    [whamlet|
        <div .is-negative .message>
            You are not authorised to view account info.
            |]
accountInfoSegment (Just AccountInfo {..}) = do
  now <- liftIO getCurrentTime
  let createdWidget = makeTimestampWidget now accountInfoCreatedTimestamp
  pure $
    mconcat
      [ [whamlet|
          <h3 .has-text-weight-bold>
            Info
          <p> Username: #{usernameText accountInfoUsername}
          <p> Created: ^{createdWidget}
        |],
        mempty -- Already subscribed or no payment necessary
      ]


adminSegment :: Maybe AccountInfo -> Widget
adminSegment Nothing = mempty
adminSegment (Just AccountInfo {..})
  | accountInfoAdmin =
      [whamlet|
          <div .columns .is-centered>
            <div .column .is-half>
              <div .content>
                <h2 .title .is-4>
                  Admin
                <p>
                  This account is an administrator.
                <p>
                  <a .is-success .button href=@{AdminR AdminPanelR}>
                    The Admin Panel|]
  | otherwise = mempty

postAccountDeleteR :: Handler Html
postAccountDeleteR =
  withLogin $ \t -> do
    NoContent <- runClientOrErr $ clientDeleteAccount t
    clearCreds False
    redirect HomeR
