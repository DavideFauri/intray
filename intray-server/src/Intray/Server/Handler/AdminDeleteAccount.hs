{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.AdminDeleteAccount
    ( serveAdminDeleteAccount
    ) where

import Import

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveAdminDeleteAccount ::
       AuthResult AuthCookie -> AccountUUID -> IntrayHandler NoContent
serveAdminDeleteAccount (Authenticated AuthCookie {..}) uuid =
    withAdminCreds authCookieUserUuid $ do
        deleteAccountFully uuid
        pure NoContent
serveAdminDeleteAccount _ _ = throwAll err401
