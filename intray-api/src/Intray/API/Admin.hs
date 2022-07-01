{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Admin
  ( module Intray.API.Admin,
    module Intray.API.Protected.Account.Types,
    module Intray.API.Admin.Types,
  )
where

import Import
import Intray.API.Admin.Types
import Intray.API.Protected.Account.Types
import Intray.API.Types
import Servant.API
import Servant.API.Generic

type IntrayAdminAPI = ToServantApi IntrayAdminSite

data IntrayAdminSite route = IntrayAdminSite
  { adminGetStats :: !(route :- AdminGetStats),
    adminDeleteAccount :: !(route :- AdminDeleteAccount),
    adminGetAccount :: !(route :- AdminGetAccount),
    adminGetAccounts :: !(route :- AdminGetAccounts)
  }
  deriving (Generic)

type AdminGetStats =
  ProtectAPI
    :> "stats"
    :> Get '[JSON] AdminStats

type AdminDeleteAccount =
  ProtectAPI
    :> "account"
    :> Capture "username" Username
    :> Verb 'DELETE 204 '[JSON] NoContent

type AdminGetAccount =
  ProtectAPI
    :> "account"
    :> Capture "username" Username
    :> Get '[JSON] AccountInfo

type AdminGetAccounts =
  ProtectAPI
    :> "accounts"
    :> Get '[JSON] [AccountInfo]

