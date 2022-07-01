{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Protected.Account
  ( IntrayProtectedAccountAPI,
    IntrayProtectedAccountSite (..),
    AuthCookie (..),
    AccountInfo (..),
    GetAccountInfo,
    DeleteAccount,
  )
where

import Import
import Intray.API.Protected.Account.Types
import Intray.API.Types
import Servant.API
import Servant.API.Generic

type IntrayProtectedAccountAPI = ToServantApi IntrayProtectedAccountSite

data IntrayProtectedAccountSite route = IntrayProtectedAccountSite
  { getAccountInfo :: !(route :- GetAccountInfo),
    postChangePassphrase :: !(route :- PostChangePassphrase),
    deleteAccount :: !(route :- DeleteAccount)
  }
  deriving (Generic)

type GetAccountInfo =
  ProtectAPI
    :> Get '[JSON] AccountInfo

type PostChangePassphrase =
  ProtectAPI
    :> "change-passphrase"
    :> ReqBody '[JSON] ChangePassphrase
    :> Verb 'POST 204 '[JSON] NoContent

type DeleteAccount =
  ProtectAPI
    :> Verb 'DELETE 204 '[JSON] NoContent

