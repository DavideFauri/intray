{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API
  ( module Intray.Data,
    module Intray.API,
    module Intray.API.Admin,
    module Intray.API.Protected,
    module Intray.API.Types,
    module Data.UUID.Typed,
  )
where

import Data.UUID.Typed
import Import
import Intray.API.Admin
import Intray.API.Protected
import Intray.API.Types
import Intray.Data
import Servant.API
import Servant.API.Generic
import Servant.Auth.Docs ()
import Servant.HTML.Blaze

intrayAPI :: Proxy IntrayAPI
intrayAPI = Proxy

type IntrayAPI = ToServantApi IntraySite

data IntraySite route = IntraySite
  { openSite :: !(route :- ToServantApi IntrayOpenSite),
    adminSite :: !(route :- "admin" :> ToServantApi IntrayAdminSite)
  }
  deriving (Generic)

intrayOpenAPI :: Proxy IntrayOpenAPI
intrayOpenAPI = Proxy

type IntrayOpenAPI = ToServantApi IntrayOpenSite

data IntrayOpenSite route = IntrayOpenSite
  { protectedSite :: !(route :- ToServantApi IntrayProtectedSite),
    publicSite :: !(route :- ToServantApi IntrayPublicSite)
  }
  deriving (Generic)

type IntrayPublicAPI = ToServantApi IntrayPublicSite

data IntrayPublicSite route = IntrayPublicSite
  { postRegister :: !(route :- PostRegister),
    postLogin :: !(route :- PostLogin),
    getDocs :: !(route :- GetDocs),
    getPricing :: !(route :- GetPricing)
  }
  deriving (Generic)

-- | The order of the items is not guaranteed to be the same for every call.
type PostRegister = "register" :> ReqBody '[JSON] Registration :> Post '[JSON] NoContent

type PostLogin =
  "login" :> ReqBody '[JSON] LoginForm :> PostNoContent '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)

type GetDocs = Get '[HTML] GetDocsResponse

type GetPricing = "pricing" :> Get '[JSON] (Maybe Pricing)
