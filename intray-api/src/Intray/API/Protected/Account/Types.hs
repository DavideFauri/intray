{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.API.Protected.Account.Types
  ( module Intray.API.Protected.Account.Types,
    module Data.UUID.Typed,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Time
import Data.UUID.Typed
import Import
import Intray.API.Types ()
import Intray.Data

data AccountInfo = AccountInfo
  { accountInfoUUID :: AccountUUID,
    accountInfoUsername :: Username,
    accountInfoCreatedTimestamp :: UTCTime,
    accountInfoLastLogin :: Maybe UTCTime,
    accountInfoAdmin :: Bool,
    accountInfoCount :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AccountInfo)

instance Validity AccountInfo

instance HasCodec AccountInfo where
  codec =
    object "AccountInfo" $
      AccountInfo
        <$> requiredField "uuid" "account uuid" .= accountInfoUUID
        <*> requiredField "username" "account username" .= accountInfoUsername
        <*> requiredField "created" "creation time" .= accountInfoCreatedTimestamp
        <*> requiredField "last-login" "last login time" .= accountInfoLastLogin
        <*> requiredField "admin" "whether the user is an admin" .= accountInfoAdmin
        <*> requiredField "count" "how many items the user has in their intray" .= accountInfoCount

data ChangePassphrase = ChangePassphrase
  { changePassphraseOld :: Text,
    changePassphraseNew :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ChangePassphrase)

instance Validity ChangePassphrase

instance HasCodec ChangePassphrase where
  codec =
    object "ChangePassphrase" $
      ChangePassphrase
        <$> requiredField "old-passphrase" "old passphrase" .= changePassphraseOld
        <*> requiredField "new-passphrase" "new passphrase" .= changePassphraseNew
