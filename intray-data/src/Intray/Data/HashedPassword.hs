{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Data.HashedPassword
  ( passwordHash
  , HashedPassword()
  , validatePassword
  ) where

import Import

import qualified Crypto.BCrypt as BCrypt
import qualified Data.Text.Encoding as TE

import Database.Persist.Sql

newtype HashedPassword =
  HashedPassword ByteString
  deriving (Show, Eq, Read, Generic, PersistField, PersistFieldSql)

instance Validity HashedPassword where
  validate (HashedPassword password) =
    declare "The password uses our chosen hashing policy" $
    BCrypt.hashUsesPolicy hashingpolicy password

hashingpolicy :: BCrypt.HashingPolicy
hashingpolicy = BCrypt.fastBcryptHashingPolicy

passwordHash :: Text -> IO (Maybe HashedPassword)
passwordHash =
  fmap (fmap HashedPassword) . BCrypt.hashPasswordUsingPolicy hashingpolicy . TE.encodeUtf8

validatePassword :: HashedPassword -> ByteString -> Bool
validatePassword (HashedPassword hp) = BCrypt.validatePassword hp
