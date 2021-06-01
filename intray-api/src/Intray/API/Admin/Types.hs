{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.API.Admin.Types where

import Data.Aeson as JSON
import Import
import Intray.API.Types ()
import Servant.Docs

data AdminStats = AdminStats
  { adminStatsNbAccounts :: !Word,
    adminStatsSubscribedUsers :: !Word,
    adminStatsNbItems :: !Word,
    adminStatsActiveUsers :: !ActiveUsers
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity AdminStats

instance FromJSON AdminStats where
  parseJSON =
    withObject "AdminStats" $ \o ->
      AdminStats <$> o .: "accounts" <*> o .: "subscribed-users" <*> o .: "items"
        <*> o
        .: "active-users"

instance ToJSON AdminStats where
  toJSON AdminStats {..} =
    object
      [ "accounts" .= adminStatsNbAccounts,
        "items" .= adminStatsNbItems,
        "active-users" .= adminStatsActiveUsers,
        "subscribed-users" .= adminStatsSubscribedUsers
      ]

instance ToSample AdminStats

data ActiveUsers = ActiveUsers
  { activeUsersDaily :: !Word,
    activeUsersWeekly :: !Word,
    activeUsersMonthly :: !Word,
    activeUsersYearly :: !Word
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity ActiveUsers

instance FromJSON ActiveUsers where
  parseJSON =
    withObject "ActiveUsers" $ \o ->
      ActiveUsers <$> o .: "daily" <*> o .: "weekly" <*> o .: "monthly" <*> o .: "yearly"

instance ToJSON ActiveUsers where
  toJSON ActiveUsers {..} =
    object
      [ "daily" .= activeUsersDaily,
        "weekly" .= activeUsersWeekly,
        "monthly" .= activeUsersMonthly,
        "yearly" .= activeUsersYearly
      ]

instance ToSample ActiveUsers
