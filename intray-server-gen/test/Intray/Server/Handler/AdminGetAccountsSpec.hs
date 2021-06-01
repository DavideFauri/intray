{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.Handler.AdminGetAccountsSpec
  ( spec,
  )
where

import Intray.Client
import Intray.Data.Gen ()
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec =
  withIntrayServer $
    describe "AdminGetAccounts" $
      do
        it "fails without PermitAdminGetAccounts" $ \cenv ->
          failsWithOutPermission cenv PermitAdminGetAccounts clientAdminGetAccounts
        it "forbids non-admin users from getting account info" $ \cenv ->
          requiresAdmin cenv clientAdminGetAccounts
        it "only returns valid account info" $ \cenv ->
          withAdmin cenv $ \token -> do
            accountInfos <- runClientOrError cenv $ clientAdminGetAccounts token
            shouldBeValid accountInfos
