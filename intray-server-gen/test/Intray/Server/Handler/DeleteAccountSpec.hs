{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.Handler.DeleteAccountSpec
  ( spec,
  )
where

import Intray.Client
import Intray.Data.Gen ()
import Intray.Server.TestUtils
import qualified Network.HTTP.Types as Http
import TestImport

spec :: Spec
spec =
  withIntrayServer $
    describe "DeleteAccount" $
      do
        it "fails without PermitDeleteAccount" $ \cenv ->
          failsWithOutPermission cenv PermitDeleteAccount clientDeleteAccount
        it "deletes an account" $ \cenv ->
          withValidNewUser cenv $ \token -> do
            NoContent <- runClientOrError cenv $ clientDeleteAccount token
            errOrAccountInfo <- runClient cenv $ clientGetAccountInfo token
            case errOrAccountInfo of
              Left err ->
                case err of
                  FailureResponse _ resp -> Http.statusCode (responseStatusCode resp) `shouldBe` 404
                  _ -> expectationFailure "Should have gotten the right error."
              Right ai ->
                expectationFailure $
                  unlines ["Should not have found account info, got this instead:", show ai]
