{-# LANGUAGE TypeApplications #-}

module Intray.API.Admin.InstanceSpec
  ( spec,
  )
where

import Intray.API.Admin.Gen ()
import Intray.API.Admin.Types
import Test.Validity.Aeson
import TestImport

spec :: Spec
spec = do
  genValidSpec @AdminStats
  jsonSpecOnValid @AdminStats
