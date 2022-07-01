{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Protected.Account.Gen where

import Import
import Intray.API
import Intray.API.Admin.Gen ()
import Intray.Data.Gen ()

instance GenValid AccountInfo where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ChangePassphrase where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
