{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Gen
  ( module Intray.API.Gen,
    module Intray.API.Admin.Gen,
    module Intray.API.Protected.Gen,
  )
where

import Import
import Intray.API
import Intray.API.Admin.Gen ()
import Intray.API.Protected.Gen ()
import Intray.Data.Gen ()

instance GenValid Registration where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid LoginForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

