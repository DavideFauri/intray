{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Web.Stripe.Types as Stripe

instance GenValid Registration where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid LoginForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Pricing where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

deriving instance Generic Currency

instance GenValid Stripe.Currency where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Stripe.Amount where
  genValid = Stripe.Amount <$> genValid
  shrinkValid a = Stripe.Amount <$> shrinkValid (Stripe.getAmount a)

instance GenValid Stripe.PlanId where
  genValid = Stripe.PlanId <$> genValid
  shrinkValid (Stripe.PlanId t) = Stripe.PlanId <$> shrinkValid t
