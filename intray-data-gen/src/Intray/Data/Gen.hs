{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Data.Gen where

import Import

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID ()
import qualified Data.Text as T

import Intray.Data

instance GenUnchecked ItemUUID

instance GenValid ItemUUID

instance GenUnchecked AccountUUID

instance GenValid AccountUUID

instance GenUnchecked ItemType

instance GenValid ItemType

instance GenUnchecked IntrayItem

instance GenValid IntrayItem where
    genValid =
        IntrayItem <$> genValid <*> genValid <*> genValid <*> genValid <*>
        genValid <*>
        genValid

instance GenUnchecked Username

instance GenValid Username where
    genValid = do
        username <- parseUsername <$> textGen
        case username of
            Just name -> pure name
            Nothing -> genValid
      where
        textGen =
            T.pack <$>
            ((:) <$> charGen <*>
             ((:) <$> charGen <*> ((:) <$> charGen <*> genListOf charGen)))
        charGen = genValid `suchThat` validUsernameChar

instance GenUnchecked User

instance GenUnchecked HashedPassword

instance GenUnchecked Permission

instance GenValid Permission

instance GenUnchecked AccessKeySecret

instance GenValid AccessKeySecret
