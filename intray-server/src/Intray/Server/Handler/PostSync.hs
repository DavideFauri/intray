{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.PostSync
  ( servePostSync,
  )
where

import Data.Mergeless
import Data.Mergeless.Persistent
import Data.UUID.Typed
import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Item
import Intray.Server.Types

servePostSync ::
  AuthCookie ->
  SyncRequest ClientId ItemUUID (AddedItem TypedItem) ->
  IntrayHandler (SyncResponse ClientId ItemUUID (AddedItem TypedItem))
servePostSync AuthCookie {..} sr = do
  doSync authCookieUserUUID sr

doSync ::
  AccountUUID ->
  SyncRequest ClientId ItemUUID (AddedItem TypedItem) ->
  IntrayHandler (SyncResponse ClientId ItemUUID (AddedItem TypedItem))
doSync userId =
  runDB
    . serverProcessSyncWithCustomIdQuery
      nextRandomUUID
      IntrayItemIdentifier
      [IntrayItemUserId ==. userId]
      makeAdded
      (makeAddedIntrayItem userId)
