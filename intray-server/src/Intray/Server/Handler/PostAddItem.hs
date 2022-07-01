{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.PostAddItem
  ( servePostAddItem,
  )
where

import Data.Time
import Data.UUID.Typed
import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Item
import Intray.Server.Types

servePostAddItem :: AuthCookie -> TypedItem -> IntrayHandler ItemUUID
servePostAddItem AuthCookie {..} typedItem = do
  now <- liftIO getCurrentTime
  uuid <- liftIO nextRandomUUID
  runDB $ insert_ $ makeIntrayItem authCookieUserUUID uuid now typedItem
  pure uuid
