{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Home
  ( getHomeR,
  )
where

import Import
import Intray.Client
import Intray.Web.Server.Foundation
import Yesod

getHomeR :: Handler Html
getHomeR = do
  withNavBar $ do
    setTitle "Intray"
    setDescriptionIdemp "A GTD In-box system"
    $(widgetFile "home")
