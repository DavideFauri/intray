{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Public.PostLogin
  ( servePostLogin,
  )
where

import Control.Monad.Except
import Data.Text.Encoding (decodeUtf8)
import Data.Time
import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

servePostLogin :: LoginForm -> IntrayHandler (Headers '[Header "Set-Cookie" Text] NoContent)
servePostLogin LoginForm {..} = do
  me <- runDB $ getBy $ UniqueUsername loginFormUsername
  case me of
    Nothing -> throwError err401
    Just (Entity uid user) ->
      if validatePassword (userHashedPassword user) loginFormPassword
        then do
          admins <- asks envAdmins
          let perms =
                if userUsername user `elem` admins
                  then adminPermissions
                  else userPermissions
          setLoggedIn uid user perms
        else do
          aks <- runDB $ selectList [AccessKeyUser ==. userIdentifier user] [Asc AccessKeyCreatedTimestamp]
          let mli =
                flip map aks $ \(Entity _ AccessKey {..}) -> do
                  submittedKey <- parseAccessKeySecretText loginFormPassword
                  if validatePassword accessKeyHashedKey (accessKeySecretText submittedKey)
                    then Right accessKeyPermissions
                    else Left "invalid password"
          case msum mli of
            Left _ -> throwError err401 -- Excplicitly not using the error here
            Right perms -> setLoggedIn uid user perms
  where
    setLoggedIn uid user perms = do
      let cookie =
            AuthCookie {authCookieUserUUID = userIdentifier user, authCookiePermissions = perms}
      IntrayServerEnv {..} <- ask
      mCookie <- liftIO $ makeSessionCookieBS envCookieSettings envJWTSettings cookie
      case mCookie of
        Nothing -> throwError err401
        Just setCookie -> do
          now <- liftIO getCurrentTime
          runDB $ update uid [UserLastLogin =. Just now]
          return $ addHeader (decodeUtf8 setCookie) NoContent
