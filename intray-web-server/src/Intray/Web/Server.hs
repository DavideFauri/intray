{-# LANGUAGE RecordWildCards #-}

module Intray.Web.Server
  ( intrayWebServer,
  )
where

import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist.Sqlite
import Import
import Intray.Web.Server.Application ()
import Intray.Web.Server.Foundation
import Intray.Web.Server.OptParse
import qualified Network.HTTP.Client.TLS as Http
import Yesod

intrayWebServer :: IO ()
intrayWebServer = do
  settings <- getSettings
  pPrint settings
  runIntrayWebServer settings

runIntrayWebServer :: Settings -> IO ()
runIntrayWebServer Settings {..} =
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= setLogLevel) $
      withSqlitePoolInfo (mkSqliteConnectionInfo $ T.pack setLoginCacheFile) 1 $
        \pool -> do
          man <- liftIO Http.newTlsManager
          let app =
                App
                  { appHttpManager = man,
                    appStatic = myStatic,
                    appAPIBaseUrl = setAPIBaseUrl,
                    appConnectionPool = pool
                  }
          liftIO $ do
            runSqlPool (runMigration migrateLoginCache) pool
            warp setPort app
