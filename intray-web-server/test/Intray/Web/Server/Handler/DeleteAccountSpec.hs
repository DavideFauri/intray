module Intray.Web.Server.Handler.DeleteAccountSpec where

import Intray.Web.Server.Foundation
import Intray.Web.Server.TestUtils
import Network.HTTP.Types
import TestImport
import Yesod.Test

spec :: Spec
spec =
  intrayWebServerSpec
    $ ydescribe "DeleteAccount"
    $ yit "deletes account sucessfully and is then logged out"
    $ withExampleAccountAndLogin_
    $ do
      get AccountR
      statusIs 200
      request $ do
        setMethod methodPost
        setUrl AccountDeleteR
        addTokenFromCookie
      statusIs 303
      loc <- getLocation
      case loc of
        Right r -> liftIO $ r `shouldBe` HomeR
        _ -> liftIO $ expectationFailure $ unwords ["Should have redirected:", show loc]
