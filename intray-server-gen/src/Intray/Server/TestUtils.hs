{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.TestUtils
  ( withIntrayServer,
    withFreeIntrayServer,
    withPaidIntrayServer,
    withFreeIntrayTestApp,
    withPaidIntrayTestApp,
    withIntrayTestApp,
    setupTestHttpManager,
    runClient,
    runClientOrError,
    randomRegistration,
    withAdmin,
    withValidNewUser,
    withValidNewUserAndData,
    requiresAdmin,
    withNewUser'sAccessKey,
    login,
    failsWithOutPermissions,
    failsWithOutPermission,
    module Servant.Client,
  )
where

import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Data.Cache as Cache
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.UUID.Typed
import Database.Persist.Sqlite
import Import
import Intray.API
import Intray.Client
import Intray.Data.Gen ()
import Intray.Server
import Intray.Server.OptParse.Types
import Intray.Server.Types
import Lens.Micro
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Network.Wai.Handler.Warp (testWithApplication)
import Servant
import Servant.Auth.Server as Auth
import Servant.Client
import Web.Cookie
import Web.Stripe.Plan as Stripe

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

withIntrayServer :: SpecWith ClientEnv -> Spec
withIntrayServer specFunc = do
  context "Paying" $ withPaidIntrayServer 5 specFunc
  context "Free" $ withFreeIntrayServer specFunc

withPaidIntrayServer :: Int -> SpecWith ClientEnv -> Spec
withPaidIntrayServer maxFree specFunc =
  around (withPaidIntrayTestApp maxFree)
    $ modifyMaxShrinks (const 0)
    $ modifyMaxSuccess (`div` 20) specFunc

withFreeIntrayServer :: SpecWith ClientEnv -> Spec
withFreeIntrayServer specFunc =
  around withFreeIntrayTestApp $ modifyMaxShrinks (const 0) $ modifyMaxSuccess (`div` 20) specFunc

testdbFile :: String
testdbFile = "test.db"

withIntrayTestConn :: (ConnectionPool -> IO a) -> IO a
withIntrayTestConn func =
  withSystemTempDir "intray-server" $ \tdir -> do
    dbPath <- resolveFile tdir testdbFile
    let connInfo = mkSqliteConnectionInfo (T.pack (fromAbsFile dbPath)) & walEnabled .~ False
    runNoLoggingT $ do
      p <- createSqlitePoolFromInfo connInfo 1
      void $ runResourceT $ flip runSqlPool p $ runMigrationQuiet migrateAll
      liftIO $ func p

setupTestHttpManager :: IO HTTP.Manager
setupTestHttpManager = HTTP.newManager HTTP.defaultManagerSettings

withPaidIntrayTestApp :: Int -> (ClientEnv -> IO a) -> IO a
withPaidIntrayTestApp maxFree func = do
  now <- getCurrentTime
  let planName = PlanId "dummyPlan"
      dummyPlan =
        Stripe.Plan
          { planInterval = Year,
            planName = "dummy plan",
            planCreated = now,
            planAmount = 1200,
            planCurrency = CHF,
            planId = planName,
            planObject = "plan",
            planLiveMode = False,
            planIntervalCount = Nothing,
            planTrialPeriodDays = Nothing,
            planMetaData = MetaData [],
            planDescription = Nothing
          }
  monetisationEnvPlanCache <- newCache Nothing
  Cache.insert monetisationEnvPlanCache planName dummyPlan
  let monetisationEnvStripeSettings =
        StripeSettings
          { stripeSetPlan = planName,
            stripeSetStripeConfig = error "should not try to access stripe during testing",
            stripeSetPublishableKey = "Example, should not be used."
          }
  let monetisationEnvMaxItemsFree = maxFree
  withIntrayTestApp (Just MonetisationEnv {..}) func

withFreeIntrayTestApp :: (ClientEnv -> IO a) -> IO a
withFreeIntrayTestApp = withIntrayTestApp Nothing

withIntrayTestApp :: Maybe MonetisationEnv -> (ClientEnv -> IO a) -> IO a
withIntrayTestApp menv func =
  withIntrayTestConn $ \pool -> do
    man <- setupTestHttpManager
    signingKey <- Auth.generateKey
    let jwtCfg = defaultJWTSettings signingKey
    let cookieCfg = defaultCookieSettings
    let intrayEnv =
          IntrayServerEnv
            { envHost = "localhost",
              envConnectionPool = pool,
              envCookieSettings = cookieCfg,
              envJWTSettings = jwtCfg,
              envAdmins = [fromJust $ parseUsername "admin"],
              envFreeloaders = [],
              envMonetisation = menv
            }
    let app = serveWithContext intrayAPI (intrayAppContext intrayEnv) (makeIntrayServer intrayEnv)
    testWithApplication (pure app) $ \port ->
      func $ ClientEnv man (BaseUrl Http "127.0.0.1" port "") Nothing

runClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient = flip runClientM

runClientOrError :: ClientEnv -> ClientM a -> IO a
runClientOrError cenv func = do
  errOrRes <- runClient cenv func
  case errOrRes of
    Left err -> failure $ show err
    Right res -> pure res

withAdmin :: ClientEnv -> (Token -> IO ()) -> Expectation
withAdmin cenv = withNewUser cenv (Registration (fromJust $ parseUsername "admin") "admin")

withValidNewUser :: ClientEnv -> (Token -> IO ()) -> Expectation
withValidNewUser cenv func = withValidNewUserAndData cenv $ \_ _ -> func

randomRegistration :: IO Registration
randomRegistration = do
  u1 <- nextRandomUUID :: IO (UUID Username) -- Dummy's that are significantly likely to be random enough
  u2 <- nextRandomUUID :: IO (UUID Text)
  pure
    Registration
      { registrationUsername = fromJust $ parseUsername $ uuidText u1,
        registrationPassword = uuidText u2
      }

withValidNewUserAndData :: ClientEnv -> (Username -> Text -> Token -> IO ()) -> Expectation
withValidNewUserAndData cenv func = do
  r <- randomRegistration
  withNewUser cenv r $ func (registrationUsername r) (registrationPassword r)

withNewUser :: ClientEnv -> Registration -> (Token -> IO ()) -> Expectation
withNewUser cenv r func = do
  errOrUUID <- runClient cenv $ clientPostRegister r
  case errOrUUID of
    Left err -> failure $ "Registration should not fail with error: " <> show err
    Right NoContent -> login cenv (registrationUsername r) (registrationPassword r) >>= func

requiresAdmin :: ClientEnv -> (Token -> ClientM a) -> Expectation
requiresAdmin cenv func =
  withValidNewUser cenv $ \token -> do
    errOrStats <- runClient cenv $ func token
    case errOrStats of
      Left err ->
        case err of
          FailureResponse _ resp ->
            HTTP.statusCode (Servant.Client.responseStatusCode resp) `shouldBe` 401
          _ -> failure "Should have got a failure response."
      Right _ -> failure "Should not have been allowed."

withNewUser'sAccessKey :: ClientEnv -> Set Permission -> (Token -> IO ()) -> Expectation
withNewUser'sAccessKey cenv ps func =
  withValidNewUserAndData cenv $ \un _ tok -> do
    uuid <- nextRandomUUID :: IO (UUID AddAccessKey) -- Dummy's that are significantly likely to be random enough
    let aac = AddAccessKey {addAccessKeyName = uuidText uuid, addAccessKeyPermissions = ps}
    errOrAkc <- runClient cenv $ clientPostAddAccessKey tok aac
    case errOrAkc of
      Left err -> failure $ unwords ["Failed to create access key:", show err]
      Right AccessKeyCreated {..} -> do
        t <- login cenv un (accessKeySecretText accessKeyCreatedKey)
        func t

login :: ClientEnv -> Username -> Text -> IO Token
login cenv un pw = do
  let lf = LoginForm {loginFormUsername = un, loginFormPassword = pw}
  Headers NoContent (HCons sessionHeader HNil) <- runClientOrError cenv $ clientPostLogin lf
  case sessionHeader of
    MissingHeader -> failure "Login should return a session header"
    UndecodableHeader _ -> failure "Login should return a decodable session header"
    Header session -> pure $ Token $ setCookieValue $ parseSetCookie $ encodeUtf8 session

failsWithOutPermissions :: ClientEnv -> Set Permission -> (Token -> ClientM a) -> Property
failsWithOutPermissions cenv ps func =
  forAll (genValid `suchThat` (\ps' -> null (ps' `S.intersection` ps))) $ \perms ->
    withNewUser'sAccessKey cenv (perms `S.intersection` userPermissions) $ \t -> do
      res <- runClient cenv $ func t
      case res of
        Left err ->
          case err of
            FailureResponse _ resp ->
              HTTP.statusCode (Servant.Client.responseStatusCode resp) `shouldBe` 401
            _ -> failure "Should have gotten a failure response."
        _ -> failure "Should not have been allowed."

failsWithOutPermission :: ClientEnv -> Permission -> (Token -> ClientM a) -> Property
failsWithOutPermission cenv p = failsWithOutPermissions cenv $ S.singleton p

failure :: String -> IO a
failure s = do
  expectationFailure s
  undefined -- Won't get here anyway
