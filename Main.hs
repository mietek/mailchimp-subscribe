{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative ((<$>))
import Control.Lens ((^?))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=), ToJSON, toJSON)
import Data.Aeson.Lens (key)
import Data.Reflection (Given, give, given)
import Data.Text (Text)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Environment (getEnv, getEnvironment)
import Web.Scotty (scotty)

import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Network.HTTP.Types.Status as H
import qualified Network.Wreq as C
import qualified Web.Scotty as S


accept :: S.ActionM ()
accept = do
    S.html "<h1>202 Accepted</h1>"
    S.status H.accepted202


rejectBadRequest :: S.ActionM ()
rejectBadRequest = do
    S.html "<h1>400 Bad Request</h1>"
    S.status H.badRequest400


rejectNotAcceptable :: S.ActionM ()
rejectNotAcceptable = do
    S.html "<h1>406 Not Acceptable</h1>"
    S.status H.notAcceptable406


data Cfg = Cfg
    { apiKey            :: Text
    , defListId         :: Maybe Text
    , defEmailType      :: Maybe Text
    , defDoubleOptIn    :: Maybe Bool
    , defUpdateExisting :: Maybe Bool
    , defSendWelcome    :: Maybe Bool
    }
  deriving (Show)


defParam :: (Given Cfg, S.Parsable a) => LT.Text -> (Cfg -> Maybe a) -> S.ActionM a
defParam name def =
    S.rescue (S.param name) $ \err ->
      maybe (S.raise err) return $ def given


maybeDefParam :: (Given Cfg, S.Parsable a) => LT.Text -> (Cfg -> Maybe a) -> S.ActionM (Maybe a)
maybeDefParam name def =
    S.rescue (Just <$> S.param name) $ const $
      return $ def given


data SubscribeReq = SubscribeReq
    { reqName           :: Text
    , reqEmailAddress   :: Text
    , reqListId         :: Text
    , reqEmailType      :: Maybe Text
    , reqDoubleOptIn    :: Maybe Bool
    , reqUpdateExisting :: Maybe Bool
    , reqSendWelcome    :: Maybe Bool
    }
  deriving (Show)


instance (Given Cfg) => ToJSON SubscribeReq where
    toJSON req = J.object
      [ "apikey"          .= apiKey given
      , "merge_vars"      .= J.object ["name"  .= reqName req]
      , "email"           .= J.object ["email" .= reqEmailAddress req]
      , "id"              .= reqListId req
      , "email_type"      .= reqEmailType req
      , "double_optin"    .= reqDoubleOptIn req
      , "update_existing" .= reqUpdateExisting req
      , "send_welcome"    .= reqSendWelcome req
      ]


postSubscribeReq :: (Given Cfg) => SubscribeReq -> IO Bool
postSubscribeReq req = do
    res <- C.post "https://us3.api.mailchimp.com/2.0/lists/subscribe" $ toJSON req
    case res ^? C.responseBody . key "error" of
      Nothing  -> return True
      Just err -> do
        putStrLn $ "   *** ERROR: " ++ show err
        return False


subscribeToList :: (Given Cfg) => S.ActionM ()
subscribeToList = do
    name           <- S.param "name"
    emailAddress   <- S.param "email_address"
    listId         <- defParam      "list_id"         defListId
    emailType      <- maybeDefParam "email_type"      defEmailType
    doubleOptIn    <- maybeDefParam "double_optin"    defDoubleOptIn
    updateExisting <- maybeDefParam "update_existing" defUpdateExisting
    sendWelcome    <- maybeDefParam "send_welcome"    defSendWelcome
    let req = SubscribeReq
          { reqName           = name
          , reqEmailAddress   = emailAddress
          , reqListId         = listId
          , reqEmailType      = emailType
          , reqDoubleOptIn    = doubleOptIn
          , reqUpdateExisting = updateExisting
          , reqSendWelcome    = sendWelcome
          }
    done <- liftIO $ postSubscribeReq req
    case done of
      True  -> accept
      False -> rejectNotAcceptable


main :: IO ()
main = do
    apiKey <- T.pack <$> getEnv "MAILCHIMP_API_KEY"
    env    <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
        cfg  = Cfg
          { apiKey
          , defListId         = T.pack   <$> lookup "MAILCHIMP_LIST_ID" env
          , defEmailType      = T.pack   <$> lookup "MAILCHIMP_EMAIL_TYPE" env
          , defDoubleOptIn    = (== "1") <$> lookup "MAILCHIMP_DOUBLE_OPTIN" env
          , defUpdateExisting = (== "1") <$> lookup "MAILCHIMP_UPDATE_EXISTING" env
          , defSendWelcome    = (== "1") <$> lookup "MAILCHIMP_SEND_WELCOME" env
          }
    give cfg $ scotty port $ do
      S.middleware logStdout
      S.post       "/subscribe" subscribeToList
      S.notFound   rejectBadRequest
