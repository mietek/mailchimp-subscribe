{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ((<$>))
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=), ToJSON, toJSON)
import Data.Text (Text)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Environment (getEnv, getEnvironment)
import Web.Scotty (scotty)

import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as C
import qualified Network.HTTP.Types.Status as H
import qualified Web.Scotty as S


data Cfg = Cfg
    { cfgMailChimpApiKey :: Text
    , cfgMailChimpListId :: Text
    , cfgWebsiteUrl      :: Text
    }
  deriving (Show)


data SubRequest = SubRequest
    { srName         :: Text
    , srEmailAddress :: Text
    }
  deriving (Show)


instance (?cfg :: Cfg) => ToJSON SubRequest where
    toJSON request = J.object
      [ "apikey"          .= cfgMailChimpApiKey ?cfg
      , "id"              .= cfgMailChimpListId ?cfg
      , "merge_vars"      .= J.object ["name"  .= srName         request]
      , "email"           .= J.object ["email" .= srEmailAddress request]
      , "update_existing" .= True
      , "double_optin"    .= True
      , "send_welcome"    .= False
      ]


accept :: S.ActionM ()
accept = do
    S.html "<h1>202 Accepted</h1>"
    S.status H.accepted202


rejectNotAcceptable :: S.ActionM ()
rejectNotAcceptable = do
    S.html "<h1>406 Not Acceptable</h1>"
    S.status H.notAcceptable406


rejectBadRequest :: S.ActionM ()
rejectBadRequest = do
    S.html "<h1>400 Bad Request</h1>"
    S.status H.badRequest400


redirectToWebsite :: (?cfg :: Cfg) => S.ActionM ()
redirectToWebsite =
    S.redirect $ LT.fromStrict $ cfgWebsiteUrl ?cfg


subscribeToList :: (?cfg :: Cfg) => S.ActionM ()
subscribeToList = do
    name         <- S.param "name"
    emailAddress <- S.param "email-address"
    didSub       <- liftIO $ postSubRequest $ SubRequest name emailAddress
    case didSub of
      True  -> accept
      False -> rejectNotAcceptable


postSubRequest :: (?cfg :: Cfg) => SubRequest -> IO Bool
postSubRequest request = do
    manager  <- C.newManager C.tlsManagerSettings
    endpoint <- C.parseUrl "https://us3.api.mailchimp.com/2.0/lists/subscribe"
    let post = endpoint
          { C.method      = "POST"
          , C.requestBody = C.RequestBodyLBS $ J.encode request
          }
    response :: Either C.HttpException () <- try $
      C.withResponse post manager $ const $ return ()
    case response of
      Right _ -> return True
      Left failure -> do
        putStrLn $ "   *** WARNING: Request failed: " ++ show failure
        return False


main :: IO ()
main = do
    mailChimpApiKey <- T.pack <$> getEnv "MAILCHIMP_API_KEY"
    mailChimpListId <- T.pack <$> getEnv "MAILCHIMP_LIST_ID"
    websiteUrl      <- T.pack <$> getEnv "WEBSITE_URL"
    let ?cfg = Cfg
          { cfgMailChimpApiKey = mailChimpApiKey
          , cfgMailChimpListId = mailChimpListId
          , cfgWebsiteUrl      = websiteUrl
          }
    env <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
    scotty port $ do
      S.middleware logStdout
      S.get        "/"          redirectToWebsite
      S.post       "/subscribe" subscribeToList
      S.notFound                rejectBadRequest
