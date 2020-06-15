{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Backend.Handler.Auth
  ( handleLogin
  , handleLoginCallback
  , checkAuth
  ) where

import           Backend.App.Config
import           Backend.Handler.Config
import           Backend.Import                   hiding ((^.))
import qualified Backend.Service.HttpReq          as Req
import           Backend.Types.User
import           Backend.Util                     (aesonOptions)
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Time
import           Crypto.JOSE
import           Crypto.JWT
import           Data.Aeson
import           Data.ByteString.Base64.URL       (decodeBase64)
import qualified Data.ByteString.Char8            as Char8 (split, unpack)
import           Data.FileEmbed
import           Network.HTTP.Types.Header        hiding (Header)
import           Network.URL                      (exportParams)
import qualified Network.Wai                      as Wai
import qualified RIO.ByteString.Lazy              as LBS
import qualified RIO.HashMap                      as HM
import qualified RIO.Text                         as T
import           Servant
import           Servant.Server.Experimental.Auth
import           Web.Cookie                       (parseCookies)

handleLogin :: MonadConfig m
            => Maybe Text
            -> ExceptT ServerError m (Headers '[Header "Location" Text] NoContent)
handleLogin nextMaybe = do
  basePath <- lift getBasePath
  clientId <- lift getClientId
  redirectUri <- lift getRedirectUri
  let next = fromMaybe basePath nextMaybe
      paramStr = exportParams
            [ ("client_id", T.unpack clientId)
            , ("redirect_uri", T.unpack redirectUri)
            , ("response_type", "code")
            , ("scope", "email")
            , ("access_type", "offline")
            , ("state", T.unpack next)
            ]
      redirectDest = "https://accounts.google.com/o/oauth2/v2/auth?" <> T.pack paramStr
  pure $ addHeader redirectDest NoContent

getRedirectUri :: MonadBaseUrl m
               => m Text
getRedirectUri = do
    baseUrl <- getBaseUrl
    pure $ baseUrl <> "/api/auth/login/callback"

getClientId :: MonadConfig m => m Text
getClientId = do
    config <- getConfig
    pure $ config ^. (config_auth . authConfig_clientId)

getClientSecret :: MonadConfig m => m Text
getClientSecret = do
    config <- getConfig
    pure $ config ^. (config_auth . authConfig_clientSecret)

handleLoginCallback :: (MonadConfig m, Req.MonadHttpReq m, MonadLogger m, MonadRandom m)
                    => Maybe Text
                    -> Maybe Text
                    -> Maybe Text
                    -> ExceptT ServerError m (Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent)
handleLoginCallback stateMaybe codeMaybe errorMaybe = do
  case errorMaybe of
    Just errMsg -> throwError $ err403 { errBody = LBS.fromStrict (T.encodeUtf8 errMsg) }
    Nothing -> case codeMaybe of
      Nothing -> throwError $ err403 { errBody = "no code provided" }
      Just code -> do
        redirectUri <- lift getRedirectUri
        clientId <- lift getClientId
        clientSecret <- lift getClientSecret
        resp :: Req.JsonResponse OAuthTokenResponse <- lift . Req.runReq Req.defaultHttpConfig $ Req.req Req.POST
          (Req.https "oauth2.googleapis.com" Req./: "token")
          Req.NoReqBody Req.jsonResponse
          ( "client_id" Req.=: clientId
          <> "client_secret" Req.=: clientSecret
          <> "code" Req.=: code
          <> "grant_type" Req.=: ("authorization_code" :: Text)
          <> "redirect_uri" Req.=: redirectUri
          )
        if Req.responseStatusCode resp /= 200
          then throwError err403 { errBody = "fail to fetch oauth token" }
          else do
            let (OAuthTokenResponse idToken) = Req.responseBody resp
            case parseOAuthIdToken idToken of
              Left errMsg -> do
                $logWarn errMsg
                throwError err403 { errBody = "fail to parse oauth id_token" }
              Right email -> do
                $logInfo $ "logged in as: " <> email
                basePath <- lift getBasePath
                let next = fromMaybe basePath stateMaybe
                cookie <- lift $ signCookie (Session email)
                pure $ next `addHeader` (cookie `addHeader` NoContent)

data OAuthTokenResponse = OAuthTokenResponse
    { id_token :: Text
    }
    deriving (Show, Generic)

instance FromJSON OAuthTokenResponse

parseOAuthIdToken :: Text -- ^google oauth id_token
                  -> Either Text Text -- ^email
parseOAuthIdToken idToken =
    case parts_ of
      (_:payloadRaw:_) -> case decodeBase64 (T.encodeUtf8 payloadRaw) of
        Right payloadJson -> case eitherDecodeStrict' payloadJson of
          Right payload -> Right $ _oAuthIdToken_email payload
          Left errMsg -> Left $ "fail to decode oauth id_token payload as json, reason: " <> T.pack errMsg
        Left errMsg -> Left $ "fail to decode oauth id_token payload as base64, reason: " <> errMsg
      _ -> Left $ "oauth id_token has invalid number of parts: " <> idToken
  where
    parts_ = T.split (== '.') idToken

newtype OAuthIdToken = OAuthIdToken
  { _oAuthIdToken_email :: Text
  } deriving (Show, Generic)

instance FromJSON OAuthIdToken where
  parseJSON = genericParseJSON aesonOptions

type instance AuthServerData (AuthProtect "cookie-auth") = Session

signCookie :: (MonadRandom m, MonadLogger m, MonadBaseUrl m) => Session -> m Text
signCookie s = do
    host <- getHost
    let claimsSet = addClaim "email" (toJSON (_session_email s)) $ emptyClaimsSet & claimAud .~ (Just (Audience $ catMaybes [preview stringOrUri host]))
    r <- runExceptT (signClaims cookieSigningKey signHeader claimsSet)
    case r of
      Left (err :: JWTError) -> do
        $logError $ "fail to sign cookie jwt, reason: " <> (T.pack . show $ err)
        pure ""
      Right x ->
        let value = T.pack . Char8.unpack . LBS.toStrict . LBS.intercalate "." $ toCompact x
         in pure $ "yapi-runner=" <>  value <> ";Max-Age=2592000;Path=/"
  where
    signHeader = newJWSHeader ((), RS512)

cookieSigningKey :: JWK
cookieSigningKey = case eitherDecodeStrict' $(embedFile "conf/jwk.json") of
        Left err -> error err
        Right k  -> k

checkAuth :: (MonadBaseUrl m, MonadLogger m, MonadTime m) => Wai.Request -> ExceptT ServerError m Session
checkAuth request = do
    host <- lift getHost
    let expectedAud = preview stringOrUri host
    case expectedAud of
      Nothing -> do
        $logError $ "fail to generate expected aud"
        throwError err500
      Just aud -> do
        (claimsSetEither :: Either JWTError ClaimsSet) <- runExceptT $ do
          token <- fromCompact . fmap LBS.fromStrict . Char8.split '.' $ tokenCompact
          verifyClaims (defaultJWTValidationSettings (== aud)) cookieSigningKey token
        case claimsSetEither of
          Left err -> do
            $logWarn $ "fail to verify jwt, reason: " <> T.pack (show err) <> ", token: " <> T.pack (Char8.unpack tokenCompact)
            throwError err401 { errBody = "invalid jwt in cookie" }
          Right claimsSet ->
            case HM.lookupDefault "" "email" (claimsSet ^. unregisteredClaims) of
              String email -> do
                if T.null email
                then do
                  let errMsg = "cookie contains empty email"
                  $logWarn errMsg
                  throwError err401 { errBody = LBS.fromStrict (T.encodeUtf8 errMsg) }
                else pure . Session $ email
              _ -> do
                $logWarn "cookie contain invalid type email"
                throwError err401
  where
    headers = Wai.requestHeaders request
    cookie = fromMaybe "" (lookup hCookie headers)
    tokenCompact = fromMaybe "" $ lookup "yapi-runner" $ parseCookies cookie

