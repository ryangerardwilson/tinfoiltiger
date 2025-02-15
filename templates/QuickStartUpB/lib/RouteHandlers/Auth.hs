{-# LANGUAGE OverloadedStrings #-}

module RouteHandlers.Auth
  ( loginGetHandler,
    loginPostHandler,
    signupGetHandler,
    signupPostHandler,
    logoutHandler,
    getAuthTokenCookie,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import qualified Utils.Redis as RedisUtils
import Utils.Renderer (render_template)
import Web.Scotty

------------------------------------------------------------------------------

-- | Reads the authToken from the Cookie header.
getAuthTokenCookie :: ActionM (Maybe TL.Text)
getAuthTokenCookie = do
  mCookie <- header "Cookie"
  return $ mCookie >>= lookupCookie "authToken"
  where
    lookupCookie key cookies =
      let trim t = TL.strip t
          kvs =
            map
              ( \c ->
                  let (k, v) = TL.break (== '=') c
                   in (trim k, trim (TL.drop 1 v))
              )
              (TL.splitOn ";" cookies)
       in case [v | (k, v) <- kvs, k == key] of
            (v : _) -> Just v
            [] -> Nothing

------------------------------------------------------------------------------
-- GET /login: Render the login page.
loginGetHandler :: RedisUtils.Connection -> Map.Map FilePath TL.Text -> ActionM ()
loginGetHandler _ _templates = do
  page <- liftIO $ render_template "login.html"
  setHeader "Cache-Control" "public, max-age=3600"
  html page

------------------------------------------------------------------------------
-- POST /login: Process the login form.
loginPostHandler :: RedisUtils.Connection -> ActionM ()
loginPostHandler conn = do
  username <- formParam "username"
  password <- formParam "password"
  lookupResult <- liftIO $ RedisUtils.lookupUserIdByUsername conn username
  case lookupResult of
    Left err -> text $ "Error: " <> TL.pack (show err)
    Right Nothing -> html "Invalid username or password."
    Right (Just uid) -> do
      userReply <- liftIO $ RedisUtils.getUserFields conn uid
      case userReply of
        Left err -> text $ "Error: " <> TL.pack (show err)
        Right userFields ->
          case lookup "password" userFields of
            Just storedPassword
              | storedPassword == password -> do
                  token <- liftIO nextRandom
                  let tokenStr = toString token
                      ttl = 3600 -- 1 hour TTL for the token
                  stReply <- liftIO $ RedisUtils.storeAuthToken conn tokenStr uid ttl
                  case stReply of
                    Left err -> text $ "Error storing auth token: " <> TL.pack (show err)
                    Right _ -> do
                      setHeader "Set-Cookie" ("authToken=" <> TL.pack tokenStr <> "; Path=/; HttpOnly")
                      redirect "/dashboard"
            _ -> html "Invalid username or password."

------------------------------------------------------------------------------
-- GET /signup: Render the signup page.
signupGetHandler :: RedisUtils.Connection -> Map.Map FilePath TL.Text -> ActionM ()
signupGetHandler _ _templates = do
  page <- liftIO $ render_template "signup.html"
  setHeader "Cache-Control" "public, max-age=3600"
  html page

------------------------------------------------------------------------------
-- POST /signup: Process the signup form.
signupPostHandler :: RedisUtils.Connection -> ActionM ()
signupPostHandler conn = do
  username <- formParam "username"
  password <- formParam "password"
  lookupResult <- liftIO $ RedisUtils.lookupUserIdByUsername conn username
  case lookupResult of
    Left err -> text $ "Error: " <> TL.pack (show err)
    Right (Just _) -> html "Username already exists."
    Right Nothing -> do
      createResult <- liftIO $ RedisUtils.createUser conn username password
      case createResult of
        Left err -> text $ "Error: " <> TL.pack (show err)
        Right uid -> do
          token <- liftIO nextRandom
          let tokenStr = toString token
              ttl = 3600 -- 1 hour TTL for the token
          stReply <- liftIO $ RedisUtils.storeAuthToken conn tokenStr uid ttl
          case stReply of
            Left err -> text $ "Error storing auth token: " <> TL.pack (show err)
            Right _ -> do
              setHeader "Set-Cookie" ("authToken=" <> TL.pack tokenStr <> "; Path=/; HttpOnly")
              redirect "/dashboard"

------------------------------------------------------------------------------
-- GET /logout: Process logout by expiring the auth token cookie.
logoutHandler :: RedisUtils.Connection -> ActionM ()
logoutHandler _ = do
  setHeader "Set-Cookie" "authToken=deleted; Path=/; Max-Age=0"
  redirect "/login"
