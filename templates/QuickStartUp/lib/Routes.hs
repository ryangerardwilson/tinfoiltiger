{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Routes (appRoutes) where

import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (embedFile)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Database.Redis as R
import qualified Env
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import qualified TinFoilTiger.RouteHandlers.Auth as Auth
import qualified TinFoilTiger.Utils.MonadManager as MonadManager
import qualified TinFoilTiger.Utils.Redis as RedisUtils
import TinFoilTiger.Utils.Renderer (render_template, render_template_substitutions)
import Web.Scotty (ScottyM, finish, formParam, get, html, liftIO, middleware, notFound, post, redirect, setHeader)

-- | Define your application routes.
-- preRenderedTemplates is passed in from Main.
appRoutes :: R.Connection -> Map.Map FilePath TL.Text -> ScottyM ()
appRoutes conn preRenderedTemplates = do
  get "/" $ do
    case Map.lookup "index.html" preRenderedTemplates of
      Just tpl -> do
        let rendered = render_template_substitutions tpl [("APP_NAME", TL.pack Env.appName)]
        setHeader "Cache-Control" "public, max-age=3600"
        html rendered
      Nothing -> html "Template not found."

  post "/insert" $ do
    model <- formParam "model"
    field1 <- formParam "field1"
    field2 <- formParam "field2"
    let fields = [("field1", field1), ("field2", field2)]
    result <- liftIO $ RedisUtils.insertRecord conn model fields
    case result of
      Left err -> html $ "Error inserting record: " <> TL.pack (show err)
      Right recId -> html $ "Record inserted with id: " <> TL.pack recId

  get "/login" $ Auth.loginGetHandler conn preRenderedTemplates
  post "/login" $ Auth.loginPostHandler conn
  get "/signup" $ Auth.signupGetHandler conn preRenderedTemplates
  post "/signup" $ Auth.signupPostHandler conn
  get "/logout" $ Auth.logoutHandler conn

  get "/dashboard" $ do
    mUsername <- runMaybeT $ do
      tokenText <- MaybeT Auth.getAuthTokenCookie
      let token = TL.unpack (TL.strip tokenText)
      uid <- MonadManager.convert_action_in_any_monad_to_MaybeT_m_a (liftIO $ RedisUtils.getAuthUid conn token)
      MonadManager.convert_action_in_any_monad_to_MaybeT_m_a (liftIO $ RedisUtils.getUserField conn uid "username")
    case mUsername of
      Nothing -> redirect "/login" >> finish
      Just uname -> do
        baseTemplate <- liftIO $ render_template "dashboard.html"
        let rendered = render_template_substitutions baseTemplate [("USERNAME", TL.pack uname)]
        setHeader "Cache-Control" "private, no-cache, no-store, must-revalidate"
        html rendered

  get "/main.css" $ do
    setHeader "Content-Type" "text/css"
    html $ decodeUtf8 $ BL.fromStrict $(embedFile "resources/main.css")

  middleware $ staticPolicy (addBase "resources/assets")
  notFound $ html "Not Found"
