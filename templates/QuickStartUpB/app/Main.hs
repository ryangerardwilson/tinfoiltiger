{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Constants.Env as Env
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (embedFile)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Database.Redis as R
import Language.Haskell.TH.Syntax (Exp (..), runIO)
import qualified Language.Haskell.TH.Syntax as TH
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import qualified RouteHandlers.Auth as Auth
import System.Directory (listDirectory)
import qualified Utils.MonadManager as MonadManager
import qualified Utils.Redis as RedisUtils
import Utils.Renderer (compile_template, render_template, render_template_substitutions)
import Web.Scotty

preRenderedTemplates :: Map.Map FilePath TL.Text
preRenderedTemplates =
  Map.fromList
    $( do
         allFiles <- runIO $ listDirectory "resources"
         let htmlFiles = filter (\fp -> ".html" `isSuffixOf` fp && not ('/' `elem` fp)) allFiles
         preRenderedPairs <-
           mapM
             ( \fp -> do
                 rendered <- compile_template fp
                 [|($(TH.lift fp), rendered)|]
             )
             htmlFiles
         return (ListE preRenderedPairs)
     )

main :: IO ()
main = do
  putStrLn $ "[INFO] Running " ++ Env.appName ++ " on port " ++ show Env.port

  -- Create one Redis connection and select the desired database.
  conn <- R.checkedConnect R.defaultConnectInfo
  _ <- R.runRedis conn $ R.select Env.redisDatabaseNumber

  -- Uncomment for testing: Flush the database.
  -- _ <- R.runRedis conn R.flushdb

  -- Ensure that each model's counter exists.
  mapM_
    ( \modal -> do
        let key = pack $ modal ++ ":next_id"
        existsReply <- R.runRedis conn $ R.exists key
        case existsReply of
          Left err -> error $ "Redis exists error for " ++ modal ++ ": " ++ show err
          Right existsVal ->
            if existsVal
              then putStrLn $ "[DEBUG] Counter for model " ++ modal ++ " already exists."
              else do
                setReply <- R.runRedis conn $ R.set key (pack "0")
                case setReply of
                  Left errSet -> error $ "Redis set error for key " ++ modal ++ ": " ++ show errSet
                  Right _ -> putStrLn $ "[INFO] Initialized counter for model: " ++ modal
    )
    Env.redisDatabaseModals

  -- Start the Scotty web server.
  scotty Env.port $ do
    -- Home route.
    get "/" $ do
      case Map.lookup "index.html" preRenderedTemplates of
        Just tpl -> do
          let rendered = render_template_substitutions tpl [("APP_NAME", TL.pack Env.appName)]
          setHeader "Cache-Control" "public, max-age=3600"
          html rendered
        Nothing -> html "Template not found."

    -- A simple route to insert a record via Redis helper.
    post "/insert" $ do
      model <- formParam "model"
      field1 <- formParam "field1"
      field2 <- formParam "field2"
      let fields = [("field1", field1), ("field2", field2)]
      result <- liftIO $ RedisUtils.insertRecord conn model fields
      case result of
        Left err -> text $ "Error inserting record: " <> TL.pack (show err)
        Right recId -> text $ "Record inserted with id: " <> TL.pack recId

    ------------------------------------------------------------------------------
    -- Authentication Routes defined in Auth.hs.
    get "/login" $ Auth.loginGetHandler conn preRenderedTemplates
    post "/login" $ Auth.loginPostHandler conn
    get "/signup" $ Auth.signupGetHandler conn preRenderedTemplates
    post "/signup" $ Auth.signupPostHandler conn
    get "/logout" $ Auth.logoutHandler conn

    ------------------------------------------------------------------------------
    -- Dashboard route, rewritten using a very specific helper getUserField.
    get "/dashboard" $ do
      mUsername <- runMaybeT $ do
        -- getAuthTokenCookie is already in ActionT IO.
        tokenText <- MaybeT Auth.getAuthTokenCookie
        let token = TL.unpack (TL.strip tokenText)
        -- For Redis actions, lift them from IO to ActionT IO!
        uid <- MonadManager.convert_action_in_any_monad_to_MaybeT_m_a (liftIO $ RedisUtils.getAuthUid conn token)
        MonadManager.convert_action_in_any_monad_to_MaybeT_m_a (liftIO $ RedisUtils.getUserField conn uid "username")
      case mUsername of
        Nothing -> redirect "/login" >> finish
        Just uname -> do
          baseTemplate <- liftIO $ render_template "dashboard.html"
          let rendered = render_template_substitutions baseTemplate [("USERNAME", TL.pack uname)]
          setHeader "Cache-Control" "private, no-cache, no-store, must-revalidate"
          html rendered

    -- A simple route to deliver a CSS file.
    get "/main.css" $ do
      setHeader "Content-Type" "text/css"
      text $ decodeUtf8 $ BL.fromStrict $(embedFile "resources/main.css")

    middleware $ staticPolicy (addBase "resources/assets")
    notFound $ text "Not Found"
