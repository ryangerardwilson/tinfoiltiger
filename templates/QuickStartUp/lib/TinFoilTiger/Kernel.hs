{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TinFoilTiger.Kernel (startApp) where

import qualified Data.ByteString.Char8 as BS
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Language.Haskell.TH.Syntax (runIO)
import qualified Language.Haskell.TH.Syntax as TH
import System.Directory (listDirectory)
import TinFoilTiger.Utils.Renderer (compile_template)
import Web.Scotty (ScottyM, scotty)

-- | preRenderedTemplates compiles html templates from the "resources" directory
-- at compile time. (If you later wish to adapt this to use the public directory
-- from your environment at runtime, you’ll need to change this implementation.)
preRenderedTemplates :: Map.Map FilePath TL.Text
preRenderedTemplates =
  Map.fromList
    $( do
         files <- runIO $ listDirectory "resources"
         let htmlFiles = filter (\fp -> ".html" `isSuffixOf` fp && not ('/' `elem` fp)) files
         pairs <-
           mapM
             ( \fp -> do
                 rendered <- compile_template fp
                 [|($(TH.lift fp), rendered)|]
             )
             htmlFiles
         return (TH.ListE pairs)
     )

-- |
--  startApp bootstraps your application by taking the following environment parameters:
--
--    • appName             :: Name of the application.
--    • domainName          :: The domain name (useful for logging or URL generation, etc).
--    • publicDir           :: Directory where public (static) files reside.
--    • port                :: Port on which the web server will listen.
--    • redisDatabaseNumber :: Redis database number.
--    • redisDatabaseModals :: A list of model names whose counters should be initialized
--                             in Redis (each counter is stored with the key “<model>:next_id”).
--
--  Plus a routing function that must have the type:
--      R.Connection -> Map.Map FilePath TL.Text -> ScottyM ()
--  For example, if your Routes module provides:
--      appRoutes :: R.Connection -> Map.Map FilePath TL.Text -> ScottyM ()
--  you can start the application like this:
--      Kernel.startApp Env.appName Env.domainName Env.publicDir Env.port Env.redisDatabaseNumber Env.redisDatabaseModals Routes.appRoutes
startApp ::
  String -> -- appName
  String -> -- domainName
  String -> -- publicDir
  Int -> -- port
  Integer -> -- redisDatabaseNumber
  [String] -> -- redisDatabaseModals
  (R.Connection -> Map.Map FilePath TL.Text -> ScottyM ()) -> -- routing function
  IO ()
startApp appName domainName publicDir port redisDbNumber redisModals appRoutes = do
  putStrLn $ "[INFO] Running " ++ appName ++ " on port " ++ show port
  putStrLn $ "[INFO] Domain: " ++ domainName
  putStrLn $ "[INFO] Public directory: " ++ publicDir

  -- Initialize Redis connection and select the database.
  conn <- R.checkedConnect R.defaultConnectInfo
  _ <- R.runRedis conn $ R.select redisDbNumber

  -- Modal Creation Logic: For each model, initialize its counter in Redis if needed.
  mapM_
    ( \model -> do
        let key = BS.pack (model ++ ":next_id")
        existsReply <- R.runRedis conn $ R.exists key
        case existsReply of
          Left err ->
            error $ "[ERROR] Redis exists error for " ++ model ++ ": " ++ show err
          Right existsVal ->
            if existsVal
              then putStrLn $ "[DEBUG] Counter for model " ++ model ++ " already exists."
              else do
                setReply <- R.runRedis conn $ R.set key (BS.pack "0")
                case setReply of
                  Left errSet -> error $ "[ERROR] Redis set error for model " ++ model ++ ": " ++ show errSet
                  Right _ -> putStrLn $ "[INFO] Initialized counter for model: " ++ model
    )
    redisModals

  -- Launch the Scotty application with the provided routing function.
  scotty port (appRoutes conn preRenderedTemplates)
