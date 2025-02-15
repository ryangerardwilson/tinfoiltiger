{-# LANGUAGE OverloadedStrings #-}

module NewTemplateController (handleNewTemplate) where

import Data.List (elemIndex)
import qualified QuickStartUp.Scaffolder as S_QuickStartUp
import qualified QuickStartUpB.Scaffolder as S_QuickStartUpB
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

availableTemplates :: [(String, String)]
availableTemplates =
  [ ("QuickStartUp", "Please see the README on GitHub at <https://github.com/githubuser/YourApp#readme>"),
    ("QuickStartUpB", "Please see the README on GitHub at <https://github.com/githubuser/YourApp#readme>")
  ]

handleNewTemplate :: IO ()
handleNewTemplate = do
  args <- getArgs
  let newIndex = elemIndex "--new" args
      templateIndex = elemIndex "--template" args
  case (newIndex, templateIndex) of
    (_, Nothing) -> do
      putStrLn "------------------------------------"
      putStrLn "          AVAILABLE TEMPLATES       "
      putStrLn "------------------------------------"
      mapM_ (\(name, desc) -> putStrLn $ "  " ++ name ++ " - " ++ desc) availableTemplates
      putStrLn "------------------------------------"
      putStrLn "Usage: tinfoiltiger --new <project-name> --template <template-name>"
      putStrLn ""
      exitFailure
    (Nothing, _) -> do
      hPutStrLn stderr "Error: Missing --new flag."
      exitFailure
    (Just nIdx, Just tIdx) -> do
      let maybeTarget = if length args > nIdx + 1 then Just (args !! (nIdx + 1)) else Nothing
          maybeTemplate = if length args > tIdx + 1 then Just (args !! (tIdx + 1)) else Nothing
      case (maybeTarget, maybeTemplate) of
        (Nothing, _) -> do
          hPutStrLn stderr "Error: No target directory specified after --new."
          exitFailure
        (_, Nothing) -> do
          putStrLn "No template specified."
          putStrLn ""
          putStrLn "------------------------------------"
          putStrLn "Available Templates:"
          mapM_ (\(name, desc) -> putStrLn $ "  " ++ name ++ " - " ++ desc) availableTemplates
          putStrLn "------------------------------------"
          exitFailure
        (Just targetDir, Just tmpl) ->
          case tmpl of
            "QuickStartUp" -> S_QuickStartUp.scaffold targetDir
            "QuickStartUpB" -> S_QuickStartUpB.scaffold targetDir
            _ -> do
              hPutStrLn stderr ("Error: Unrecognized template " ++ tmpl)
              putStrLn ""
              putStrLn "------------------------------------"
              putStrLn "Available Templates:"
              mapM_ (\(name, desc) -> putStrLn $ "  " ++ name ++ " - " ++ desc) availableTemplates
              putStrLn "------------------------------------"
              exitFailure
