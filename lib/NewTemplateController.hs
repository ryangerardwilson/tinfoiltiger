-- lib/NewTemplateController.hs
{-# LANGUAGE OverloadedStrings #-}

module NewTemplateController (handleNewTemplate) where

import Data.List (sort)
import qualified Data.Map.Strict as Map
-- import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified TemplateMappings as TM

-- | handleNewTemplate parses command-line arguments and, if a known template
-- is specified, invokes that template’s scaffolder.
handleNewTemplate :: IO ()
handleNewTemplate = do
  args <- getArgs
  let newIdx = elemIndex "--new" args
      tmplIdx = elemIndex "--template" args
  case (newIdx, tmplIdx) of
    (_, Nothing) -> do
      printAvailableTemplates
      putStrLn "Usage: tinfoiltiger --new <project-name> --template <template-name>"
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
          printAvailableTemplates
          exitFailure
        (Just targetDir, Just tmpl) -> do
          available <- getAvailableTemplates
          if tmpl `elem` available
            then scaffoldTemplate tmpl targetDir
            else do
              hPutStrLn stderr ("Error: Unrecognized template " ++ tmpl)
              printAvailableTemplates
              exitFailure
  where
    ----------------------------------------------------------------
    -- elemIndex: find the index of an element in a list.
    elemIndex :: (Eq a) => a -> [a] -> Maybe Int
    elemIndex x xs = go 0 xs
      where
        go _ [] = Nothing
        go i (y : ys)
          | x == y = Just i
          | otherwise = go (i + 1) ys

    ----------------------------------------------------------------
    -- getAvailableTemplates: list available template names from scaffolderMappings.
    getAvailableTemplates :: IO [String]
    getAvailableTemplates =
      return . sort . Map.keys $ TM.scaffolderMappings

    ----------------------------------------------------------------
    -- scaffoldTemplate looks up the scaffolder for the given template name
    -- and executes that function using the target directory.
    scaffoldTemplate :: String -> FilePath -> IO ()
    scaffoldTemplate tmpl targetDir =
      case Map.lookup tmpl TM.scaffolderMappings of
        Nothing -> hPutStrLn stderr ("Error: Template " ++ tmpl ++ " is not registered.")
        Just scaffolder -> do
          putStrLn $ "[INFO] Running scaffold for template " ++ tmpl ++ "..."
          scaffolder targetDir

    ----------------------------------------------------------------
    -- printAvailableTemplates: display the names of available templates.
    printAvailableTemplates :: IO ()
    printAvailableTemplates = do
      availTemplates <- getAvailableTemplates
      putStrLn "------------------------------------"
      putStrLn "          AVAILABLE TEMPLATES       "
      putStrLn "------------------------------------"
      mapM_ (\name -> putStrLn ("  " ++ name)) availTemplates
      putStrLn "------------------------------------"
