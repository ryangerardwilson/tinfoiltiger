{-# LANGUAGE OverloadedStrings #-}

module NewTemplateController (handleNewTemplate) where

import Control.Monad (forM, forM_)
import qualified Data.ByteString as BS
import Data.List (sort)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

handleNewTemplate :: IO ()
handleNewTemplate = do
  args <- getArgs
  let newIndex = elemIndex "--new" args
      templateIndex = elemIndex "--template" args
  case (newIndex, templateIndex) of
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
    -- elemIndex: find index of an element in a list.
    elemIndex :: (Eq a) => a -> [a] -> Maybe Int
    elemIndex x xs = go 0 xs
      where
        go _ [] = Nothing
        go i (z : zs)
          | x == z = Just i
          | otherwise = go (i + 1) zs

    ----------------------------------------------------------------
    -- getAvailableTemplates: list subdirectories under "templates".
    getAvailableTemplates :: IO [String]
    getAvailableTemplates = do
      let templatesBase = "templates"
      exists <- doesDirectoryExist templatesBase
      if not exists
        then do
          hPutStrLn stderr ("Error: no templates directory found at: " ++ templatesBase)
          exitFailure
        else do
          names <- listDirectory templatesBase
          dirs <- forM names $ \nm -> do
            let fullPath = templatesBase </> nm
            isDir <- doesDirectoryExist fullPath
            return (if isDir then Just nm else Nothing)
          return (sort [nm | Just nm <- dirs])

    ----------------------------------------------------------------
    -- listDirectoryRecursive: recursively list files in a directory.
    listDirectoryRecursive :: FilePath -> IO [(FilePath, BS.ByteString)]
    listDirectoryRecursive topDir = do
      contents <- listDirectory topDir
      let properNames = filter (`notElem` [".", ".."]) contents
      filesAndDirs <- forM properNames $ \name -> do
        let fullPath = topDir </> name
        isDir <- doesDirectoryExist fullPath
        if isDir
          then listDirectoryRecursive fullPath
          else do
            fileContent <- BS.readFile fullPath
            return [(fullPath, fileContent)]
      return (concat filesAndDirs)

    ----------------------------------------------------------------
    -- scaffoldTemplate: copy all files from the template into the target directory.
    scaffoldTemplate :: String -> FilePath -> IO ()
    scaffoldTemplate tmpl targetDir = do
      let srcDir = "templates" </> tmpl
      exists <- doesDirectoryExist srcDir
      if not exists
        then do
          hPutStrLn stderr $ "Error: Template directory not found: " ++ srcDir
          exitFailure
        else do
          files <- listDirectoryRecursive srcDir
          forM_ files $ \(srcFile, content) -> do
            let relPath = makeRelative srcDir srcFile
                destPath = targetDir </> relPath
                destDir = takeDirectory destPath
            createDirectoryIfMissing True destDir
            BS.writeFile destPath content
            putStrLn $ "Created file: " ++ destPath
          putStrLn $ "Template \"" ++ tmpl ++ "\" successfully scaffolded into " ++ targetDir

    ----------------------------------------------------------------
    -- printAvailableTemplates: display the names of available templates.
    printAvailableTemplates :: IO ()
    printAvailableTemplates = do
      templates <- getAvailableTemplates
      putStrLn "------------------------------------"
      putStrLn "          AVAILABLE TEMPLATES       "
      putStrLn "------------------------------------"
      mapM_ (\name -> putStrLn $ "  " ++ name) templates
      putStrLn "------------------------------------"
