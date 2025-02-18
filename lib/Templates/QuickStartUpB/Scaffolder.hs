{-# LANGUAGE CPP #-}
module Templates.QuickStartUpB.Scaffolder
  ( scaffold
  , writeFileWithInfo
  , ensureDir
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
-- Note: we now import doesDirectoryExist and removeDirectoryRecursive.
import System.Directory ( createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive )
import System.FilePath ((</>), takeDirectory, takeBaseName)
import Control.Monad ( when )
import System.Process ( callCommand )
import System.IO (stdout, hFlush)
import Data.List ( isPrefixOf )
import Data.Time ( getCurrentTime, utctDay )
import Data.Time.Calendar ( toGregorian )
import Data.List (stripPrefix)


import Templates.QuickStartUpB.TemplateFiles

-- | Update the package.yaml file with the values provided by the user.
-- For lines starting with:
--
--   name:         --> replaced with the app name (inferred from the target directory)
--   author:       --> replaced with the author name (with escaped quotes)
--   maintainer:   --> replaced with the maintainer's email (with escaped quotes)
--   copyright:    --> replaced with "<currentYear> <author>" (with escaped quotes)
--   github:       --> replaced with the git remote URL (with escaped quotes)
--   description:  --> replaced with the short project description
--
-- Finally, if any dependency line contains the token "YourApp",
-- that token is replaced with the new app name.
updatePackageYaml :: BS.ByteString  -- original package.yaml contents
                  -> String         -- appName (project name, inferred)
                  -> String         -- Author name
                  -> String         -- Git remote URL
                  -> String         -- Maintainer email
                  -> String         -- Project description
                  -> String         -- Current year
                  -> BS.ByteString  -- updated package.yaml contents
updatePackageYaml content appName author gitRemote maintainer description year =
  let s = BS8.unpack content
      ls = lines s
      ls' = map (\line ->
                if "name:" `isPrefixOf` line
                  then "name:                " ++ appName
                else if "author:" `isPrefixOf` line
                  then "author:              \"" ++ author ++ "\""
                else if "maintainer:" `isPrefixOf` line
                  then "maintainer:          \"" ++ maintainer ++ "\""
                else if "copyright:" `isPrefixOf` line
                  then "copyright:           \"" ++ year ++ " " ++ author ++ "\""
                else if "github:" `isPrefixOf` line
                  then "github:              \"" ++ gitRemote ++ "\""
                else if "description:" `isPrefixOf` line
                  then "description:         " ++ description
                else line
              ) ls
      ls'' = map (\l ->
                let trimmed = dropWhile (== ' ') l in
                if trimmed == "- YourApp"
                  then let prefix = takeWhile (== ' ') l in prefix ++ "- " ++ appName
                  else l
              ) ls'
  in BS8.pack (unlines ls'')

{-
  scaffold performs the following steps:

  STEP I -
    · Prompt the user for:
         o Author name
         o Git remote URL (or repository identifier, e.g., https://github.com/username/repo)
         o Maintainer's email address
         o A short project description
    · The app name is automatically inferred from the target directory name.

  STEP II -
    · Remove any existing directory with the project name.
    · Execute the command:
         stack new <project_dir_name> --resolver=lts-23.8
       (The output of this command is suppressed.)

  STEP III -
    · Overwrite selected project files (with our custom templates)

  STEP IV -
    · In package.yaml, update fields using the provided information,
      including setting the copyright year to the current year and
      replacing any occurrence of "YourApp" with the new app name.
-}
scaffold :: FilePath -> IO ()
scaffold targetDir = do
  -- The app (project) name is inferred from the targetDir using takeBaseName.
  let appName = takeBaseName targetDir
  putStrLn ("[INFO] Inferred application name: " ++ appName)

  -- STEP I: Prompt user for remaining project information.
  putStrLn "======================================"
  putStrLn "          PROJECT INFORMATION         "
  putStrLn "======================================"
  putStrLn ""
  putStr "Author Name               : "
  hFlush stdout
  author <- getLine
  putStr "Owner/Repo (owner/repo)   : "
  hFlush stdout
  gitRemote <- getLine
  putStr "Maintainer Email          : "
  hFlush stdout
  maintainer <- getLine
  putStr "Project Description       : "
  hFlush stdout
  projDescription <- getLine


  -- Get the current year.
  currentTime <- getCurrentTime
  let (year,_,_) = toGregorian (utctDay currentTime)
      currentYear = show year

  -- STEP II: Remove existing project directory if it exists.
  dirExists <- doesDirectoryExist targetDir
  when dirExists $ do
    putStrLn ("[INFO] Removing existing directory " ++ targetDir)
    removeDirectoryRecursive targetDir

  -- STEP II (continued): Run the 'stack new' command to create the new project.
  -- NOTE: We append shell redirection to suppress its output.
  let stackNewCmd = "stack new " ++ targetDir ++ " --resolver=lts-23.8 > /dev/null 2>&1"
  putStrLn $ "[INFO] Running: " ++ stackNewCmd
  callCommand stackNewCmd

   -- STEP III/IV: Overwrite generated project files with our custom templates.
  -- Define a helper to strip the "templates/<appName>/" prefix.
  let stripTemplatePrefix rel =
        case stripPrefix ("templates/" ++ "QuickStartUpB" ++ "/") rel of
          Just stripped -> stripped
          Nothing       -> rel

      fullPath sub = targetDir </> sub

      -- Use the aggregated list of templates from the qualified import.
      fileTemplates = Templates.QuickStartUpB.TemplateFiles.templates

  mapM_ (\(rel, fileData) -> do
           putStrLn $ "[DEBUG] Original relative path: " ++ rel
           let rel' = stripTemplatePrefix rel
           putStrLn $ "[DEBUG] Stripped relative path: " ++ rel'
           let dir = takeDirectory rel'
           putStrLn $ "[DEBUG] Directory for file (from stripped path): " ++ dir
           when (not (null dir)) $ do
             putStrLn $ "[DEBUG] Ensuring directory exists: " ++ (fullPath dir)
             ensureDir (fullPath dir)
           let finalContent = if rel' == "package.yaml"
                                then updatePackageYaml fileData appName author gitRemote maintainer projDescription currentYear
                                else fileData
           putStrLn $ "[DEBUG] Writing file to: " ++ (fullPath rel')
           writeFileWithInfo (fullPath rel') finalContent
        ) fileTemplates


  putStrLn "[INFO] tinfoiltiger - Scaffolding complete. You can now edit your files or compile."

-- | Write a file and print an informational message.
writeFileWithInfo :: FilePath -> BS.ByteString -> IO ()
writeFileWithInfo path content = do
  BS.writeFile path content
  putStrLn $ "[INFO] tinfoiltiger - Created or updated file: " ++ path

-- | Ensure that a directory exists.
ensureDir :: FilePath -> IO ()
ensureDir = createDirectoryIfMissing True


    