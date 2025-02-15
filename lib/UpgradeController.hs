------------------------------------------------------------
-- File: lib/UpgradeController.hs
------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UpgradeController (handleUpgrade) where

import Control.Exception (catch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List (find)
import Network.HTTP.Client (HttpException)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import qualified QuickStartUp.Templates as TQuickStartUp
import qualified QuickStartUpB.Templates as TQuickStartUpB
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    removeDirectoryRecursive,
  )
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)

-- import System.IO (hFlush, stdout)
-- import System.Process (callCommand)

----------------------------------------------------------------------
-- Helpers for string processing
----------------------------------------------------------------------
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (== ' ')

removeQuotes :: String -> String
removeQuotes s =
  if length s >= 2 && head s == '"' && last s == '"'
    then tail (init s)
    else s

----------------------------------------------------------------------
-- Main entry point: handleUpgrade
--
-- This function reads the project's package.yaml, extracts and
-- compares the template version in the "x-tinfoiltiger:" field,
-- then proceeds to:
--   1. Replace the lib/TinFoilTiger directory with new embedded content.
--   2. Update package.yaml by:
--        • Setting x-tinfoiltiger to "QuickStartUp/0.0.53.1"
--        • Scanning the local library dependencies and adding any new
--          packages from the embedded package.yaml (without removing any local package)
----------------------------------------------------------------------
handleUpgrade :: IO ()
handleUpgrade = do
  exists <- doesFileExist "package.yaml"
  if not exists
    then do
      putStrLn "Error: package.yaml not found in the current directory."
      putStrLn "Please cd into your project directory."
      exitFailure
    else do
      content <- BS.readFile "package.yaml"
      let pkgLines = BS8.lines content
          maybeXField = find (BS8.isPrefixOf "x-tinfoiltiger:") pkgLines
      case maybeXField of
        Nothing -> do
          putStrLn "No 'x-tinfoiltiger' field found in package.yaml."
          putStrLn "Please cd into your project directory and set the value"
          putStrLn "to something like \"TemplateName/get-latest\"."
        Just line -> do
          let rawVal = BS8.drop (BS8.length "x-tinfoiltiger:") line
              projValueRaw = trim (BS8.unpack rawVal)
              projValue = removeQuotes projValueRaw
          putStrLn $ "Found x-tinfoiltiger field in project package.yaml: " ++ projValue
          let (tmpl, rest) = break (== '/') projValue
          if null rest
            then putStrLn "Error: The x-tinfoiltiger field is not in the expected format \"TemplateName/version\"."
            else do
              let projVersion = drop 1 rest -- drop the '/'
              binaryPkg <- getEmbeddedPackageYaml tmpl
              let binLines = BS8.lines binaryPkg
                  maybeBinField = find (BS8.isPrefixOf "x-tinfoiltiger:") binLines
              case maybeBinField of
                Nothing -> putStrLn "Error: The embedded package.yaml does not have an x-tinfoiltiger field."
                Just binLine -> do
                  let binRaw = BS8.drop (BS8.length "x-tinfoiltiger:") binLine
                      binValueRaw = trim (BS8.unpack binRaw)
                      binValue = removeQuotes binValueRaw
                  putStrLn $ "Template header version in embedded package.yaml: " ++ binValue
                  let (_, binRest) = break (== '/') binValue
                  if null binRest
                    then putStrLn "Error: Embedded x-tinfoiltiger field is not in expected format."
                    else do
                      let binVersion = drop 1 binRest
                      if projVersion == binVersion
                        then putStrLn $ "Template versions match (" ++ projVersion ++ "). Proceeding with upgrade..."
                        else
                          putStrLn
                            ( "[INFO] Your project currently runs TinFoilTiger ("
                                ++ projVersion
                                ++ "), but the embedded template is ("
                                ++ binVersion
                                ++ "). Proceeding with upgrade..."
                            )
                      upgradeLibDir
                      updateProjectPackageYaml binaryPkg
                      checkRemoteVersion projVersion

----------------------------------------------------------------------
-- getEmbeddedPackageYaml selects the embedded package.yaml
-- for a given template name.
----------------------------------------------------------------------
getEmbeddedPackageYaml :: String -> IO BS.ByteString
getEmbeddedPackageYaml tmpl =
  case tmpl of
    "QuickStartUp" -> return TQuickStartUp.filePackageExtYaml
    "QuickStartUpB" -> return TQuickStartUpB.filePackageExtYaml
    _ -> do
      putStrLn $ "Error: Unrecognized template name \"" ++ tmpl ++ "\"."
      exitFailure

----------------------------------------------------------------------
-- upgradeLibDir replaces the lib/TinFoilTiger directory contents
-- with fresh embedded files. Instead of hardcoding file paths,
-- it filters the embedded template files (from QuickStartUp.Templates)
-- for any file whose path starts with "lib/TinFoilTiger".
----------------------------------------------------------------------
upgradeLibDir :: IO ()
upgradeLibDir = do
  let baseDir = "lib/TinFoilTiger"
  exists <- doesDirectoryExist baseDir
  if exists
    then do
      putStrLn $ "[INFO] Removing existing directory: " ++ baseDir
      removeDirectoryRecursive baseDir
    else return ()
  -- Use all embedded templates and filter those under lib/TinFoilTiger.
  let allTemplates = TQuickStartUp.templates -- the templates :: [(FilePath, BS.ByteString)]
      filesToUpgrade = filter (\(fp, _) -> "lib/TinFoilTiger/" `isPrefixOf` fp) allTemplates
  mapM_ writeTemplateFile filesToUpgrade
  putStrLn "[INFO] Successfully updated lib/TinFoilTiger directory."
  where
    -- A simple isPrefixOf helper.
    isPrefixOf :: String -> String -> Bool
    isPrefixOf prefix str = prefix == take (length prefix) str

    writeTemplateFile :: (FilePath, BS.ByteString) -> IO ()
    writeTemplateFile (fPath, content) = do
      let dir = takeDirectory fPath
      createDirectoryIfMissing True dir
      BS.writeFile fPath content
      putStrLn $ "[INFO] Updated file: " ++ fPath

----------------------------------------------------------------------
-- updateProjectPackageYaml updates project package.yaml by:
--  1) Replacing or inserting the "x-tinfoiltiger:" field with:
--         x-tinfoiltiger:      "QuickStartUp/0.0.53.1"
--  2) Scanning the existing library dependencies, and adding any new
--     dependencies listed in the embedded package.yaml's library block.
----------------------------------------------------------------------
updateProjectPackageYaml :: BS.ByteString -> IO ()
updateProjectPackageYaml embeddedPkgBs = do
  putStrLn "[INFO] Updating project package.yaml..."
  pkgExists <- doesFileExist "package.yaml"
  if not pkgExists
    then putStrLn "Error: package.yaml not found." >> exitFailure
    else do
      currContent <- BS.readFile "package.yaml"
      let currLines = lines (BS8.unpack currContent)
          -- Update or add x-tinfoiltiger field.
          newXField = "x-tinfoiltiger:      \"QuickStartUp/0.0.53.1\""
          updatedXLines = updateXField currLines newXField
          -- Break the file at the library section.
          (_, libBlockAndAfter) = break (isPrefixOf "library:") updatedXLines
          localDeps = if null libBlockAndAfter then [] else extractDependencies libBlockAndAfter
      -- Extract the dependencies from the embedded package.yaml.
      let embeddedLines = lines (BS8.unpack embeddedPkgBs)
          embeddedDeps = extractDependencies embeddedLines
          -- Determine which dependencies are new (order is not important).
          depsToAdd = filter (`notElem` localDeps) embeddedDeps
      putStrLn $ "[INFO] Adding new dependencies: " ++ show depsToAdd
      -- Now, update the library block: simply append missing dependencies
      -- to the dependencies: section.
      let finalLines = appendDependencies updatedXLines depsToAdd
      BS.writeFile "package.yaml" (BS8.pack (unlines finalLines))
      putStrLn "[INFO] package.yaml updated."
  where
    -- updateXField replaces the first occurrence of x-tinfoiltiger: with the new value.
    updateXField :: [String] -> String -> [String]
    updateXField [] _ = []
    updateXField (l : ls) newVal
      | "x-tinfoiltiger:" `isPrefixOf` l = newVal : ls
      | otherwise = l : updateXField ls newVal

    -- isPrefixOf: simple helper.
    isPrefixOf :: String -> String -> Bool
    isPrefixOf prefix str = prefix == take (length prefix) str

    -- extractDependencies searches inside a library block for lines starting with a dash,
    -- and returns a list of dependency names (trimmed).
    extractDependencies :: [String] -> [String]
    extractDependencies [] = []
    extractDependencies (l : ls)
      | "dependencies:" `isPrefixOf` l =
          let deps = takeWhile (\s -> (not (null s)) && (head s == ' ' || head s == '\t')) ls
           in map (trim . dropDash) deps
      | otherwise = extractDependencies ls
      where
        dropDash s = case dropWhile (== ' ') s of
          ('-' : rest) -> trim rest
          other -> trim other

    -- appendDependencies scans for the "dependencies:" block under the library section
    -- and appends any new dependencies (one per line) with proper indentation.
    appendDependencies :: [String] -> [String] -> [String]
    appendDependencies [] _ = []
    appendDependencies (l : ls) deps
      | "dependencies:" `isPrefixOf` l =
          let (depLines, rest) = span (\s -> (not (null s)) && (head s == ' ' || head s == '\t')) ls
              newDepLines = depLines ++ map (\d -> "    - " ++ d) deps
           in l : newDepLines ++ rest
      | otherwise = l : appendDependencies ls deps

----------------------------------------------------------------------
-- checkRemoteVersion downloads remote package info and compares the remote
-- tinfoiltiger version versus the given current version.
-- (Kept for informational purposes.)
----------------------------------------------------------------------
checkRemoteVersion :: String -> IO ()
checkRemoteVersion currVersion = do
  putStrLn "Checking for a newer tinfoiltiger application version remotely..."
  let url = "https://files.ryangerardwilson.com/tinfoiltiger/debian/dists/stable/main/binary-amd64/Packages"
  req <- parseRequest url
  response <-
    httpLBS req
      `catch` ( \(e :: HttpException) -> do
                  putStrLn ("Error fetching remote package info: " ++ show e)
                  exitFailure
              )
  let body = L8.lines (getResponseBody response)
      trimmedBody = map (L8.pack . trim . L8.unpack) body
      mVersionLine = find (L8.isPrefixOf "Version:") trimmedBody
  case mVersionLine of
    Nothing -> putStrLn "Could not determine remote tinfoiltiger version."
    Just verLine -> do
      let remoteVerRaw = L8.unpack (L8.drop 8 verLine)
          remoteVer = normalizeVersion (trim remoteVerRaw)
          currVer = normalizeVersion currVersion
      if compareVersions currVer remoteVer == LT
        then do
          putStrLn $ "A newer version of tinfoiltiger is available: " ++ remoteVer
          putStrLn "You may upgrade after running:"
          putStrLn "  sudo apt update; sudo apt install --only-upgrade tinfoiltiger"
        else putStrLn "tinfoiltiger is up-to-date."

----------------------------------------------------------------------
-- normalizeVersion maps '-' to '.' so that versions like "0.0.53-1"
-- become "0.0.53.1"
----------------------------------------------------------------------
normalizeVersion :: String -> String
normalizeVersion = map (\c -> if c == '-' then '.' else c)

----------------------------------------------------------------------
-- parseVersion splits a version string into a list of Ints.
----------------------------------------------------------------------
parseVersion :: String -> [Int]
parseVersion s = map read (wordsWhen (== '.') s)

----------------------------------------------------------------------
-- wordsWhen splits a string based on a predicate.
----------------------------------------------------------------------
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

----------------------------------------------------------------------
-- compareVersions compares two normalized version strings.
----------------------------------------------------------------------
compareVersions :: String -> String -> Ordering
compareVersions v1 v2 = compare (parseVersion v1) (parseVersion v2)
