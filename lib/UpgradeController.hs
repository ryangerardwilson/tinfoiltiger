{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UpgradeController (handleUpgrade) where

import Control.Exception (catch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List (find)
-- version :: String defined in lib/Env.hs
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Env (version)
import Network.HTTP.Client (HttpException)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
-----------------------------------
-- NEED TO MAKE THE PYTHON SCRIPT DYNAMICALLY ADD THE BELOW 2 IMPORTS, ALONG WITH THE METHOD getEmbeddedPackageYaml
-----------------------------------
import qualified QuickStartUp.Templates as TQuickStartUp
import qualified QuickStartUpB.Templates as TQuickStartUpB
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)

----------------------------------------------------------------------
-- handleUpgrade
--
-- 1. Reads package.yaml and extracts the x-tinfoiltiger field.
-- 2. Splits that into tmpl and projVersion.
-- 3. Reads the embedded package.yaml for tmpl and extracts binVersion.
-- 4. Builds a new header line dynamically:
--      "x-tinfoiltiger:      \"tmpl/binVersion\""
-- 5. Checks current code version compared to remote.
--    - If a newer version of current code exists, prints instructions and exits.
-- 6. If the project’s version equals the current code version then informs
--    the user; otherwise it indicates an upgrade.
-- 7. Calls upgradeLibDir and updateProjectPackageYaml with the new header.
-- 8. Finally, calls checkRemoteVersion for informational purposes.
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
                          currentCodeVer = normalizeVersion version
                      newerCodeAvailable <- checkCurrentCodeVersion version
                      if newerCodeAvailable
                        then do
                          putStrLn "[INFO] A newer version of the current code is available."
                          putStrLn "Please upgrade current code by running:"
                          putStrLn "  sudo apt update; sudo apt install --only-upgrade tinfoiltiger; tinfoiltiger --upgrade"
                          exitFailure
                        else do
                          if normalizeVersion projVersion == currentCodeVer
                            then putStrLn $ "[INFO] You are already using the latest version (" ++ projVersion ++ "). Re-running upgrade..."
                            else putStrLn $ "[INFO] Upgrading version from " ++ projVersion ++ " to " ++ currentCodeVer ++ " in package.yaml..."
                          -- Build new header dynamically.
                          let newHeader = "x-tinfoiltiger:      \"" ++ tmpl ++ "/" ++ binVersion ++ "\""
                          upgradeLibDir
                          updateProjectPackageYaml binaryPkg newHeader
                          checkRemoteVersion projVersion

----------------------------------------------------------------------
-- updateProjectPackageYaml
--
-- Uses the new header (newXField) to update package.yaml by replacing
-- the x-tinfoiltiger field and merging the library dependencies from
-- the current file with the embedded ones.
----------------------------------------------------------------------
updateProjectPackageYaml :: BS.ByteString -> String -> IO ()
updateProjectPackageYaml embeddedPkgBs newXField = do
  putStrLn "[INFO] Updating project package.yaml..."
  pkgExists <- doesFileExist "package.yaml"
  if not pkgExists
    then putStrLn "Error: package.yaml not found." >> exitFailure
    else do
      currContent <- BS.readFile "package.yaml"
      let currLines = lines (BS8.unpack currContent)
          updatedXLines = updateXFieldFunc currLines newXField
          (beforeLib, libBlockAndAfter) = break isLibLine updatedXLines
          currentLibBlock = if null libBlockAndAfter then [] else extractLibraryBlock libBlockAndAfter
          localDeps = extractDependencies currentLibBlock
      putStrLn "[DEBUG] Current library block from package.yaml:"
      mapM_ putStrLn currentLibBlock
      putStrLn $ "[DEBUG] Extracted local dependencies: " ++ show localDeps

      let embeddedLines = lines (BS8.unpack embeddedPkgBs)
          embeddedLibBlock = extractLibraryBlock embeddedLines
          embeddedDeps = extractDependencies embeddedLibBlock
      putStrLn "[DEBUG] Embedded library block from embedded package.yaml:"
      mapM_ putStrLn embeddedLibBlock
      putStrLn $ "[DEBUG] Extracted embedded dependencies: " ++ show embeddedDeps

      let mergedDeps = nub (localDeps ++ embeddedDeps)
      putStrLn $ "[DEBUG] Merged dependency list: " ++ show mergedDeps

      let newLibBlock =
            [ "library:",
              "  source-dirs: lib",
              "  dependencies:"
            ]
              ++ map (\d -> "    - " ++ d) mergedDeps
      putStrLn "[DEBUG] New library block to be inserted:"
      mapM_ putStrLn newLibBlock

      let finalLines = beforeLib ++ newLibBlock ++ [""] ++ dropLibraryBlock libBlockAndAfter
      putStrLn "[DEBUG] Final package.yaml content (excerpt):"
      mapM_ putStrLn (take 20 finalLines)
      BS.writeFile "package.yaml" (BS8.pack (unlines finalLines))
      putStrLn "[INFO] package.yaml updated."
  where
    updateXFieldFunc :: [String] -> String -> [String]
    updateXFieldFunc [] _ = []
    updateXFieldFunc (l : ls) newVal
      | "x-tinfoiltiger:" `isPrefixOf` (dropSpaces l) = newVal : ls
      | otherwise = l : updateXFieldFunc ls newVal

    isLibLine :: String -> Bool
    isLibLine s = "library:" `isPrefixOf` (dropSpaces s)

    isPrefixOf :: String -> String -> Bool
    isPrefixOf prefix str = prefix == take (length prefix) str

    dropSpaces :: String -> String
    dropSpaces = dropWhile (\(c :: Char) -> c `elem` (" \t" :: String))

    extractLibraryBlock :: [String] -> [String]
    extractLibraryBlock [] = []
    extractLibraryBlock (l : ls)
      | "library:" `isPrefixOf` (dropSpaces l) =
          l : takeWhile (\s -> null s || (not (null s) && head s `elem` (" \t" :: String))) ls
      | otherwise = extractLibraryBlock ls

    dropLibraryBlock :: [String] -> [String]
    dropLibraryBlock [] = []
    dropLibraryBlock (l : ls) = dropWhile (\s -> null s || (not (null s) && head s `elem` (" \t" :: String))) ls

    extractDependencies :: [String] -> [String]
    extractDependencies [] = []
    extractDependencies (l : ls)
      | "dependencies:" `isPrefixOf` (dropSpaces l) =
          let deps = takeWhile (\s -> not (null s) && head s `elem` (" \t" :: String)) ls
           in map (trim . dropDash) deps
      | otherwise = extractDependencies ls
      where
        dropDash s = case dropWhile (== ' ') s of
          ('-' : rest) -> trim rest
          other -> trim other

    trim :: String -> String
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

    nub :: (Eq a) => [a] -> [a]
    nub [] = []
    nub (x : xs) = x : nub (filter (/= x) xs)

----------------------------------------------------------------------
-- upgradeLibDir
--
-- Deletes any existing lib/TinFoilTiger directory and writes embedded
-- template files (whose paths start with "templates/QuickStartUp/") to
-- the corresponding target paths after stripping the "templates/QuickStart/" prefix.
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
  -- Use all embedded templates; strip the prefix and filter for files under "lib/TinFoilTiger/"
  let templateName = "QuickStartUp"
      stripPrefixForUpgrade fp =
        case stripPrefix ("templates/" ++ templateName ++ "/") fp of
          Just stripped -> stripped
          Nothing -> fp
      allTemplates = TQuickStartUp.templates -- aggregated list: [(FilePath, BS.ByteString)]
      filesToUpgrade = filter (\(fp, _) -> let fp' = stripPrefixForUpgrade fp in "lib/TinFoilTiger/" `isPrefixOf` fp') allTemplates
  mapM_
    ( \(fp, content) -> do
        let targetPath = stripPrefixForUpgrade fp
            dir = takeDirectory targetPath
        createDirectoryIfMissing True dir
        BS.writeFile targetPath content
        putStrLn $ "[INFO] Updated file: " ++ targetPath
    )
    filesToUpgrade
  putStrLn "[INFO] Successfully updated lib/TinFoilTiger directory."
  where
    isPrefixOf :: String -> String -> Bool
    isPrefixOf prefix str = prefix == take (length prefix) str

    -- Import Data.List.stripPrefix
    stripPrefix :: String -> String -> Maybe String
    stripPrefix [] ys = Just ys
    stripPrefix _ [] = Nothing
    stripPrefix (x : xs) (y : ys)
      | x == y = stripPrefix xs ys
      | otherwise = Nothing

----------------------------------------------------------------------
-- checkCurrentCodeVersion
--
-- Fetches remote package information and compares the remote code version
-- with the current code version from Env.version. Returns True if a newer
-- version is available.
----------------------------------------------------------------------
checkCurrentCodeVersion :: String -> IO Bool
checkCurrentCodeVersion currentVer = do
  putStrLn "Checking for a newer version of current code remotely..."
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
    Nothing -> do
      putStrLn "Could not determine remote version for current code."
      return False
    Just verLine -> do
      let remoteVerRaw = L8.unpack (L8.drop 8 verLine)
          remoteVer = normalizeVersion (trim remoteVerRaw)
      putStrLn $ "[DEBUG] Current code version: " ++ normalizeVersion currentVer
      putStrLn $ "[DEBUG] Remote code version: " ++ remoteVer
      return (compareVersions (normalizeVersion currentVer) remoteVer == LT)

----------------------------------------------------------------------
-- checkRemoteVersion
--
-- For informational purposes, fetches remote package information and compares
-- the remote tinfoiltiger version with the version currently specified in package.yaml.
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
-- Helpers for version normalization and comparison
----------------------------------------------------------------------
normalizeVersion :: String -> String
normalizeVersion = map (\c -> if c == '-' then '.' else c)

parseVersion :: String -> [Int]
parseVersion s = map read (wordsWhen (== '.') s)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> let (w, s'') = break p s' in w : wordsWhen p s''

compareVersions :: String -> String -> Ordering
compareVersions v1 v2 = compare (parseVersion v1) (parseVersion v2)

----------------------------------------------------------------------
-- Basic string helpers: trim and removeQuotes
----------------------------------------------------------------------
trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

removeQuotes :: String -> String
removeQuotes s =
  if length s >= 2 && head s == '"' && last s == '"'
    then tail (init s)
    else s

----------------------------------------------------------------------
-- getEmbeddedPackageYaml
--
-- Chooses the embedded package.yaml based on the template name.
----------------------------------------------------------------------
getEmbeddedPackageYaml :: String -> IO BS.ByteString
getEmbeddedPackageYaml tmpl =
  case tmpl of
    "QuickStartUp" -> return TQuickStartUp.filePackageExtYaml
    "QuickStartUpB" -> return TQuickStartUpB.filePackageExtYaml
    _ -> do
      putStrLn $ "Error: Unrecognized template name \"" ++ tmpl ++ "\"."
      exitFailure

------------------------------------------------------------
-- End of UpgradeController.hs
------------------------------------------------------------
