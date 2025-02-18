-- lib/UpgradeController.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UpgradeController (handleUpgrade) where

import Control.Exception (catch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List (find, isPrefixOf)
import qualified Data.Map.Strict as Map
import Env (version)
import Network.HTTP.Client (HttpException)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    removeDirectoryRecursive,
  )
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, takeFileName, (</>))
import qualified TemplateMappings as TM

-- | Main entry point for the upgrade command.
-- Everything used is nested within handleUpgrade.
handleUpgrade :: IO ()
handleUpgrade = do
  -- Check that package.yaml exists in the current directory.
  pkgExists <- doesFileExist "package.yaml"
  if not pkgExists
    then do
      putStrLn "Error: package.yaml not found in the current directory."
      putStrLn "Please cd into your project directory."
      exitFailure
    else return ()

  -- Read and parse package.yaml to extract the x-tinfoiltiger field.
  content <- BS.readFile "package.yaml"
  let pkgLines = BS8.lines content
      maybeXField = find (BS8.isPrefixOf "x-tinfoiltiger:") pkgLines
  case maybeXField of
    Nothing -> do
      putStrLn "No 'x-tinfoiltiger' field found in package.yaml."
      putStrLn "Please cd into your project directory and set the value to something like \"TemplateName/get-latest\"."
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
                      putStrLn "Please upgrade current code by running: "
                      putStrLn "  sudo apt update; sudo apt install --only-upgrade tinfoiltiger; tinfoiltiger --upgrade"
                      exitFailure
                    else do
                      if normalizeVersion projVersion == currentCodeVer
                        then putStrLn $ "[INFO] You are already using the latest version (" ++ projVersion ++ "). Re-running upgrade..."
                        else putStrLn $ "[INFO] Upgrading version from " ++ projVersion ++ " to " ++ currentCodeVer ++ " in package.yaml..."
                      let newHeader = "x-tinfoiltiger:      \"" ++ tmpl ++ "/" ++ binVersion ++ "\""
                      upgradeLibDir tmpl
                      updateProjectPackageYaml binaryPkg newHeader
                      checkRemoteVersion projVersion
  where
    --------------------------------------------------------------------------------
    -- Nested helper: updateProjectPackageYaml
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
          | "x-tinfoiltiger:" `isPrefixOf` dropSpaces l = newVal : ls
          | otherwise = l : updateXFieldFunc ls newVal

        isLibLine :: String -> Bool
        isLibLine s = "library:" `isPrefixOf` dropSpaces s

        dropSpaces :: String -> String
        dropSpaces = dropWhile (`elem` (" \t" :: String))

        extractLibraryBlock :: [String] -> [String]
        extractLibraryBlock [] = []
        extractLibraryBlock (l : ls)
          | "library:" `isPrefixOf` dropSpaces l =
              l : takeWhile (\s -> null s || (not (null s) && head s `elem` (" \t" :: String))) ls
          | otherwise = extractLibraryBlock ls

        dropLibraryBlock :: [String] -> [String]
        dropLibraryBlock [] = []
        dropLibraryBlock (_ : ls) = dropWhile (\s -> null s || (not (null s) && head s `elem` (" \t" :: String))) ls

        extractDependencies :: [String] -> [String]
        extractDependencies [] = []
        extractDependencies (l : ls)
          | "dependencies:" `isPrefixOf` dropSpaces l =
              let deps = takeWhile (\s -> not (null s) && head s `elem` (" \t" :: String)) ls
               in map (trim . dropDash) deps
          | otherwise = extractDependencies ls
          where
            dropDash s = case dropWhile (== ' ') s of
              ('-' : rest) -> trim rest
              other -> trim other

        nub :: (Eq a) => [a] -> [a]
        nub [] = []
        nub (x : xs) = x : nub (filter (/= x) xs)

    --------------------------------------------------------------------------------
    -- Nested helper: upgradeLibDir (upgrades files under lib/TinFoilTiger).
    upgradeLibDir :: String -> IO ()
    upgradeLibDir tmpl = do
      let baseDir = "lib/TinFoilTiger"
          prefix = "templates/" ++ tmpl ++ "/lib/TinFoilTiger/"
      exists <- doesDirectoryExist baseDir
      if exists
        then do
          putStrLn $ "[INFO] Removing existing directory: " ++ baseDir
          removeDirectoryRecursive baseDir
        else return ()
      createDirectoryIfMissing True baseDir
      putStrLn $ "[DEBUG] Created directory: " ++ baseDir
      case Map.lookup tmpl TM.templateFilesMapping of
        Nothing -> do
          putStrLn $ "[ERROR] Template " ++ tmpl ++ " is not registered in templateFilesMapping."
          exitFailure
        Just files -> do
          putStrLn $ "[DEBUG] Embedded template file keys for " ++ tmpl ++ ": " ++ show (map fst files)
          let filesToUpgrade = filter (\(fp, _) -> prefix `isPrefixOf` fp) files
          putStrLn $ "[DEBUG] Found " ++ show (length filesToUpgrade) ++ " file(s) with prefix \"" ++ prefix ++ "\"."
          if null filesToUpgrade
            then putStrLn $ "[WARN] No files with prefix " ++ prefix ++ " found in the embedded template files. Nothing to upgrade."
            else mapM_ (writeEmbeddedFile baseDir prefix) filesToUpgrade
          putStrLn "[INFO] Successfully updated lib/TinFoilTiger directory."
      where
        writeEmbeddedFile :: FilePath -> String -> (FilePath, BS.ByteString) -> IO ()
        writeEmbeddedFile baseDir prefix (fp, content) = do
          let relativePath = drop (length prefix) fp
              targetPath = baseDir </> relativePath
              destDir = takeDirectory targetPath
          putStrLn $ "[DEBUG] Writing file: " ++ fp ++ " -> " ++ targetPath
          createDirectoryIfMissing True destDir
          BS.writeFile targetPath content
          putStrLn $ "[INFO] Updated file: " ++ targetPath

    --------------------------------------------------------------------------------
    -- Nested helper: getEmbeddedPackageYaml.
    getEmbeddedPackageYaml :: String -> IO BS.ByteString
    getEmbeddedPackageYaml tmpl =
      case Map.lookup tmpl TM.templateFilesMapping of
        Nothing -> do
          putStrLn $ "Error: Template \"" ++ tmpl ++ "\" is not registered in the embedded templateFilesMapping."
          exitFailure
        Just files -> do
          putStrLn $ "[DEBUG] TemplateFiles keys for template " ++ tmpl ++ ": " ++ show (map fst files)
          case find (\(k, _) -> takeFileName k == "package.yaml") files of
            Just (_, pkgYaml) -> do
              putStrLn $ "[DEBUG] Found package.yaml for template " ++ tmpl ++ " in templateFilesMapping."
              return pkgYaml
            Nothing -> do
              putStrLn "Error: Template package file (package.yaml) not found in the embedded templateFilesMapping."
              exitFailure

    --------------------------------------------------------------------------------
    -- Nested helper: checkCurrentCodeVersion.
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

    --------------------------------------------------------------------------------
    -- Nested helper: checkRemoteVersion.
    checkRemoteVersion :: String -> IO ()
    checkRemoteVersion projVersion = do
      putStrLn $ "[INFO] (Stub) Performed remote version check for project version: " ++ projVersion
      return ()

    --------------------------------------------------------------------------------
    -- Nested helper: normalizeVersion.
    normalizeVersion :: String -> String
    normalizeVersion = map (\c -> if c == '-' then '.' else c)

    --------------------------------------------------------------------------------
    -- Nested helper: parseVersion.
    parseVersion :: String -> [Int]
    parseVersion s = map read (wordsWhen (== '.') s)

    --------------------------------------------------------------------------------
    -- Nested helper: wordsWhen.
    wordsWhen :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =
      case dropWhile p s of
        "" -> []
        s' -> let (w, s'') = break p s' in w : wordsWhen p s''

    --------------------------------------------------------------------------------
    -- Nested helper: compareVersions.
    compareVersions :: String -> String -> Ordering
    compareVersions v1 v2 = compare (parseVersion v1) (parseVersion v2)

    --------------------------------------------------------------------------------
    -- Nested helper: trim.
    trim :: String -> String
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

    --------------------------------------------------------------------------------
    -- Nested helper: removeQuotes.
    removeQuotes :: String -> String
    removeQuotes s =
      if length s >= 2 && head s == '"' && last s == '"'
        then tail (init s)
        else s
