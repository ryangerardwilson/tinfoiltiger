-- UpgradeController.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UpgradeController (handleUpgrade) where

import Control.Exception (catch)
-- import Data.Time (getCurrentTime, utctDay)
-- import Data.Time.Calendar (toGregorian)

import Control.Monad (forM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List (find)
import Env (version)
import Network.HTTP.Client (HttpException)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeDirectoryRecursive,
  )
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))

-- import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------
-- Main entry point for upgrade
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
                          upgradeLibDir tmpl
                          updateProjectPackageYaml binaryPkg newHeader
                          checkRemoteVersion projVersion
  where
    ----------------------------------------------------------------
    -- Modify package.yaml by merging dependency blocks
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

        -- isPrefixOf :: String -> String -> Bool
        -- isPrefixOf prefix str = prefix == take (length prefix) str

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

    ----------------------------------------------------------------
    -- Upgrade files under lib/TinFoilTiger using embedded templates.
    -- Now the list of template files will be discovered from disk.
    upgradeLibDir :: String -> IO ()
    upgradeLibDir tmpl = do
      -- Remove the current installed library if it exists.
      let baseDir = "lib/TinFoilTiger"
      exists <- doesDirectoryExist baseDir
      if exists
        then do
          putStrLn $ "[INFO] Removing existing directory: " ++ baseDir
          removeDirectoryRecursive baseDir
        else return ()

      -- Assume that the embedded templates are stored on disk under:
      --    templates/<TemplateName>/
      let templatesPath = "templates" </> tmpl
      templatesExist <- doesDirectoryExist templatesPath
      if not templatesExist
        then do
          putStrLn $ "[ERROR] Template directory not found: " ++ templatesPath
          exitFailure
        else do
          -- Recursively list all files in the template directory.
          fileList <- listDirectoryRecursive templatesPath
          -- In the previous version we only upgraded files in the "lib/TinFoilTiger/" subdirectory.
          -- Here we mimic that logic by filtering for files that get installed into that folder.
          let filesToUpgrade =
                filter
                  ( \(fp, _) ->
                      let fp' = case stripPrefix (templatesPath ++ "/") fp of
                            Just stripped -> stripped
                            Nothing -> fp
                       in "lib/TinFoilTiger/" `isPrefixOf` fp'
                  )
                  fileList
          mapM_
            ( \(fp, content) -> do
                let targetPath = case stripPrefix (templatesPath ++ "/") fp of
                      Just stripped -> stripped
                      Nothing -> fp
                    dir = takeDirectory targetPath
                createDirectoryIfMissing True dir
                BS.writeFile targetPath content
                putStrLn $ "[INFO] Updated file: " ++ targetPath
            )
            filesToUpgrade
          putStrLn "[INFO] Successfully updated lib/TinFoilTiger directory."

    ----------------------------------------------------------------
    -- A helper: recursively list directory files with their contents.
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
    -- Get the embedded package.yaml file from disk.
    -- Expects the file at "templates/<TemplateName>/package-ext.yaml"
    getEmbeddedPackageYaml :: String -> IO BS.ByteString
    getEmbeddedPackageYaml tmpl = do
      let templatesBase = "templates"
          pkgFile = templatesBase </> tmpl </> "package-ext.yaml"
      exists <- doesFileExist pkgFile
      if exists
        then BS.readFile pkgFile
        else do
          putStrLn $ "Error: Template package file not found at " ++ pkgFile
          exitFailure

    ----------------------------------------------------------------
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

    ----------------------------------------------------------------
    checkRemoteVersion :: String -> IO ()
    checkRemoteVersion projVersion = do
      putStrLn $ "[INFO] (Stub) Performed remote version check for project version: " ++ projVersion
      return ()

    ----------------------------------------------------------------
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

    trim :: String -> String
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

    removeQuotes :: String -> String
    removeQuotes s =
      if length s >= 2 && head s == '"' && last s == '"'
        then tail (init s)
        else s

    ----------------------------------------------------------------
    -- Simple prefix helpers.
    isPrefixOf :: String -> String -> Bool
    isPrefixOf prefix str = prefix == take (length prefix) str

    stripPrefix :: String -> String -> Maybe String
    stripPrefix [] ys = Just ys
    stripPrefix _ [] = Nothing
    stripPrefix (x : xs) (y : ys)
      | x == y = stripPrefix xs ys
      | otherwise = Nothing
