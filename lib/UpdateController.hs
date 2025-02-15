{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UpdateController
  ( handleUpdate,
    handleUpgrade,
  )
where

import Control.Exception (catch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List (find)
import Network.HTTP.Client (HttpException)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
-- Import the embedded templates for each template type.
import qualified QuickStartUp.Templates as TQuickStartUp
import qualified QuickStartUpB.Templates as TQuickStartUpB
import System.Directory (doesFileExist)
import System.Exit (exitFailure)

----------------------------------------------------------------------
-- Helper: trim whitespace from both ends.
----------------------------------------------------------------------
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (== ' ')

----------------------------------------------------------------------
-- Helper: remove surrounding quotes if present.
----------------------------------------------------------------------
removeQuotes :: String -> String
removeQuotes s =
  if length s >= 2 && head s == '"' && last s == '"'
    then tail (init s)
    else s

----------------------------------------------------------------------
-- Main entry point: handleUpdate
--
-- Reads package.yaml in the current directory and looks for the
-- x-tinfoiltiger field. It then removes any extra quotes, extracts
-- the TemplateName and version, and compares against the embedded
-- package.yaml for this template.
----------------------------------------------------------------------
handleUpdate :: IO ()
handleUpdate = do
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
              -- Get the embedded (binary) package.yaml for this template.
              binaryPkg <- getEmbeddedPackageYaml tmpl
              let binLines = BS8.lines binaryPkg
                  maybeBinField = find (BS8.isPrefixOf "x-tinfoiltiger:") binLines
              case maybeBinField of
                Nothing -> do
                  putStrLn "Error: The embedded package.yaml does not have an x-tinfoiltiger field."
                Just binLine -> do
                  let binRaw = BS8.drop (BS8.length "x-tinfoiltiger:") binLine
                      binValueRaw = trim (BS8.unpack binRaw)
                      binValue = removeQuotes binValueRaw
                  putStrLn $ "Binary template header version: " ++ binValue
                  let (_, binRest) = break (== '/') binValue
                  if null binRest
                    then putStrLn "Error: Binary x-tinfoiltiger field is not in expected format."
                    else do
                      let binVersion = drop 1 binRest
                      if projVersion == binVersion
                        then do
                          putStrLn $ "Template versions match (" ++ projVersion ++ ")."
                          checkRemoteVersion projVersion
                        else do
                          putStrLn $ "[INFO] Your project currently runs TinFoilTiger (" ++ projVersion ++ "), and an upgrade to lib/TinFoilTiger is available in TinFoilTiger version (" ++ binVersion ++ "). Consider updating after upgrading TinFoilTiger:"
                          putStrLn "  - Step 1: sudo apt update; sudo apt install --upgrade-only tinfoil"
                          putStrLn "  - Step 2: cd <your-project-dir>; tinfoiltiger --update"
                          putStrLn "  - Step 3: (if an update is available) tinfoiltiger --upgrade"

----------------------------------------------------------------------
-- handleUpgrade currently does nothing.
----------------------------------------------------------------------
handleUpgrade :: IO ()
handleUpgrade = putStrLn "handleUpgrade: Feature not implemented yet."

----------------------------------------------------------------------
-- getEmbeddedPackageYaml chooses the embedded package.yaml
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
-- checkRemoteVersion downloads remote package info and compares
-- the remote tinfoiltiger version versus the given current version.
--
-- The Packages file is expected to include a line beginning with "Version:".
-- A simple normalization is applied (replacing '-' with '.') before comparing.
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
      -- Instead of L8.strip, we convert each line to String, trim it, and pack it back.
      trimmedBody = map (L8.pack . trim . L8.unpack) body
      mVersionLine = find (L8.isPrefixOf "Version:") trimmedBody
  case mVersionLine of
    Nothing -> putStrLn "Could not determine remote tinfoiltiger version."
    Just verLine -> do
      let remoteVerRaw = L8.unpack (L8.drop 8 verLine) -- drop "Version:" (8 characters)
          remoteVer = normalizeVersion (trim remoteVerRaw)
          currVer = normalizeVersion currVersion
      if compareVersions currVer remoteVer == LT
        then do
          putStrLn $ "A newer version of tinfoiltiger is available: " ++ remoteVer
          putStrLn "You may upgrade after running:"
          putStrLn "  sudo apt update; sudo apt install --only-upgrade tinfoiltiger"
        else putStrLn "tinfoiltiger is up-to-date."

----------------------------------------------------------------------
-- normalizeVersion maps '-' to '.' so that versions like "0.0.53-1" become "0.0.53.1"
----------------------------------------------------------------------
normalizeVersion :: String -> String
normalizeVersion = map (\c -> if c == '-' then '.' else c)

----------------------------------------------------------------------
-- parseVersion splits a version string into a list of Ints.
----------------------------------------------------------------------
parseVersion :: String -> [Int]
parseVersion s = map read (wordsWhen (== '.') s)

----------------------------------------------------------------------
-- Splits a string based on a predicate.
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

--------------------------------------------------
