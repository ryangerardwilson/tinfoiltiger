{-# LANGUAGE QuasiQuotes #-}

module Initializer (initTool) where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Process (callCommand, readProcess)
import Text.RawString.QQ (r)

-- | initTool carries out all initialization steps:
--   1. Updates APT and upgrades the haskell-stack package.
--   2. Verifies the stack version.
--   3. Checks if the "tailwind" command exists; if not, creates a Python bootstrap script
--      to install the Tailwind binary and makes a symbolic link.
--   4. Ensures that Redis is installed, enabled, and only started if not already running.
initTool :: IO ()
initTool = do
  putStrLn "Initializing environment..."

  -- Update system and upgrade haskell-stack.
  putStrLn "Running: sudo apt update"
  callCommand "sudo apt update"
  putStrLn "Running: sudo apt upgrade haskell-stack -y"
  callCommand "sudo apt upgrade haskell-stack -y"
  putStrLn "Verifying stack installation with: stack --version"
  callCommand "stack --version"

  -- Check for the tailwind symlink.
  let tailwindLink = "/usr/local/bin/tailwind"
  exists <- doesFileExist tailwindLink
  if exists
    then putStrLn "Tailwind command already exists. Skipping tailwind installation."
    else do
      homeDir <- getEnv "HOME"
      let tailwindDir = homeDir </> ".tailwind"
      putStrLn $ "Creating tailwind directory at: " ++ tailwindDir
      createDirectoryIfMissing True tailwindDir

      -- Write out the Python tailwind bootstrap script.
      let scriptPath = tailwindDir </> "tailwind.py"
      putStrLn $ "Writing tailwind bootstrap script to: " ++ scriptPath
      writeFile scriptPath pythonScriptContent

      -- Make the script executable.
      putStrLn $ "Setting executable flag on: " ++ scriptPath
      callCommand $ "chmod +x " ++ scriptPath

      -- Create a symbolic link in /usr/local/bin.
      putStrLn $ "Creating symbolic link: sudo ln -s " ++ scriptPath ++ " /usr/local/bin/tailwind"
      callCommand $ "sudo ln -s " ++ scriptPath ++ " /usr/local/bin/tailwind"
      putStrLn "Tailwind installation completed successfully."

  -- Ensure redis is installed and running.
  putStrLn "Ensuring Redis is installed..."
  -- Though apt update was called earlier, call it again if needed.
  callCommand "sudo apt update"
  putStrLn "Installing redis-server (if not already installed): sudo apt install redis-server -y"
  callCommand "sudo apt install redis-server -y"
  putStrLn "Enabling redis-server: sudo systemctl enable redis-server"
  callCommand "sudo systemctl enable redis-server"

  putStrLn "Checking if redis-server is active..."
  status <- readProcess "sudo" ["systemctl", "is-active", "redis-server"] ""
  -- Remove any trailing newline when comparing.
  let currentStatus = takeWhile (/= '\n') status
  if currentStatus == "active"
    then putStrLn "Redis server is already running."
    else do
      putStrLn "Starting redis-server: sudo systemctl start redis-server"
      callCommand "sudo systemctl start redis-server"

  putStrLn "Displaying redis-server status: sudo systemctl status redis-server"
  callCommand "sudo systemctl status redis-server"

  putStrLn "Environment initialization complete."

-- | The Python script content defined as a nicely formatted multi-line string.
pythonScriptContent :: String
pythonScriptContent =
  [r|#!/usr/bin/env python3
import os
import sys
import stat
import shutil
import tempfile
import subprocess
import urllib.request

TAILWIND_CSS_URL = "https://github.com/tailwindlabs/tailwindcss/releases/latest/download/tailwindcss-linux-x64"

TARGET_DIR = os.path.join(os.path.expanduser('~'), '.tailwind')
TAILWIND_BINARY_NAME = "tailwindcss"
TAILWIND_BINARY_PATH = os.path.join(TARGET_DIR, TAILWIND_BINARY_NAME)

def ensure_target_directory():
    if not os.path.isdir(TARGET_DIR):
        print("Target directory {} does not exist. Creating it...".format(TARGET_DIR))
        try:
            os.makedirs(TARGET_DIR, exist_ok=True)
        except Exception as e:
            print("Failed to create directory {}: {}".format(TARGET_DIR, e))
            sys.exit(1)

def download_tailwind():
    print("tailwindcss binary not found in {}. Downloading...".format(TARGET_DIR))
    try:
        with urllib.request.urlopen(TAILWIND_CSS_URL) as response:
            with open(TAILWIND_BINARY_PATH, "wb") as out_file:
                shutil.copyfileobj(response, out_file)
    except Exception as e:
        print("Error downloading tailwindcss: {}".format(e))
        sys.exit(1)

    st = os.stat(TAILWIND_BINARY_PATH)
    os.chmod(TAILWIND_BINARY_PATH, st.st_mode | stat.S_IEXEC)
    print("tailwindcss successfully downloaded to {}.".format(TAILWIND_BINARY_PATH))

def main():
    if len(sys.argv) != 2:
        print("Usage: {} <output_file>".format(sys.argv[0]))
        sys.exit(1)

    output_file = sys.argv[1]

    ensure_target_directory()

    if not os.path.isfile(TAILWIND_BINARY_PATH):
        download_tailwind()
    else:
        print("Found tailwindcss binary at {}.".format(TAILWIND_BINARY_PATH))

    with tempfile.TemporaryDirectory() as tmpdirname:
        input_css_path = os.path.join(tmpdirname, "input.css")
        with open(input_css_path, "w") as f:
            f.write('@import "tailwindcss";\n')

        cmd = [TAILWIND_BINARY_PATH, "-i", input_css_path, "-o", output_file, "--minify"]
        print("Executing command: {}".format(" ".join(cmd)))

        try:
            subprocess.run(cmd, check=True)
        except subprocess.CalledProcessError as e:
            print("An error occurred while running tailwindcss: {}".format(e))
            sys.exit(1)

if __name__ == "__main__":
    main()|]
