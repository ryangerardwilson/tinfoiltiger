-- File: app/Main.hs
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Env (version)
import Initializer (initTool)
import Logo (logo)
import NewTemplateController (handleNewTemplate)
import System.Environment (getArgs)
-- import Text.RawString.QQ (r)
import UpgradeController (handleUpgrade)

main :: IO ()
main = do
  -- Print ASCII art banner using a raw string literal.

  putStrLn logo

  -- let version = "0.0.57-1"
  putStrLn $ "Version: " ++ version

  -- Get command-line arguments.
  args <- getArgs

  -- Handle the different flags.
  if "--init" `elem` args
    then
      initTool
    else
      if "--new" `elem` args
        then
          handleNewTemplate
        else
          if "--upgrade" `elem` args
            then
              handleUpgrade
            else
              putStrLn "Usage: tinfoiltiger --init | --new <target_directory> --template <template_name> | --upgrade"
