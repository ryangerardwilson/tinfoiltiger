{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TemplateMappings
  ( templateMappings,
    templateMappingsLookup,
    scaffolderMappings,
    templateFilesMapping
  ) where

import qualified Data.ByteString as BS
import Data.FileEmbed (embedDir)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- Import the scaffolder modules from your templates.
import qualified Templates.QuickStartUp.Scaffolder
import qualified Templates.QuickStartUpB.Scaffolder

-- Import the TemplateFiles modules from your templates.
import qualified Templates.QuickStartUp.TemplateFiles
import qualified Templates.QuickStartUpB.TemplateFiles

-- | A mapping from template name to its embedded (source) files.
-- Each entry is a list of (relative file path, file content) pairs.
templateMappings :: Map String [(FilePath, BS.ByteString)]
templateMappings = Map.fromList
  [   ("QuickStartUp", $(embedDir "lib/Templates/QuickStartUp")),
  ("QuickStartUpB", $(embedDir "lib/Templates/QuickStartUpB"))
  ]

-- | Lookup helper to find a template’s embedded files.
templateMappingsLookup :: String -> Maybe [(FilePath, BS.ByteString)]
templateMappingsLookup = flip Map.lookup templateMappings

-- | A mapping from template name to its scaffolder function.
scaffolderMappings :: Map String (FilePath -> IO ())
scaffolderMappings = Map.fromList
  [   ("QuickStartUp",  Templates.QuickStartUp.Scaffolder.scaffold),
  ("QuickStartUpB",  Templates.QuickStartUpB.Scaffolder.scaffold)
  ]

-- | A mapping for template-specific files exported from each TemplateFiles module.
-- Here we map a template name to its aggregated embedded files.
templateFilesMapping :: Map String [(FilePath, BS.ByteString)]
templateFilesMapping = Map.fromList
  [   ("QuickStartUp",  Templates.QuickStartUp.TemplateFiles.templates),
  ("QuickStartUpB",  Templates.QuickStartUpB.TemplateFiles.templates)
  ]
