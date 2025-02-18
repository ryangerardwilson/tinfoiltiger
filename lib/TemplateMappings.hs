{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TemplateMappings
  ( templateMappings,
    templateMappingsLookup,
    scaffolderMappings,
    templateFilesMapping,
  )
where

import qualified Data.ByteString as BS
import Data.FileEmbed (embedDir)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- Import the scaffolder modules from your templates.
import qualified Templates.QuickGtk.Scaffolder
-- Import the TemplateFiles modules from your templates.
import qualified Templates.QuickGtk.TemplateFiles
import qualified Templates.QuickStartUp.Scaffolder
import qualified Templates.QuickStartUp.TemplateFiles

-- | A mapping from template name to its embedded (source) files.
-- Each entry is a list of (relative file path, file content) pairs.
templateMappings :: Map String [(FilePath, BS.ByteString)]
templateMappings =
  Map.fromList
    [ ("QuickGtk", $(embedDir "lib/Templates/QuickGtk")),
      ("QuickStartUp", $(embedDir "lib/Templates/QuickStartUp"))
    ]

-- | Lookup helper to find a template’s embedded files.
templateMappingsLookup :: String -> Maybe [(FilePath, BS.ByteString)]
templateMappingsLookup = flip Map.lookup templateMappings

-- | A mapping from template name to its scaffolder function.
scaffolderMappings :: Map String (FilePath -> IO ())
scaffolderMappings =
  Map.fromList
    [ ("QuickGtk", Templates.QuickGtk.Scaffolder.scaffold),
      ("QuickStartUp", Templates.QuickStartUp.Scaffolder.scaffold)
    ]

-- | A mapping for template-specific files exported from each TemplateFiles module.
-- Here we map a template name to its aggregated embedded files.
templateFilesMapping :: Map String [(FilePath, BS.ByteString)]
templateFilesMapping =
  Map.fromList
    [ ("QuickGtk", Templates.QuickGtk.TemplateFiles.templates),
      ("QuickStartUp", Templates.QuickStartUp.TemplateFiles.templates)
    ]
