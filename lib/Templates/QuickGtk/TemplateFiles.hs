{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Templates.QuickGtk.TemplateFiles
  ( fileSetupExtHs,
    filePackageExtYaml,
    fileLicense,
    extGitignore,
    fileStackExtYaml,
    fileQuickgtkExtCabal,
    fileReadmeExtMd,
    appMain,
    testSpec,
    libLib,
    templates,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

fileSetupExtHs :: ByteString
fileSetupExtHs = $(embedFile "templates/QuickGtk/Setup.hs") -- fileSetupExtHs

filePackageExtYaml :: ByteString
filePackageExtYaml = $(embedFile "templates/QuickGtk/package.yaml") -- filePackageExtYaml

fileLicense :: ByteString
fileLicense = $(embedFile "templates/QuickGtk/LICENSE") -- fileLicense

extGitignore :: ByteString
extGitignore = $(embedFile "templates/QuickGtk/.gitignore") -- extGitignore

fileStackExtYaml :: ByteString
fileStackExtYaml = $(embedFile "templates/QuickGtk/stack.yaml") -- fileStackExtYaml

fileQuickgtkExtCabal :: ByteString
fileQuickgtkExtCabal = $(embedFile "templates/QuickGtk/QuickGtk.cabal") -- fileQuickgtkExtCabal

fileReadmeExtMd :: ByteString
fileReadmeExtMd = $(embedFile "templates/QuickGtk/README.md") -- fileReadmeExtMd

appMain :: ByteString
appMain = $(embedFile "templates/QuickGtk/app/Main.hs") -- appMain

testSpec :: ByteString
testSpec = $(embedFile "templates/QuickGtk/test/Spec.hs") -- testSpec

libLib :: ByteString
libLib = $(embedFile "templates/QuickGtk/lib/Lib.hs") -- libLib

templates :: [(FilePath, ByteString)]
templates =
  [ ("templates/QuickGtk/Setup.hs", fileSetupExtHs),
    ("templates/QuickGtk/package.yaml", filePackageExtYaml),
    ("templates/QuickGtk/LICENSE", fileLicense),
    ("templates/QuickGtk/.gitignore", extGitignore),
    ("templates/QuickGtk/stack.yaml", fileStackExtYaml),
    ("templates/QuickGtk/QuickGtk.cabal", fileQuickgtkExtCabal),
    ("templates/QuickGtk/README.md", fileReadmeExtMd),
    ("templates/QuickGtk/app/Main.hs", appMain),
    ("templates/QuickGtk/test/Spec.hs", testSpec),
    ("templates/QuickGtk/lib/Lib.hs", libLib)
  ]
