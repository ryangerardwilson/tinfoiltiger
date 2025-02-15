{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module QuickStartUpB.Templates
  ( fileSetupExtHs,
    filePackageExtYaml,
    fileLicense,
    extGitignore,
    fileStackExtYaml,
    fileReadmeExtMd,
    appMain,
    testSpec,
    resourcesSignup,
    resourcesDebugger,
    resourcesMain,
    resourcesRedisInsert,
    resourcesAbout,
    resourcesLogin,
    resourcesLanding,
    resourcesIndex,
    resourcesDashboard,
    resourcesAssetsStyles,
    resourcesComponentsWrappersBaseTemplate,
    resourcesComponentsEmbedsMainContent,
    libUtilsMonadmanager,
    libUtilsRenderer,
    libUtilsRedis,
    libConstantsEnv,
    libRoutehandlersAuth,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

fileSetupExtHs :: ByteString
fileSetupExtHs = $(embedFile "templates/QuickStartUpB/Setup.hs") -- fileSetupExtHs

filePackageExtYaml :: ByteString
filePackageExtYaml = $(embedFile "templates/QuickStartUpB/package.yaml") -- filePackageExtYaml

fileLicense :: ByteString
fileLicense = $(embedFile "templates/QuickStartUpB/LICENSE") -- fileLicense

extGitignore :: ByteString
extGitignore = $(embedFile "templates/QuickStartUpB/.gitignore") -- extGitignore

fileStackExtYaml :: ByteString
fileStackExtYaml = $(embedFile "templates/QuickStartUpB/stack.yaml") -- fileStackExtYaml

fileReadmeExtMd :: ByteString
fileReadmeExtMd = $(embedFile "templates/QuickStartUpB/README.md") -- fileReadmeExtMd

appMain :: ByteString
appMain = $(embedFile "templates/QuickStartUpB/app/Main.hs") -- appMain

testSpec :: ByteString
testSpec = $(embedFile "templates/QuickStartUpB/test/Spec.hs") -- testSpec

resourcesSignup :: ByteString
resourcesSignup = $(embedFile "templates/QuickStartUpB/resources/signup.html") -- resourcesSignup

resourcesDebugger :: ByteString
resourcesDebugger = $(embedFile "templates/QuickStartUpB/resources/debugger.html") -- resourcesDebugger

resourcesMain :: ByteString
resourcesMain = $(embedFile "templates/QuickStartUpB/resources/main.css") -- resourcesMain

resourcesRedisInsert :: ByteString
resourcesRedisInsert = $(embedFile "templates/QuickStartUpB/resources/redis_insert.html") -- resourcesRedisInsert

resourcesAbout :: ByteString
resourcesAbout = $(embedFile "templates/QuickStartUpB/resources/about.html") -- resourcesAbout

resourcesLogin :: ByteString
resourcesLogin = $(embedFile "templates/QuickStartUpB/resources/login.html") -- resourcesLogin

resourcesLanding :: ByteString
resourcesLanding = $(embedFile "templates/QuickStartUpB/resources/landing.html") -- resourcesLanding

resourcesIndex :: ByteString
resourcesIndex = $(embedFile "templates/QuickStartUpB/resources/index.html") -- resourcesIndex

resourcesDashboard :: ByteString
resourcesDashboard = $(embedFile "templates/QuickStartUpB/resources/dashboard.html") -- resourcesDashboard

resourcesAssetsStyles :: ByteString
resourcesAssetsStyles = $(embedFile "templates/QuickStartUpB/resources/assets/styles.css") -- resourcesAssetsStyles

resourcesComponentsWrappersBaseTemplate :: ByteString
resourcesComponentsWrappersBaseTemplate = $(embedFile "templates/QuickStartUpB/resources/components/wrappers/base-template.html") -- resourcesComponentsWrappersBaseTemplate

resourcesComponentsEmbedsMainContent :: ByteString
resourcesComponentsEmbedsMainContent = $(embedFile "templates/QuickStartUpB/resources/components/embeds/main-content.html") -- resourcesComponentsEmbedsMainContent

libUtilsMonadmanager :: ByteString
libUtilsMonadmanager = $(embedFile "templates/QuickStartUpB/lib/Utils/MonadManager.hs") -- libUtilsMonadmanager

libUtilsRenderer :: ByteString
libUtilsRenderer = $(embedFile "templates/QuickStartUpB/lib/Utils/Renderer.hs") -- libUtilsRenderer

libUtilsRedis :: ByteString
libUtilsRedis = $(embedFile "templates/QuickStartUpB/lib/Utils/Redis.hs") -- libUtilsRedis

libConstantsEnv :: ByteString
libConstantsEnv = $(embedFile "templates/QuickStartUpB/lib/Constants/Env.hs") -- libConstantsEnv

libRoutehandlersAuth :: ByteString
libRoutehandlersAuth = $(embedFile "templates/QuickStartUpB/lib/RouteHandlers/Auth.hs") -- libRoutehandlersAuth
