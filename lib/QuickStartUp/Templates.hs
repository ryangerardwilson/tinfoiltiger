{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module QuickStartUp.Templates
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
    libEnv,
    libRoutes,
    libTinfoiltigerEnv,
    libTinfoiltigerKernel,
    libTinfoiltigerUtilsMonadmanager,
    libTinfoiltigerUtilsRenderer,
    libTinfoiltigerUtilsRedis,
    libTinfoiltigerRoutehandlersAuth,
    templates,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

fileSetupExtHs :: ByteString
fileSetupExtHs = $(embedFile "templates/QuickStartUp/Setup.hs") -- fileSetupExtHs

filePackageExtYaml :: ByteString
filePackageExtYaml = $(embedFile "templates/QuickStartUp/package.yaml") -- filePackageExtYaml

fileLicense :: ByteString
fileLicense = $(embedFile "templates/QuickStartUp/LICENSE") -- fileLicense

extGitignore :: ByteString
extGitignore = $(embedFile "templates/QuickStartUp/.gitignore") -- extGitignore

fileStackExtYaml :: ByteString
fileStackExtYaml = $(embedFile "templates/QuickStartUp/stack.yaml") -- fileStackExtYaml

fileReadmeExtMd :: ByteString
fileReadmeExtMd = $(embedFile "templates/QuickStartUp/README.md") -- fileReadmeExtMd

appMain :: ByteString
appMain = $(embedFile "templates/QuickStartUp/app/Main.hs") -- appMain

testSpec :: ByteString
testSpec = $(embedFile "templates/QuickStartUp/test/Spec.hs") -- testSpec

resourcesSignup :: ByteString
resourcesSignup = $(embedFile "templates/QuickStartUp/resources/signup.html") -- resourcesSignup

resourcesDebugger :: ByteString
resourcesDebugger = $(embedFile "templates/QuickStartUp/resources/debugger.html") -- resourcesDebugger

resourcesMain :: ByteString
resourcesMain = $(embedFile "templates/QuickStartUp/resources/main.css") -- resourcesMain

resourcesRedisInsert :: ByteString
resourcesRedisInsert = $(embedFile "templates/QuickStartUp/resources/redis_insert.html") -- resourcesRedisInsert

resourcesAbout :: ByteString
resourcesAbout = $(embedFile "templates/QuickStartUp/resources/about.html") -- resourcesAbout

resourcesLogin :: ByteString
resourcesLogin = $(embedFile "templates/QuickStartUp/resources/login.html") -- resourcesLogin

resourcesLanding :: ByteString
resourcesLanding = $(embedFile "templates/QuickStartUp/resources/landing.html") -- resourcesLanding

resourcesIndex :: ByteString
resourcesIndex = $(embedFile "templates/QuickStartUp/resources/index.html") -- resourcesIndex

resourcesDashboard :: ByteString
resourcesDashboard = $(embedFile "templates/QuickStartUp/resources/dashboard.html") -- resourcesDashboard

resourcesAssetsStyles :: ByteString
resourcesAssetsStyles = $(embedFile "templates/QuickStartUp/resources/assets/styles.css") -- resourcesAssetsStyles

resourcesComponentsWrappersBaseTemplate :: ByteString
resourcesComponentsWrappersBaseTemplate = $(embedFile "templates/QuickStartUp/resources/components/wrappers/base-template.html") -- resourcesComponentsWrappersBaseTemplate

resourcesComponentsEmbedsMainContent :: ByteString
resourcesComponentsEmbedsMainContent = $(embedFile "templates/QuickStartUp/resources/components/embeds/main-content.html") -- resourcesComponentsEmbedsMainContent

libEnv :: ByteString
libEnv = $(embedFile "templates/QuickStartUp/lib/Env.hs") -- libEnv

libRoutes :: ByteString
libRoutes = $(embedFile "templates/QuickStartUp/lib/Routes.hs") -- libRoutes

libTinfoiltigerEnv :: ByteString
libTinfoiltigerEnv = $(embedFile "templates/QuickStartUp/lib/TinFoilTiger/Env.hs") -- libTinfoiltigerEnv

libTinfoiltigerKernel :: ByteString
libTinfoiltigerKernel = $(embedFile "templates/QuickStartUp/lib/TinFoilTiger/Kernel.hs") -- libTinfoiltigerKernel

libTinfoiltigerUtilsMonadmanager :: ByteString
libTinfoiltigerUtilsMonadmanager = $(embedFile "templates/QuickStartUp/lib/TinFoilTiger/Utils/MonadManager.hs") -- libTinfoiltigerUtilsMonadmanager

libTinfoiltigerUtilsRenderer :: ByteString
libTinfoiltigerUtilsRenderer = $(embedFile "templates/QuickStartUp/lib/TinFoilTiger/Utils/Renderer.hs") -- libTinfoiltigerUtilsRenderer

libTinfoiltigerUtilsRedis :: ByteString
libTinfoiltigerUtilsRedis = $(embedFile "templates/QuickStartUp/lib/TinFoilTiger/Utils/Redis.hs") -- libTinfoiltigerUtilsRedis

libTinfoiltigerRoutehandlersAuth :: ByteString
libTinfoiltigerRoutehandlersAuth = $(embedFile "templates/QuickStartUp/lib/TinFoilTiger/RouteHandlers/Auth.hs") -- libTinfoiltigerRoutehandlersAuth

templates :: [(FilePath, ByteString)]
templates =
  [ ("templates/QuickStartUp/Setup.hs", fileSetupExtHs),
    ("templates/QuickStartUp/package.yaml", filePackageExtYaml),
    ("templates/QuickStartUp/LICENSE", fileLicense),
    ("templates/QuickStartUp/.gitignore", extGitignore),
    ("templates/QuickStartUp/stack.yaml", fileStackExtYaml),
    ("templates/QuickStartUp/README.md", fileReadmeExtMd),
    ("templates/QuickStartUp/app/Main.hs", appMain),
    ("templates/QuickStartUp/test/Spec.hs", testSpec),
    ("templates/QuickStartUp/resources/signup.html", resourcesSignup),
    ("templates/QuickStartUp/resources/debugger.html", resourcesDebugger),
    ("templates/QuickStartUp/resources/main.css", resourcesMain),
    ("templates/QuickStartUp/resources/redis_insert.html", resourcesRedisInsert),
    ("templates/QuickStartUp/resources/about.html", resourcesAbout),
    ("templates/QuickStartUp/resources/login.html", resourcesLogin),
    ("templates/QuickStartUp/resources/landing.html", resourcesLanding),
    ("templates/QuickStartUp/resources/index.html", resourcesIndex),
    ("templates/QuickStartUp/resources/dashboard.html", resourcesDashboard),
    ("templates/QuickStartUp/resources/assets/styles.css", resourcesAssetsStyles),
    ("templates/QuickStartUp/resources/components/wrappers/base-template.html", resourcesComponentsWrappersBaseTemplate),
    ("templates/QuickStartUp/resources/components/embeds/main-content.html", resourcesComponentsEmbedsMainContent),
    ("templates/QuickStartUp/lib/Env.hs", libEnv),
    ("templates/QuickStartUp/lib/Routes.hs", libRoutes),
    ("templates/QuickStartUp/lib/TinFoilTiger/Env.hs", libTinfoiltigerEnv),
    ("templates/QuickStartUp/lib/TinFoilTiger/Kernel.hs", libTinfoiltigerKernel),
    ("templates/QuickStartUp/lib/TinFoilTiger/Utils/MonadManager.hs", libTinfoiltigerUtilsMonadmanager),
    ("templates/QuickStartUp/lib/TinFoilTiger/Utils/Renderer.hs", libTinfoiltigerUtilsRenderer),
    ("templates/QuickStartUp/lib/TinFoilTiger/Utils/Redis.hs", libTinfoiltigerUtilsRedis),
    ("templates/QuickStartUp/lib/TinFoilTiger/RouteHandlers/Auth.hs", libTinfoiltigerRoutehandlersAuth)
  ]
