{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Env
import qualified Routes
import qualified TinFoilTiger.Kernel as Kernel

main :: IO ()
main = Kernel.startApp Env.appName Env.domainName Env.publicDir Env.port Env.redisDatabaseNumber Env.redisDatabaseModals Routes.appRoutes
