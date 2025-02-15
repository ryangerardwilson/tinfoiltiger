module Env (appName, domainName, publicDir, port, redisDatabaseNumber, redisDatabaseModals) where

appName :: String
appName = "TinFoilTigerDocs"

domainName :: String
domainName = "tinfoiltiger.com"

publicDir :: String
publicDir = "resources"

port :: Int
port = 8081

redisDatabaseNumber :: Integer
redisDatabaseNumber = 0

redisDatabaseModals :: [String]
redisDatabaseModals = ["user", "posts"]
