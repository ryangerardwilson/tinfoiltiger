{-# LANGUAGE OverloadedStrings #-}

module TinFoilTiger.Utils.Redis
  ( Connection, -- re-export the Connection type (an alias for Database.Redis.Connection)
    insertRecord,
    getRecord,
    updateRecord,
    deleteRecord,
    lookupUserIdByUsername,
    getUserFields,
    createUser,
    storeAuthToken,
    getAuthUid,
    getUserField, -- NEW helper to get a single field from a user's hash
  )
where

import Data.ByteString.Char8 (pack, unpack)
import qualified Database.Redis as R

-- | Alias for Redis connections.
type Connection = R.Connection

-- | Inserts a new record into a given model.
insertRecord :: Connection -> String -> [(String, String)] -> IO (Either R.Reply String)
insertRecord conn model fields = do
  let counterKey = pack $ model ++ ":next_id"
  idReply <- R.runRedis conn $ R.incr counterKey
  case idReply of
    Left err -> return (Left err)
    Right newId -> do
      let recordKey = pack $ model ++ ":" ++ show newId
          fieldPairs = map (\(f, v) -> (pack f, pack v)) fields
      hmsetReply <- R.runRedis conn $ R.hmset recordKey fieldPairs
      case hmsetReply of
        Left err -> return (Left err)
        Right _ -> return (Right (show newId))

-- | Retrieves a record (its field/value pairs) for a given model and record id.
getRecord :: Connection -> String -> String -> IO (Either R.Reply [(String, String)])
getRecord conn model recordId = do
  let recordKey = pack $ model ++ ":" ++ recordId
  reply <- R.runRedis conn $ R.hgetall recordKey
  case reply of
    Left err -> return (Left err)
    Right xs -> return (Right $ map (\(k, v) -> (unpack k, unpack v)) xs)

-- | Updates a record in a given model using provided field/value pairs.
updateRecord :: Connection -> String -> String -> [(String, String)] -> IO (Either R.Reply String)
updateRecord conn model recordId fields = do
  let recordKey = pack $ model ++ ":" ++ recordId
      fieldPairs = map (\(f, v) -> (pack f, pack v)) fields
  reply <- R.runRedis conn $ R.hmset recordKey fieldPairs
  case reply of
    Left err -> return (Left err)
    Right _ -> return (Right recordId)

-- | Deletes a record from a given model by record id.
deleteRecord :: Connection -> String -> String -> IO (Either R.Reply Integer)
deleteRecord conn model recordId = do
  let recordKey = pack $ model ++ ":" ++ recordId
  R.runRedis conn $ R.del [recordKey]

-- | Looks up a user id based on a username.
lookupUserIdByUsername :: Connection -> String -> IO (Either R.Reply (Maybe String))
lookupUserIdByUsername conn username = do
  let key = pack $ "username:" ++ username
  reply <- R.runRedis conn $ R.get key
  case reply of
    Left err -> return (Left err)
    Right maybeUidBS ->
      case maybeUidBS of
        Nothing -> return (Right Nothing)
        Just uidBS -> return (Right (Just (unpack uidBS)))

-- | Retrieves the user fields (for the "user" model) for a given user id.
getUserFields :: Connection -> String -> IO (Either R.Reply [(String, String)])
getUserFields conn userId = getRecord conn "user" userId

-- | Creates a new user record given a username and password.
createUser :: Connection -> String -> String -> IO (Either R.Reply String)
createUser conn username password = do
  idReply <- R.runRedis conn $ R.incr (pack "user:next_id")
  case idReply of
    Left err -> return (Left err)
    Right newId -> do
      let uid = show newId
          userKey = "user:" ++ uid
          userData = [("username", username), ("password", password)]
          rUserKey = pack userKey
          fieldPairs = map (\(f, v) -> (pack f, pack v)) userData
      hmsetReply <- R.runRedis conn $ R.hmset rUserKey fieldPairs
      case hmsetReply of
        Left err -> return (Left err)
        Right _ -> do
          _ <- R.runRedis conn $ R.set (pack $ "username:" ++ username) (pack uid)
          return (Right uid)

-- | Stores an auth token for a user with a specified TTL.
storeAuthToken :: Connection -> String -> String -> Int -> IO (Either R.Reply ())
storeAuthToken conn token uid ttl = do
  let key = pack $ "auth:" ++ token
      value = pack uid
      ttlInteger = fromIntegral ttl -- Convert Int to Integer
  reply <- R.runRedis conn $ R.setex key ttlInteger value
  case reply of
    Left err -> return (Left err)
    Right _ -> return (Right ())

-- | Retrieves the user id associated with an auth token.
getAuthUid :: Connection -> String -> IO (Either R.Reply (Maybe String))
getAuthUid conn token = do
  let key = pack $ "auth:" ++ token
  reply <- R.runRedis conn $ R.get key
  case reply of
    Left err -> return (Left err)
    Right Nothing -> return (Right Nothing)
    Right (Just uidBS) -> return (Right (Just (unpack uidBS)))

-- | Retrieves a single field from the "user" hash.
getUserField :: Connection -> String -> String -> IO (Either R.Reply (Maybe String))
getUserField conn uid field = do
  let key = pack $ "user:" ++ uid
      f = pack field
  reply <- R.runRedis conn $ R.hget key f
  case reply of
    Left err -> return (Left err)
    Right Nothing -> return (Right Nothing)
    Right (Just val) -> return (Right (Just (unpack val)))
