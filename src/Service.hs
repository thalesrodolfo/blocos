{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Service where

import Types.User
import Data.Pool
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Database.PostgreSQL.Simple
import qualified Data.Text as T
import GHC.Generics


data DbConfig = DbConfig {
     dbName :: String,
     dbUser :: String,
     dbPassword :: String
     } deriving (Show, Generic)

newConn :: DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo
               { connectUser = dbUser conf
               , connectPassword = dbPassword conf
               , connectDatabase = dbName conf
               }

makeDbConfig :: C.Config -> IO (Maybe DbConfig)
makeDbConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  pwd <- C.lookup conf "database.password" :: IO (Maybe String)
  return $ DbConfig <$> name
                    <*> user
                    <*> pwd

findUsers :: Pool Connection -> IO [User]
findUsers pool = do
  res <- withResource pool $ \conn ->
    query_ conn "SELECT * FROM users ORDER BY id DESC" :: IO [(Int, T.Text, T.Text, T.Text)]
  return $ map (\(id, username, password, email) -> User id username password email) res

findUser :: Int -> IO (Maybe User)
findUser userId = undefined

