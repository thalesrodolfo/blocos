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
import GHC.Int

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

mkUser :: (Int, T.Text, T.Text, T.Text) -> User
mkUser (id, username, password, email) = User id username password email

findUsers :: Pool Connection -> IO [User]
findUsers pool = do
  res <- withResource pool $ \conn ->
    query_ conn "SELECT * FROM users ORDER BY id DESC" :: IO [(Int, T.Text, T.Text, T.Text)]
  return $ map mkUser res

findUser :: Int64 -> Pool Connection -> IO (Maybe User)
findUser userId pool = do
  res <- withResource pool $ \conn ->
    query conn "SELECT * FROM users WHERE id = ?" (Only userId) :: IO [(Int, T.Text, T.Text, T.Text)]
  case length res of
    0 -> return Nothing
    _ -> return . Just $ mkUser . head $ res

createUser :: User -> Pool Connection -> IO (Maybe User)
createUser (User _ username password email) pool = do
  res <- withResource pool $ \conn ->
    execute conn "INSERT INTO users (username, password, email) values (?,?,?)" [username, password, email]
  findUser res pool

