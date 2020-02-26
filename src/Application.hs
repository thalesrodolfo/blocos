{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Application (runApp, runApp') where

import qualified Web.Scotty as S
import Service


import Data.Pool
import Database.PostgreSQL.Simple (close, Connection)

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import           Network.Wai (Application)

import Control.Monad.IO.Class
import Network.HTTP.Types

app' :: Pool Connection -> S.ScottyM ()
app' pool = do
  S.get "/api/v1/usuarios" $ do
    users <- liftIO $ findUsers pool
    S.status status200
    S.json users

  S.get "/api/v1/usuarios/:id" $ do
    id <- S.param "id"
    user <- liftIO $ findUser id pool
    case user of
      Nothing -> S.status status400
      Just _ -> S.status status200 >> S.json user

  S.post "/api/v1/usuarios" $ do
    userJson <- S.jsonData
    userResponse <- liftIO $ createUser userJson pool
    case userResponse of
      Nothing -> S.status status400
      Just _ -> S.status status200 >> S.json userResponse

  S.post "/api/v1/loginUser" $ do
    username <- S.param "username"
    password <- S.param "password"
    loggedUser <- liftIO $ loginUser username password pool
    case loggedUser of
      Nothing -> S.status status400
      Just _ -> S.status status200 >> S.json loggedUser

runApp' :: Pool Connection -> IO Application
runApp' pool = S.scottyApp (app' pool)

runApp :: IO ()
runApp = do
  loadedConf <- C.load [C.Required "application.conf"]
  dbConf <- makeDbConfig loadedConf
  case dbConf of
    Nothing -> putStrLn "Erro: Não foi possível obter as informações de conexão com o DB"
    Just conf -> do
      pool <- createPool (newConn conf) close 1 40 10
      S.scotty 8080 (app' pool)
