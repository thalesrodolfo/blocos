{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Example (runApp, app) where

import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import qualified Web.Scotty as S
import Service


import Data.Pool
import Database.PostgreSQL.Simple (close)

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

import Control.Monad.IO.Class

app' :: S.ScottyM ()
app' = do
  S.get "/" $ do
    S.text "hello"

  S.get "/some-json" $ do
    S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = do
  loadedConf <- C.load [C.Required "application.conf"]
  dbConf <- makeDbConfig loadedConf
  case dbConf of
    Nothing -> putStrLn "Erro: Não foi possível obter as informações de conexão com o DB"
    Just conf -> do
      pool <- createPool (newConn conf) close 1 40 10
      S.scotty 8080 $ do
        S.get "/api/v1/usuarios" $ do
          users <- liftIO $ findUsers pool
          S.json users
