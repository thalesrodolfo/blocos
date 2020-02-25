{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Network.HTTP.Types.Header
import           Data.Aeson (Value(..), object, (.=))

import           Application (runApp')
import qualified Web.Scotty                 as Scotty

import Data.Pool
import Database.PostgreSQL.Simple (close, Connection)

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

import Service

main :: IO ()
main = do
  loadedConf <- C.load [C.Required "application.conf"]
  dbConf <- makeDbConfig loadedConf
  case dbConf of
    Nothing -> putStrLn "Erro: Não foi possível obter as informações de conexão com o DB"
    Just conf -> do
      pool <- createPool (newConn conf) close 1 40 10
      hspec $ with (runApp' pool) $ do
        describe "GET /api/v1/usuarios" $ do
          it "responds with some JSON" $ do
           get "/api/v1/usuarios" `shouldRespondWith` expectedJsonResponse

expectedJsonResponse = 
  let
    ResponseMatcher status headers body =
      [json|[{"email":"teste","username":"teste","password":"teste","id":1}]|]
  in
    ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body
