{-# LANGUAGE OverloadedStrings #-}

module Types.User where

import Data.Aeson
import Data.Text

data User = User
  { id :: Int
  , username :: Text
  , password :: Text
  , email :: Text
  } deriving (Show)

instance FromJSON User where
  parseJSON (Object v) = User <$>
    v .: "id" <*>
    v .: "username" <*>
    v .: "password" <*>
    v .: "email"

instance ToJSON User where
  toJSON (User id username password email) =
    object ["id" .= id
           ,"username" .= username
           ,"password" .= password
           ,"email" .= email
           ]
