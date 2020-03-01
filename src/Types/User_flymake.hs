{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.User where

import Data.Aeson
import Data.Text
import GHC.Generics

data User = User
  { id :: Int
  , username :: Text
  , password :: Text
  , email :: Text
  } deriving (Show)

instance FromJSON User where
  parseJSON (Object v) = User <$>
    v .:? "id" .!=0 <*>
    v .: "username" <*>
    v .: "password" <*>
    v .: "email"

instance ToJSON User where
  toJSON (User id username _ email) =
    object ["id" .= id
           ,"username" .= username
           ,"email" .= email
           ]


data LoginUser = LoginUser
  { loginUsername :: Text
  , loginPassword :: Text
  } deriving (Show, Generic)

instance FromJSON LoginUser
instance ToJSON LoginUser
