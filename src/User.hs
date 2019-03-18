{-# LANGUAGE DeriveGeneric #-}

module User where

import           Data.Aeson   (FromJSON, ToJSON, Value)
import           GHC.Generics

data User = User
  { userId   :: Int
  , userName :: String
  } deriving (Show, Generic)

instance FromJSON User

instance ToJSON User

bob :: User
bob = User {userId = 1, userName = "bob"}

jenny :: User
jenny = User {userId = 2, userName = "jenny"}

allUsers :: [User]
allUsers = [bob, jenny]
