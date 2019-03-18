{-# LANGUAGE DeriveGeneric #-}

module WagonCount where

import           Data.Aeson   (FromJSON, ToJSON, Value)
import           GHC.Generics

data WagonCount = WagonCount
  { trainNumber :: Int
  , wagonCount  :: Int
  } deriving (Show, Generic)

instance ToJSON WagonCount
