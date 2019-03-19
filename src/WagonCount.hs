{-# LANGUAGE DeriveGeneric #-}

module WagonCount where

import           Data.Aeson   (FromJSON, ToJSON, Value)
import           GHC.Generics

data WagonCountsResponse = WagonCountsResponse
  { wagonCounts :: [WagonCount]
  , date        :: String
  } deriving (Show, Generic)

instance ToJSON WagonCountsResponse

data WagonCount = WagonCount
  { trainNumber :: Int
  , wagonCount  :: Int
  } deriving (Show, Generic)

instance ToJSON WagonCount
