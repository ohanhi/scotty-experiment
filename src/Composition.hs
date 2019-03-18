{-# LANGUAGE DeriveGeneric #-}

module Composition where

import           Data.Aeson   (FromJSON, ToJSON, Value)
import           GHC.Generics

data Composition = Composition
  { trainNumber     :: Int
  , journeySections :: [JourneySection]
  } deriving (Show, Generic)

instance FromJSON Composition

data JourneySection = JourneySection
  { wagons :: [Wagon]
  } deriving (Show, Generic)

instance FromJSON JourneySection

data Wagon = Wagon
  { location :: Int
  } deriving (Show, Generic)

instance FromJSON Wagon
