{-# LANGUAGE DeriveGeneric #-}

module ErrorHandling
  ( handleEx
  , Except(..)
  ) where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.String        (fromString)
import           GHC.Generics
import           Network.HTTP.Types
import           Web.Scotty.Trans

-- Define a custom exception type.
data Except
  = Forbidden
  | NotFound String
  | StringEx String
  deriving (Show, Eq)

-- The type must be an instance of 'ScottyError'.
-- 'ScottyError' is essentially a combination of 'Error' and 'Show'.
instance ScottyError Except where
  stringError = StringEx
  showError = fromString . show

data ErrorMessage = ErrorMessage
  { status  :: Int
  , error   :: String
  , message :: String
  } deriving (Show, Generic)

instance ToJSON ErrorMessage

-- Handler for uncaught exceptions.
handleEx :: Monad m => Except -> ActionT Except m ()
handleEx except =
  case except of
    Forbidden -> do
      Web.Scotty.Trans.status status403
      json $
        ErrorMessage 403 "Forbidden" "You are not authorized for this action"
    NotFound i -> do
      Web.Scotty.Trans.status status404
      json $ ErrorMessage 404 "Not found" $ i
    StringEx s -> do
      Web.Scotty.Trans.status status500
      json $ ErrorMessage 500 "Internal error" s
