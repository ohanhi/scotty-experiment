{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Data.Aeson                           (FromJSON, ToJSON, Value)
import qualified Data.Text                            as Text
import qualified Data.Time
import qualified Data.Time.Format                     as TF
import           Flow
import           GHC.Generics
import           Network.HTTP.Req
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified System.Environment                   as Env
import           Web.Scotty.Trans

import           Composition
import           ErrorHandling
import           User
import           WagonCount

main :: IO ()
main = do
  port <- Env.lookupEnv "PORT" |> fmap (maybe 3000 read)
  scottyT port id <| do
    middleware RequestLogger.logStdoutDev
    defaultHandler handleEx
    router

router :: ScottyT Except IO ()
router = do
  get "/" <| text "OK"
  get "/wagon-counts" <| wagonCountsAction
  get "/users" <| json allUsers
  get "/users/:id" <| do
    id <- param "id"
    case filter (\u -> userId u == id) allUsers of
      user:a -> json user
      a      -> raise (NotFound ("No user with id " ++ show id))
  notFound <| raise (NotFound "Unknown route")

--- WAGON COUNTS
wagonCountsAction :: (ScottyError e, MonadIO m) => ActionT e m ()
wagonCountsAction = liftAndCatchIO wagonCountsRequest >>= json

wagonCountsRequest :: IO WagonCountsResponse
wagonCountsRequest =
  runReq defaultHttpConfig <| do
    time <- liftIO Data.Time.getCurrentTime
    let url = compositionsUrl time
    response <- req GET url NoReqBody jsonResponse mempty
    let body :: [Composition] = responseBody response
    let wagonCounts = map compositionToWagonCount body
    case body of
      first:rest ->
        return <|
        WagonCountsResponse
          {date = departureDate first, wagonCounts = wagonCounts}
      _ -> fail "Malformed response"

compositionsUrl :: Data.Time.UTCTime -> Url 'Https
compositionsUrl time =
  time |> TF.formatTime TF.defaultTimeLocale "%F" |> Text.pack |>
  (https "rata.digitraffic.fi" /: "api" /: "v1" /: "compositions" /:)

compositionToWagonCount :: Composition -> WagonCount
compositionToWagonCount composition =
  WagonCount
    { trainNumber = Composition.trainNumber composition
    , wagonCount =
        journeySections composition |> map (wagons .> length) |> minimum
    }
-- TRAINS
-- TODO
