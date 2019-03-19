{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Data.Aeson                           (FromJSON, ToJSON, Value)
import           Data.Function                        ((&))
import           Data.Maybe                           as Maybe
import qualified Data.Text                            as Text
import qualified Data.Time
import qualified Data.Time.Format                     as TF
import           Flow
import           GHC.Generics
import           Network.HTTP.Req
import           Network.Wai.Middleware.RequestLogger as RequestLogger
import           Web.Scotty.Trans

import           Composition
import           ErrorHandling
import           User
import           WagonCount

wagonCountsRequest :: IO WagonCountsResponse
wagonCountsRequest =
  let format = TF.formatTime TF.defaultTimeLocale "%F" .> Text.pack
   in runReq defaultHttpConfig <| do
        response <-
          liftIO Data.Time.getCurrentTime >>= return . format >>=
          return .
          (https "rata.digitraffic.fi" /: "api" /: "v1" /: "compositions" /:) >>=
          (\url ->
             req
               GET
               url
               NoReqBody
               jsonResponse -- specify how to interpret response
               mempty -- query params, headers, explicit port number, etc.
           )
        let body :: [Composition] = responseBody response
            wagonCounts = map compositionToWagonCount body
         in case body of
              first:rest ->
                return <|
                WagonCountsResponse
                  {date = departureDate first, wagonCounts = wagonCounts}
              _ -> fail "Malformed response"

compositionToWagonCount :: Composition -> WagonCount
compositionToWagonCount composition =
  WagonCount
    { trainNumber = Composition.trainNumber composition
    , wagonCount =
        journeySections composition |> map (wagons .> length) |> minimum
    }

wagonCountsAction :: (ScottyError e, MonadIO m) => ActionT e m ()
wagonCountsAction = liftAndCatchIO wagonCountsRequest >>= json

main :: IO ()
main = do
  putStrLn "Starting server..."
  scottyT 3000 id <| do
    middleware RequestLogger.logStdoutDev
    defaultHandler handleEx
    router

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
