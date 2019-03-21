module Train where

data Train = Train
  { trainNumber          :: Int
  , lineId               :: String
  , runningCurrently     :: Bool
  , cancelled            :: Bool
  , currentStation       :: Maybe CurrentStation
  , homeStationDeparture :: TimetableRow
  , endStationArrival    :: TimetableRow
  , durationMinutes      :: Int
  , stopsBetween         :: Int
  , viaAirport           :: Bool
  }

data TrainRaw = TrainRaw
  { trainNumber      :: Int
  , lineId           :: String
  , trainCategory    :: String
  , timetableRows    :: [TimetableRow]
  , runningCurrently :: Bool
  , cancelled        :: Bool
  }

data TimetableRow = TimetableRow
  { scheduledTime       :: Posix
  , trainStopping       :: Bool
  , stationShortCode    :: String
  , stationUICCode      :: Int
  , track               :: Maybe String
  , rowType             :: RowType
  , actualTime          :: Maybe Posix
  , liveEstimateTime    :: Maybe Posix
  , differenceInMinutes :: Maybe Int
  }

data CurrentStation = CurrentStation
  { rowType             :: RowType
  , actualTime          :: Posix
  , differenceInMinutes :: Int
  , currentShortCode    :: String
  , stoppingType        :: CurrentStationStopping
  }

data CurrentStationStopping
  = NonStopping { prevStopShortCode :: String
                , nextStopShortCode :: String }
  | Stopping
