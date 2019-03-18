module Playground where

import           Control.Monad
import qualified Data.Time
import qualified System.IO

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

printStack :: IO ()
printStack = System.IO.readFile "stack.yaml" >>= putStrLn

printTime :: IO ()
printTime = Data.Time.getCurrentTime >>= (putStrLn . show)

askMessage :: IO String
askMessage = do
  putStrLn "Give message plox:"
  message <- System.IO.getLine
  return message

askCount :: IO Int
askCount = do
  putStrLn "How many times:"
  n <- System.IO.getLine
  readIO n

main :: IO ()
main = do
  printTime
  msg <- askMessage
  count <- askCount
  mapM (\n -> putStrLn (show n ++ ": " ++ msg)) [1 .. count]
  return ()
