{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.ByteString (ByteString)
import Data.String (IsString (..))
import Data.Traversable
import Database.PostgreSQL.Simple
import System.IO

-- import Data.ByteString (ByteString)

hello :: IO Int
hello = do
  conn <- connectPostgreSQL pg
  [Only i] <- query_ conn "select 2 + 2"
  return i

yb :: ByteString
yb = "postgresql://yugabyte:yugabyte@localhost:5433/db"

pg :: ByteString
pg = "postgresql://postgres:password@localhost:5998/postgres"

grabEvent :: Connection -> IO (Maybe Int)
grabEvent conn = do
  q <- fromString <$> readFile "grab_event.sql"
  [Only eventId] <- query_ conn q
  pure eventId

createEvent :: Connection -> Int -> IO ()
createEvent conn id = do
  q <- fromString <$> readFile "create_event.sql"
  execute conn q (Only id)
  pure ()

markEventCompleted :: Connection -> Int -> IO ()
markEventCompleted conn id = do
  q <- fromString <$> readFile "complete_event.sql"
  execute conn q (Only id)
  pure ()

delay = 2000000

processEvent :: Connection -> IO ()
processEvent conn = do
  eventId <- grabEvent conn
  for eventId \e -> do
    putStrLn $ "grab " <> show e
    threadDelay delay
    markEventCompleted conn e
    putStrLn $ "done " <> show e
  pure ()

main :: IO ()
main = do
  conn <- connectPostgreSQL pg
  processEvent conn
